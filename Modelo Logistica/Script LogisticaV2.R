######################################
# Simulation Oil optimization SCRIPT #
######################################

#### 00. libraries and delete env ####
rm(list = ls())
cat("\f")
require(xlsx)
require(gdata)
require(igraph)
set.seed(10)

#### 01. read files with tareas & parameters ####
#parameters

years2eval <- 1
days1y <- 365

total_days <- years2eval*days1y
total_days_set <- 1:total_days
labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
labor_days_id <-  (1:total_days)[labor_days_bool]

#setwd("C:/Users/mferrari/Desktop/Mantenimiento Xavier")
tareas_log <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 2, header = TRUE)
tareas_log <- tareas_log[,c(1:2,4:7,9:11)]
colnames(tareas_log) <- c('grupo','actividad','prioridad','tarea','unidad','demanda','tiempo_car-des',
                          'comienzo','fin')

#calulate tareas log columns
tareas_log$duracion <- tareas_log$fin -tareas_log$comienzo +1
tareas_log$demanda_diaria <- tareas_log$demanda/tareas_log$duracion 

                          
recursos <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 4, header = TRUE)
rownames(recursos) = recursos$Tipo.Unidad

frecuencia <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 1, header = TRUE)

purgas <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 3, header = TRUE)

frecuencia_purgas <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 5, header = TRUE)
row.names(frecuencia_purgas) <- frecuencia_purgas$Categoria

rutas = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 7, header = TRUE)
puntos = read.xls ("Tareas Mantenimiento vMArtin.xlsx", sheet = 8, header = TRUE)

#clean up
rutas <- rutas[,c(3:6,2)]

#keeps point Activos and with Category
colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')

#attention with the S? instead of Si depending on how to read the text
puntos <- puntos[puntos$Activo=='Si',c(2,3,4,6)]

#all point in at least one route
punt <- c(as.character(rutas$origen),as.character(rutas$destino))
not.present <- !(puntos$Identif %in% punt)
#show those not present.
puntos$Identif[not.present]

#keep only those points present in at least one route
puntos <- puntos[!not.present,]

#rownames puntos
rownames(puntos) <- puntos$Identif

#### 02. explore graph ####
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)
comp <- components(g)

#remove not connected points in the graph
puntos.not_connect <- names(comp$membership[comp$membership!=1])
rutas <- rutas[!(rutas$origen %in% puntos.not_connect),]
#rutas[!(rutas$destino %in% puntos.not_connect),]

#declare again the graph
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)

#measure the distance between Base and each node
# calculate all shortest paths from base
sp <- shortest.paths(g,as.numeric(V(g)["Base"]),weights = rutas$tiempo)

#convert sp as days. round trip
sp <- sp/60/60*2

#just in case remove not connected
puntos <- puntos[!(rownames(puntos) %in% puntos.not_connect),]

#initialize list of pozos, baterias, satelites, plantas
pozos <- rownames(puntos[puntos$Tipo=='Pozo',])
baterias <-rownames(puntos[puntos$Tipo=='Bateria',])
satelites <-rownames(puntos[puntos$Tipo=='Satelite',])
plantas_pet <- c('Planta Central', 'Planta La Petiza')
plantas_agua <- c('Planta Sur', 'Planta Norte','Planta Oeste')
plantas_gas <- c('Planta Gas')
subestacion <- c('Subestacion')
gasoducto <- c('Gasoducto')
motogenerador <- c('Motogenerador')

# declare a matrix of sp
sp.m <- matrix (0, ncol=nrow(puntos)+1,nrow=nrow(puntos)+1)
rownames(sp.m) <- c('Base',as.character(puntos$Identif))
colnames(sp.m) <- c('Base',as.character(puntos$Identif))

# calculate sp matrix (can be improved)

for (i in rownames(sp.m)) {
  mess1 <- paste(c( "calculating shortest path for node: ",i),collapse='')
  print(mess1)
  id_i <- as.numeric(V(g)[i])
  for (j in colnames(sp.m)) {
    id_j <- as.numeric(V(g)[j])
    if (sp.m[i,j] ==0) {
      s <- shortest.paths(g, id_i, id_j, weights = rutas$tiempo)
      sp.m[i,j] <- s
      sp.m[j,i] <- s
    }
  }
}
print('')
print('shortest path calculation completed')

#### 03. initialize the calendar of logistics needs everithing but PURGA ####

print('')
print('initializing calendar of tasks (without purges)')
stack_log <- data.frame(act=character(), prio = numeric(), act_id=numeric(), 
                        tarea=character(), unidad=character(), demanda= numeric(), 
                        day = numeric(), location=character(), tiempo = numeric(),
                        demanda_open = numeric(),
                        complete_day = numeric())


add_task_stack <- function (type,day, type_number) {
  
  #select a location randomly
  loc <- sample(puntos$Identif,1)
  transport_time_h <- sp[loc]

  mat <- tareas_log[tareas_log$grupo==type,]
  mat$day <-  day + mat$comienzo -1
  mat$tiempo_total <- transport_time_h + mat$`tiempo_car-des`
  mat$type_number <- type_number
  mat$loc <- loc
  mat$comp_dia <- NA
  mat$demanda_open <- mat$demanda

  # if WO select only one row from the matrix and re-assing
  if (type=='WO')  {
    subtype = sample(c('WO Primaria','WO Secundaria Inyector',
                       'WO Secundaria Productores','WO Etiles',
                       'Terminacion','Abandono','Pulling Pesado'),1)
    rows2keep <- mat$actividad==subtype
    mat <- mat[rows2keep,]
  }

  stack_log <<- rbind(stack_log, 
              mat[,c( 'grupo','prioridad','type_number','tarea',
                         'unidad', 'demanda', 'day', 'loc','tiempo_total', 'demanda_open','comp_dia')])
  
}


for (type_task_id in 1:nrow(frecuencia)) {
  cant_total <- frecuencia$Cantidad.Anual[type_task_id]*years2eval
  
  days_ini_task <- round(seq(1,total_days,total_days/cant_total)[1:cant_total],0)
  days_ini_task <- days_ini_task[!is.na(days_ini_task)]

  for (type_task_number in 1:cant_total) {
    add_task_stack(frecuencia$Grupo[type_task_id],days_ini_task[type_task_number],type_task_number)
  }
}

#### 04. functions to optimiza the purgas routes ####

initialize_pozos <- function(category) {
  bag<- as.character(purgas$Inst..Asociada[purgas$Categoria==category])
  #jor <- 1
  #num_vis <- categorias[category,'visitas']
  
  for (i in 1:length(bag)) {
    c1 <- bag[i]
    sol <<- append (sol, list(c1))
  }
}

tiempos.detail <- function(route) {
  #Chequed!!
  n.points <- length(route)
  times <- rep(0,(n.points+1))
  #print(route)
  #print(route[1])
  times[1] <- sp.m ['Base',route[1]]
  times[n.points+1] <- sp.m [route[n.points],'Base']
  if (n.points >1) {
    for (i in 2:n.points) {
      times[i] <-  sp.m [route[i-1],route[i]]
    }
  } 
  return(times/60/60)
}

tiempos.total <- function(route) {
  #chequed!
  n.points <- length(route)
  #print(c('t.t',as.character(n.points)))
  
  t.transport <- sum(tiempos.detail(route))
  t.pozo <- labor*n.points
  return(c(t.transport,t.pozo,n.points,max(0,jor_time-t.transport-t.pozo)))
}

#create a list of times resulting of removing each point
tiempos.saved <- function(route) {
  #checked
  n.points <- length(route)
  times.s <- rep(0,n.points)
  times <- tiempos.detail(route)
  if (n.points==1){
    times.s[1] <- sum(times)
  } else {
    #first element
    times.s[1] <- times [1] + times[2] - sp.m ['Base', route[2]]/60/60
    #last element
    times.s[n.points] <- times [n.points] + times[n.points+1] - sp.m [route[n.points-1],'Base']/60/60
    if (n.points>2) {
      for (i in 2:(n.points-1)) {
        times.s[i] <- times [i] + times[i +1] - sp.m [route[i-1],route[i+1]]/60/60
      }
    }
  }
  times.s [times.s<0] <- 0
  return (times.s)
}

where2add <- function(route,node, saving) {
  #print('where2add')
  t2 <- tiempos.2add(route, node)
  
  if (sum(saving > t2)==0) {
    #print('cost greather than saving')
    return (0)
  } else {
    td <- tiempos.total(route)[4]
    pos <- sample(which(saving > t2),1)
    if (td < t2[pos] + labor) {
      #print('time increases the 240 min')
      return (0)
    } else {
      #print('to be added')
      return (pos)
    }
  }
}

add.node <- function  (route,node, position) {
  route <- append (route, node, position-1)
  return (route)
}

remove.node <- function (route,position) {
  route <- route [-position]
  return (route)
}

tiempos.2add <- function(route,node) {
  n.points <- length(route)  
  #print(c('t.2a',n.points))
  times.2 <- rep(0,n.points+1)
  times.d <- tiempos.detail(route)
  times.2[1] <- sp.m['Base',node] + sp.m[node, route[1]]
  times.2[n.points+1] <- sp.m[route[n.points],node] + sp.m[node, 'Base'] 
  if (n.points>1) {
    for (i in 2:n.points) {
      times.2[i] <- sp.m[route[i-1],node] + sp.m[node,route[i]]
    }
  }
  return(times.2/60/60-times.d)
}

#### 05. modeling purgas ####
print('modeling purges')
iterations <- 500
jor_time <- 4
labor <- 0.5

for (cat_pozo in frecuencia_purgas$Categoria) {
  
print('')
print(paste(c('routing the purges in instalation category: ',cat_pozo),collapse=''))

sol <- list()
initialize_pozos(cat_pozo)

t.m <- do.call(rbind, lapply(sol,tiempos.total))

colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')

k<- 1
while (k <= iterations) {
  #initial diagnisys
  turnos <- nrow(t.m)
  tiempo.ocioso <- sum(t.m[,'av.time'])
  #print(c('k:',k,'-turnos:',turnos,'-tiempo.ocioso:',tiempo.ocioso))
  
  #select the tour to extract 
  tour2extract <- sample(1:nrow(t.m),1, prob=(t.m[,'av.time']+1))
  
  #declare route to work
  route2e <- sol[[tour2extract]]
  
  #calculate the times saved
  t.s <- tiempos.saved(route2e)
  
  #select the point to extract
  if (sum(t.s<=0)==length(t.s)) {  # the cylce does not offer any node worth to extract
    t.s[1]<-1 }
  pos2e <- sample(1:length(route2e),1, prob = t.s)
  node <- route2e[pos2e]
  
  #calculate savings
  saving <- t.s[pos2e]
  
  #select tour to add (ensure they are different)
  tour2add <- sample(1:nrow(t.m), 1,prob=(t.m[,'av.time']+1))
  while(tour2add==tour2extract) {
    tour2add <- sample(1:nrow(t.m), 1,prob=(t.m[,'av.time']+1))
  }
  
  route2a <- sol[[tour2add]]
  pos2a  <- where2add(route2a,node,saving)
  
  if (pos2a>0) {
    #add the node in tour2a
    sol[[tour2add]] <- add.node(sol[[tour2add]],node, pos2a)
    
    #update times
    #recalculate the t.m add
    t.m[tour2add,] <- tiempos.total(sol[[tour2add]])
    
    #remove node from tour2extract
    sol[[tour2extract]] <- remove.node(sol[[tour2extract]],pos2e)
    
    #what happens whan the sol tourtoextract is empty.
    if (length(sol[[tour2extract]])==0){
      #print('removing the last node')
      #remove prom sol and from t.m
      sol[[tour2extract]] = NULL
      t.m <- t.m[-tour2extract,]
      
      #convert t.m into a matrix if it is a vector
      if(is.null(nrow(t.m))) {
        t.m <- matrix(t.m,nrow=1)
        colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')
        break 
      }
      
    } else { #tour2extract is not empty
      #recalculate tm in tour2extract
      t.m[tour2extract,] <- tiempos.total(sol[[tour2extract]])
    } 
  } else {     #continues all cases where pos>0  
  }
  k <- k+1
}

## add task to stack_tot
stack_log_temp = NULL
for (day_iteration in seq(1,total_days,frecuencia_purgas[cat_pozo,'dias'])) {
rows2add <- cbind.data.frame(grupo=paste(c('Purga',cat_pozo),collapse='_'),
                             prioridad=frecuencia_purgas[cat_pozo,'prioridad'],
                             type_number=(1:nrow(t.m)),
                             tarea='visita purga',
                             unidad='Camion de Vacio',
                             demanda=0.1,
                             day=day_iteration,
                             loc='varias',
                             tiempo_total=t.m[,'t.transport']+t.m[,'t.pozo'],
                             demanda_open=0.1,
                             comp_dia=NA)

stack_log_temp <- rbind.data.frame(stack_log_temp, rows2add)

} #

print('')
print(paste(c('instalation category ',cat_pozo,' can be visited with ', nrow(t.m),
              ' cycles of 4 hours each.'),collapse=''))

#add stack_log_temp to stack log

print('')
print(paste(c('adding ',nrow(stack_log_temp),' to the stack of needs.'),collapse=''))

stack_log <- rbind.data.frame(stack_log, stack_log_temp)

} #end loop frecuencia_purgas$Categoria

#order stack_log by prioridad and day
stack_log <- stack_log[order(stack_log$prioridad,stack_log$day),]

stack_log <- stack_log[!is.na(stack_log$day),]

#### 06. assignment function ####
assign <- function (id_task,id_vehicle,ini_eq_free) {
  task <- stack_log[id_task,]
  vehicle <- recursos_record[id_vehicle,]
  
  if(ini_eq_free) {
    ini <- vehicle$free 
  } else {
    ini <- task$day
  }
  
  day_end <- ini + task$tiempo_total/24
  
  #update recrusos_record
  recursos_record [id_vehicle,'free'] <<- day_end
  
  #update stack_log
  if(vehicle$capacity > task$demanda_open) {
    #complete task
    stack_log[id_task,'demanda_open']<<- 0
    stack_log[id_task,'comp_dia'] <<-day_end
    
  } else {
    #incomplete task
    stack_log[id_task,'demanda_open']<<-  task$demanda_open-vehicle$capacity
  }
  
  #update assign_log
  row <- cbind.data.frame(as.character(task$grupo),
                          as.character(task$prioridad),
                          as.character(task$type_number),
                          as.character(task$tarea),
                          as.character(task$unidad), 
                          vehicle$id,ini, day_end)
  
  if (is.null(assign_log )) {
    assign_log <<- row
  } else {
    assign_log <<- rbind.data.frame(assign_log,row)
  }
}

#### 07. loop by type of vehicle ####
ids2model <- recursos$Turno=='24LD'&recursos$Modelo=='Si'
for (type in as.character(recursos$Tipo.Unidad[ids2model])) {
#type <- as.character(recursos$Tipo.Unidad[ids2model])[1]
  
print('')
print(paste(c('assigning the vehicle type: ', type ),collapse=''))

#### 08. initialize resources_record ####

#for now just 24LD transport
total_vehic <- 1

#### loop by quantity  
continue <- TRUE 

while (continue) {

print(paste(c('trying to assign with: ', total_vehic ,' vehicle/s'),collapse=''))  
  
#total_vehic <- sum(recursos$Cantidad[ids2model])
recursos_record = data.frame(tipo=character(total_vehic),
                             id=numeric(total_vehic),
                             capacity = numeric(total_vehic),
                             free=numeric(total_vehic),
                             stringsAsFactors = FALSE)
row <- 1

  for (num_vehiculo in 1:total_vehic){
    
    recursos_record[row,] =  c(tipo=type, id=row,capacity= recursos[type,'Capacidad'],free=1)
    row <- row +1
  }


recursos_record$id <- as.numeric(recursos_record$id)
recursos_record$free <- as.numeric(recursos_record$free)
recursos_record$capacity <- as.numeric (recursos_record$capacity)

#### 09. assignment resources-task ####
assign_log <- NULL
end <- FALSE
i <- 1
while(!end) {
  #pick up the vehicle that frees before
  id_vehicle <- which.min(recursos_record$free)
  vehicle_free <- min(recursos_record$free)
  vehicle_type <- recursos_record[id_vehicle,'tipo']
  
  past_tasks_ids <- stack_log$day<=vehicle_free
  same_type_tasks_ids <- grepl(vehicle_type, stack_log$unidad)
  completed_tasks_ids <- !is.na(stack_log$comp_dia)
  
  #free > totaldays +1 -> END (no more vehicles)
  if (vehicle_free > total_days + 1) { 
    end <- TRUE
    #print('no more vehicles')
  }
  
  #is there an open task in the past (free<day)?
  past_tasks <- stack_log[past_tasks_ids & same_type_tasks_ids&!completed_tasks_ids,]
  
  if (nrow(past_tasks)>0) {
    id_task <- rownames(past_tasks[1,])
    assign(id_task,id_vehicle,TRUE)
  } else {
    next_tasks <- stack_log[!past_tasks_ids & same_type_tasks_ids&!completed_tasks_ids,]
    
    if (nrow(next_tasks)>0) {
      id_task <- rownames(next_tasks[1,])
      assign(id_task,id_vehicle,FALSE)
    } else {
      #end
      recursos_record[id_vehicle,'free'] <- total_days +2
      #print('no more task')
    }
  }
  i <- i +1
 
}
#print(i)
assign_log <- data.frame(assign_log, row.names = NULL)
colnames(assign_log) <- c('grupo','prioridad','type_number','tarea','unidad','id','ini','end')

convert_to_numeric<-c("prioridad","type_number","id","ini","end")

for (col in convert_to_numeric) {
  assign_log[,col] <- as.numeric(assign_log[,col] ) 
}

#### 10. sumarizing data ####

stack_log$dif <- stack_log$comp_dia - stack_log$day

#percentaje not complete
not_completed_ids <- is.na(stack_log$dif)

perc_not_comp <- sum(tapply(not_completed_ids[same_type_tasks_ids],stack_log$prioridad[same_type_tasks_ids],mean))

if (perc_not_comp < 0.01) {
  continue <- FALSE
  print('successfull assignment of tasks')
  print(paste(c('vehicle: ', type, ', minimum quantity to complete the tasks: ',total_vehic ), collapse = ""))

  print('proportion not completed by priority:')
  print(tapply(not_completed_ids[same_type_tasks_ids],stack_log$prioridad[same_type_tasks_ids],mean))
  
  #print('proportion not completed by vehicle:')
  #print(tapply(not_completed_ids[same_type_tasks_ids],stack_log$unidad[same_type_tasks_ids],mean))
  
  print('average days to complete')
  print(tapply(stack_log$dif[!not_completed_ids&same_type_tasks_ids],
               stack_log$prioridad[!not_completed_ids&same_type_tasks_ids],mean))
}

if ( continue) {
  total_vehic <- total_vehic +1 
}
} #end loop total vehicle

} #end loop type vehicle

#### 11. export the stack_log ####
file_name <- 'assigment result.xlsx'
write.xlsx(stack_log, file=file_name)

#### 12. to-dos ####
#complete the model with Potable
