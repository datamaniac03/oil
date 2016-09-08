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
tareas_log <- tareas_log[,c(1,4:7,9:11)]
colnames(tareas_log) <- c('grupo','prioridad','tarea','unidad','demanda','tiempo_car-des',
                          'comienzo','fin')

#calulate tareas log columns
tareas_log$duracion <- tareas_log$fin -tareas_log$comienzo +1
tareas_log$demanda_diaria <- tareas_log$demanda/tareas_log$duracion 

                          
recursos <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 4, header = TRUE)
rownames(recursos) = recursos$Tipo.Unidad

frecuencia <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 1, header = TRUE)

purgas <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 3, header = TRUE)

frecuencia_purgas <- read.xls ("Modelo Logística vDraft(II).xlsx", sheet = 5, header = TRUE)

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
rutas[!(rutas$destino %in% puntos.not_connect),]

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

#### 03. initialize the calendar of logistics needs everithing but PURGA ####

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

#order stack_log by prioridad and day
stack_log <- stack_log[order(stack_log$prioridad,stack_log$day),]

stack_log <- stack_log[!is.na(stack_log$day),]

#### 04. initialize resources_record ####

#for now just 24LD transport
ids2model <- recursos$Turno=='24LD'&recursos$Modelo=='Si'
total_vehic <- sum(recursos$Cantidad[ids2model])
recursos_record = data.frame(tipo=character(total_vehic),
                             id=numeric(total_vehic),
                             capacity = numeric(total_vehic),
                             free=numeric(total_vehic),
                             stringsAsFactors = FALSE)
row <- 1
for (type in as.character(recursos$Tipo.Unidad[ids2model])) {
  for (num_vehiculo in 1:recursos[type,'Cantidad']){
    
    recursos_record[row,] =  c(tipo=type, id=row,capacity= recursos[type,'Capacidad'],free=1)
    row <- row +1
  }
}

recursos_record$id <- as.numeric(recursos_record$id)
recursos_record$free <- as.numeric(recursos_record$free)
recursos_record$capacity <- as.numeric (recursos_record$capacity)

#### 05. assignment resources-task ####

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
    print('no more vehicles')
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
      print('no more task')
    }
  }
  i <- i +1
  print(i)
}

assign_log <- data.frame(assign_log, row.names = NULL)
colnames(assign_log) <- c('grupo','prioridad','type_number','tarea','unidad','id','ini','end')

convert_to_numeric<-c("prioridad","type_number","id","ini","end")

for (col in convert_to_numeric) {
  assign_log[,col] <- as.numeric(assign_log[,col] ) 
}

#### 06. sumarizing data ####

stack_log$dif <- stack_log$comp_dia - stack_log$day

#percentaje not complete
not_completed_ids <- is.na(stack_log$dif)
print('proportion not completed by priority:')
tapply(not_completed_ids,stack_log$prioridad,mean)

print('average days to complete')
tapply(stack_log$dif[!not_completed_ids],stack_log$prioridad[!not_completed_ids],mean)

#### 07. to-dos ####
#complete the model with Potable
#separate model to route the PURGAS