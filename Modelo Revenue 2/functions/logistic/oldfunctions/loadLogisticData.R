#### 00. Libraries and delete env ####
require(xlsx)
require(gdata)
require(igraph)
#set.seed(10)

setGlobalParameters <- function(years2eval = 1, days1y = 365){
  days1y <- days1y
  total_days <- years2eval*days1y
  total_days_set <- 1:total_days
  labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
  labor_days_id <-  (1:total_days)[labor_days_bool]  
  
  list(years2eval = years2eval, 
       days1y = days1y,
       total_days = total_days,
       total_days_set = total_days_set,
       labor_days_bool = labor_days_bool,
       labor_days_id =  labor_days_id
       )
}



readLogisticsFiles <- function(logisticDataPath, maintenanceDataPath,
                               globalParameters){
  #### 01. Read files with tareas & parameters ####
  
  #Parameters
  years2eval <- globalParameters$years2eval
  days1y <- globalParameters$days1y
  
  total_days <- globalParameters$total_days
  total_days_set <- globalParameters$total_days_set 
  labor_days_bool <- globalParameters$labor_days_bool
  labor_days_id <- globalParameters$labor_days_id
  
  tareas_log <- read.xls (logisticDataPath, sheet = 2, header = TRUE) #ok
  
  #tareas_log <- tareas_log[,c(1:2,4:5,10:14,16:18, 6:9)]
  tareas_log <- tareas_log[,c(4,5,10,1,19,20,9,8,11,7,13,14,15,16,17,18)]
  colnames(tareas_log) <- c('grupo','actividad','prioridad','tarea','turno','weekdays',
                            'fluido','unidad','demanda','tiempo_car-des','comienzo','fin',
                            'horaInicioMin', 'horaInicioMax','tolerancia','respuesta')
  
  #Calulate tareas log columns
  tareas_log$duracion <- tareas_log$fin -tareas_log$comienzo + 1

  tareas_log$demanda_diaria <- tareas_log$demanda/tareas_log$duracion 

  recursos <- read.xls (logisticDataPath, sheet = 4, header = TRUE) #ok
  rownames(recursos) = recursos$Tipo.Unidad
  
  frecuencia <- read.xls (logisticDataPath, sheet = 3, header = TRUE) #ok
  purgas <- read.xls (logisticDataPath, sheet = 5, header = TRUE) #ok
  
  frecuencia_purgas <- read.xls (logisticDataPath, sheet = 6, header = TRUE)
  row.names(frecuencia_purgas) <- frecuencia_purgas$Categoria
  
  rutas = read.xls (#maintenanceDataPath
                    'data/Rutas v01.xlsx', sheet = 1, header = TRUE)
  # puntos = read.xls (#maintenanceDataPath
  #                   , sheet = 8, header = TRUE)
  
  #Clean up
  rutas <- rutas[,c(3:6,2)]
  
  #Keeps point Activos and with Category
  colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')
  
  #Attention with the S? instead of Si depending on how to read the text
  #Tipo	Identif	Bateria		Categoria
  
  #puntos <- puntos[puntos$Activo=='Si',c(2,3,4,6)]
  puntos <- instalations_table[instalations_table$Activa=='Si' ,c(1,3,7,10)]
  
  colnames(puntos) <- c('Tipo',	'Identif',	'Bateria','Categoria')
  #All point in at least one route
  punt <- c(as.character(rutas$origen),as.character(rutas$destino))
  not.present <- !(puntos$Identif %in% punt)
  
  #Show those not present.
  puntos$Identif[not.present]
  
  #Keep only those points present in at least one route
  puntos <- puntos[!not.present,]
  
  #Rownames puntos
  rownames(puntos) <- puntos$Identif
  
  result <- list(tareas_log = tareas_log, recursos = recursos, 
                 frecuencia = frecuencia, purgas = purgas, 
                 frecuencia_purgas = frecuencia_purgas, 
                 rutas = rutas, puntos = puntos)
}

getShortestPaths <- function(logisData){
  #### 02. Explore graph ####
  rutas <- logisData$rutas
  puntos <- logisData$puntos
  
  g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)
  comp <- components(g)
  
  #remove not connected points in the graph
  puntos.not_connect <- names(comp$membership[comp$membership!=1])
  print(puntos.not_connect)
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
  sp.m <- matrix (1, ncol=nrow(puntos)+1,nrow=nrow(puntos)+1)
  rownames(sp.m) <- c('Base',as.character(puntos$Identif))
  colnames(sp.m) <- c('Base',as.character(puntos$Identif))
  
  result <- list(sp = sp, sp.m = sp.m)
  return(result)
  
}

createStackLog <- function(logisData, shortestPaths, globalParameters){
 
  puntos <- logisData$puntos
  tareas_log <- logisData$tareas_log
  frecuencia <- logisData$frecuencia
  sp <- shortestPaths$sp

  years2eval <- globalParameters$years2eval
  total_days <- globalParameters$total_days
  
  
  stack_log <- data.frame(act=character(), prio = numeric(), act_id=numeric(), 
                          tarea=character(), unidad=character(), demanda= numeric(), 
                          day = numeric(), location=character(), tiempo = numeric(),
                          demanda_open = numeric(),
                          complete_day = numeric(),
                          cantidad_viajes = numeric(),
                          tiempo_total_viajes = numeric(),
                          respuesta = character(),
                          horaInicioMin = numeric(),
                          horaInicioMax = numeric())
  
  
  add_task_stack <- function (type_uni,day, type_number) {
    
    #select a location randomly
    #print(puntos$Identif)
    
    loc <- sample(puntos$Identif,1)

    transport_time_h <- sp[,loc]
    
    mat <- tareas_log[tareas_log$grupo==type_uni,]
    mat$day <-  day + mat$comienzo -1 
    
    # Add random start in same day for task with random tolerance
    withTolerance <-which(mat$tolerancia == "Random") 
    randomStart <- runif(length(withTolerance), 0, 1)
    mat$day[withTolerance] <- mat$day[withTolerance] + randomStart

    
    mat$tiempo_total <- transport_time_h + mat$`tiempo_car-des`
    mat$type_number <- type_number
    mat$loc <- loc
    mat$comp_dia <- NA
    mat$demanda_open <- mat$demanda
    mat$complete_day = NA
    mat$cantidad_viajes = NA
    mat$tiempo_total_viajes =NA
    
    # if WO select only one row from the matrix and re-assing
    if (type_uni=='WO')  {
      #set.seed(1)
      subtype = sample(c('WO Primaria','WO Secundaria Inyector',
                         'WO Secundaria Productores','WO Etiles',
                         'Terminacion','Abandono','Pulling Pesado'),size=1, 
                       prob = c(0.46,0.07,0.05,0.22,0.09,0.09,0.02))
      #print(subtype)
      rows2keep <- mat$actividad==subtype
      mat <- mat[rows2keep,]
    }
    
    stack_log <<- rbind(stack_log, 
                        mat[,c( 'grupo','prioridad','type_number','tarea',
                                'unidad', 'demanda', 'day', 'loc','tiempo_total', 'demanda_open','comp_dia','cantidad_viajes',
                                'tiempo_total_viajes', 'respuesta', 
                                'horaInicioMin', 'horaInicioMax')])
  }
  
  
  for (type_task_id in 1:nrow(frecuencia)) {
    cant_total <- frecuencia$Cantidad.Anual[type_task_id]*years2eval
    
    days_ini_task <- round(seq(1,total_days,total_days/cant_total)[1:cant_total],0)
    days_ini_task <- days_ini_task[!is.na(days_ini_task)]
    #set.seed(1)
    for (type_task_number in 1:length(days_ini_task)) {
      add_task_stack(as.character(frecuencia$Grupo[type_task_id]),days_ini_task[type_task_number],type_task_number)
    }
  }
  
  # Set prioridad = 0 to task with some value for maximum time of response
  #stack_log$prioridad[stack_log$respuesta != ""] <- 0
  stack_log$maxResponseDays <- as.character(stack_log$respuesta)
  stack_log$maxResponseDays[stack_log$maxResponseDays == "+ 1 Turno"] <- "24"
  stack_log$maxResponseDays[stack_log$maxResponseDays == ""] <- "-1"
  stack_log$maxResponseDays <- as.numeric(stack_log$maxResponseDays)
  stack_log$maxResponseDays[stack_log$maxResponseDays==-1] <- Inf
  stack_log$maxResponseDays <- stack_log$maxResponseDays/24
  
  result <- stack_log
  result
}

optimizePurguesRoute <- function(logisData, stack_log, shortest_paths,
                                 globalParameters){
  frecuencia_purgas <- logisData$frecuencia_purgas
  purgas <- logisData$purgas
  stack_log <- stack_log
  total_days <- globalParameters$total_days
  sp <- shortest_paths$sp
  sp.m <- shortest_paths$sp.m
  
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
    #print(sp.m)
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
    t.pozo <- labor*n.points #De donde sale labors
    return(c(t.transport,t.pozo,n.points,max(0,jor_time-t.transport-t.pozo)))
  }
  
  #Create a list of times resulting of removing each point
  tiempos.saved <- function(route) {
    #Checked
    n.points <- length(route)
    times.s <- rep(0,n.points)
    times <- tiempos.detail(route)
    
    if (n.points==1){
      times.s[1] <- sum(times)
    } else {
      #First element
      times.s[1] <- times [1] + times[2] - sp.m ['Base', route[2]]/60/60
      #Last element
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
      #print('Cost greather than saving')
      return (0)
    } else {
      td <- tiempos.total(route)[4]
      pos <- sample(which(saving > t2),1)
      
      if (td < t2[pos] + labor) {
        #print('Time increases the 240 min')
        return (0)
      } else {
        #print('To be added')
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
  
  
  
  ###############################################
  iterations <- 500
  jor_time <- 4
  labor <- 0.5
  
  for (cat_pozo in frecuencia_purgas$Categoria) {
    
    print('')
    print(paste(c('Routing the purges in instalation category: ',cat_pozo),collapse=''))
    
    sol <- list()
    initialize_pozos(cat_pozo)
    
    t.m <- do.call(rbind, lapply(sol,tiempos.total))
    
    colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')
    
    k<- 1
    
    while (k <= iterations) {
      #Initial diagnisys
      turnos <- nrow(t.m)
      tiempo.ocioso <- sum(t.m[,'av.time'])
      #print(c('k:',k,'-turnos:',turnos,'-tiempo.ocioso:',tiempo.ocioso))
      
      #Select the tour to extract 
      tour2extract <- sample(1:nrow(t.m),1, prob=(t.m[,'av.time']+1))
      
      #Declare route to work
      route2e <- sol[[tour2extract]]
      
      #Calculate the times saved
      t.s <- tiempos.saved(route2e)
      
      #Select the point to extract
      if (sum(t.s<=0)==length(t.s)) {  #The cycle does not offer any node worth to extract
        t.s[1]<-1 
      }
      
      pos2e <- sample(1:length(route2e),1, prob = t.s)
      node <- route2e[pos2e]
      
      #Calculate savings
      saving <- t.s[pos2e]
      
      #Select tour to add (ensure they are different)
      tour2add <- sample(1:nrow(t.m), 1,prob=(t.m[,'av.time']+1))
      
      while(tour2add==tour2extract) {
        tour2add <- sample(1:nrow(t.m), 1,prob=(t.m[,'av.time']+1))
      }
      
      route2a <- sol[[tour2add]]
      pos2a  <- where2add(route2a,node,saving)
      
      if (pos2a>0) {
        #Add the node in tour2a
        sol[[tour2add]] <- add.node(sol[[tour2add]],node, pos2a)
        
        #Update times
        #Recalculate the t.m add
        t.m[tour2add,] <- tiempos.total(sol[[tour2add]])
        
        #Remove node from tour2extract
        sol[[tour2extract]] <- remove.node(sol[[tour2extract]],pos2e)
        
        #What happens whan the sol tourtoextract is empty.
        if (length(sol[[tour2extract]])==0){
          
          #Remove prom sol and from t.m
          sol[[tour2extract]] = NULL
          t.m <- t.m[-tour2extract,]
          
          #Convert t.m into a matrix if it is a vector
          if(is.null(nrow(t.m))) {
            t.m <- matrix(t.m,nrow=1)
            colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')
            break 
          }
          
        } else { #tour2extract is not empty
          #Recalculate tm in tour2extract
          t.m[tour2extract,] <- tiempos.total(sol[[tour2extract]])
        } 
      } else {     #Continues all cases where pos>0  
      }
      k <- k+1
    }
    
    ##Add task to stack_tot
    stack_log_temp = NULL
    
    for (day_iteration in seq(1,total_days,frecuencia_purgas[cat_pozo,'dias'])) {
      rows2add <- cbind.data.frame(grupo=paste(c('Purga',cat_pozo),collapse='_'),
                                   prioridad=frecuencia_purgas[cat_pozo,'prioridad'],
                                   type_number=(1:nrow(t.m)), tarea='visita purga', unidad='Camion de Vacio',
                                   demanda=0.1, day=day_iteration, loc='varias',
                                   tiempo_total=t.m[,'t.transport']+t.m[,'t.pozo'], demanda_open=0.1,
                                   comp_dia=NA, cantidad_viajes=NA, tiempo_total_viajes=NA,
                                   respuesta = "", maxResponseDays = Inf,
                                   horaInicioMin = 0, horaInicioMax = 24)
      
      stack_log_temp <- rbind.data.frame(stack_log_temp, rows2add)
    } 
    
    print('')
    print(paste(c('Instalation category ',cat_pozo,' can be visited with ', nrow(t.m),
                  ' cycles of 4 hours each.'),collapse=''))
    
    #Add stack_log_temp to stack log
    print('')
    print(paste(c('adding ',nrow(stack_log_temp),' to the stack of needs.'),collapse=''))
    
    stack_log <- rbind.data.frame(stack_log, stack_log_temp)
    
  }#End loop frecuencia_purgas$Categoria
  
  #Order stack_log by prioridad and day
  stack_log <- stack_log[order(stack_log$prioridad,stack_log$day),]
  stack_log <- stack_log[!is.na(stack_log$day),]
  
  #Remove the task beyond the end of simulation.
  stack_log <- stack_log[stack_log$day<=total_days,]
  
  #Multiply by 3 the task related to Potable
  Pot_tasks <- stack_log[stack_log$unidad=='Potable',]
  stack_log <- rbind(stack_log, Pot_tasks)
  stack_log <- rbind(stack_log, Pot_tasks)
  
  result <- stack_log
  result
}

add_rest_vehicle <- function(stack_log, type_uni) {
  
  #Add descanzo to stack_log
  if(ncol(stack_log)==13) {
    rows_descanzo <-data.frame('Descanzo',1,1,'descanzo',type_uni,0.1,1:day_iteration,
                               NA,(1-recursos[type_uni,'Disponibilidad'])*24,0.1,NA,
                               1,(1-recursos[type_uni,'Disponibilidad'])*24)
  } else {
    rows_descanzo <-data.frame('Descanzo',1,1,'descanzo',type_uni,0.1,1:day_iteration,
                               NA,(1-recursos[type_uni,'Disponibilidad'])*24,0.1,NA,
                               1,(1-recursos[type_uni,'Disponibilidad'])*24,NA)
  }
  
  if (type_uni=='Potable') {
    rows_descanzo[,13] <- (1-recursos[type_uni,'Disponibilidad'])*9
  }
  colnames(rows_descanzo) <- colnames(stack_log)
  stack_log <<- rbind(stack_log,rows_descanzo)
  
  result <- stack_log
}

makeStackLogFromLogisData <- function(logisData, globalParameters){
  sp <- getShortestPaths(logisData)
  sl <- createStackLog(logisData, sp, globalParameters)
  sl <- optimizePurguesRoute(logisData, sl, sp, globalParameters) 
  
  result <- list(stack_log = sl, shortest_paths = sp)
}

makeStackLogFromFiles <- function(logisticDataPath, maintenanceDataPath,
                                  globalParameters){
  logisData <- readLogisticsFiles(logisticDataPath,
                                  maintenanceDataPath,
                                  globalParameters)
  print('tareasLOG')
  print(logisData$tareas_log) #ok
  
  aux <- makeStackLogFromLogisData(logisData, globalParameters)
  print('stackLOG')
  print(aux$stack_log) #no ok
  result <- list(stack_log = aux$stack_log, shortest_paths = aux$shortest_paths, logisData = logisData)
}

makeStackLogFromFilesWAL <- function(logisticDataPath, maintenanceDataPath,
                                  globalParameters,n_pozos){
  logisData <- readLogisticsFiles(logisticDataPath,
                                  maintenanceDataPath,
                                  globalParameters)
  
  #adjust logisData$frecuencia according to n_pozos
  logisData[['frecuencia']][3,'Cantidad.Anual'] <- round(
    logisData[['frecuencia']][3,'Cantidad.Anual']*n_pozos/400,0)
  logisData[['frecuencia']][4,'Cantidad.Anual'] <- round(
    logisData[['frecuencia']][4,'Cantidad.Anual']*n_pozos/400,0)
  
  print(logisData[['frecuencia']])
  aux <- makeStackLogFromLogisData(logisData, globalParameters)
  
  result <- list(stack_log = aux$stack_log, shortest_paths = aux$shortest_paths, logisData = logisData)
}

