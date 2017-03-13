
# Build a data.frame for storing the current schedule of individuals vehicles.
makeResourceSchedulleStructure <- function(resources){
  auxResourceType <- c()
  for (i in 1:length(resources[,1])){
    auxResourceType <- c(auxResourceType, 
                         rep(as.character(resources$Tipo.Unidad[i]),
                             times = resources$Cantidad[i]))
  }
  auxResourceId <- 1:length(auxResourceType)
  data.frame(id = auxResourceId, type = auxResourceType, freeTime = 1)

  
}

scheduleTasks2 <- function(tasks, resources, resourceSchedule,
                          globalParameters){
  aux <- tasks[tasks$demanda_open>0,]
  #aux <- aux[with(aux, order(day, prioridad)),]
  aux <- aux[with(aux, order(day, prioridad, maxResponseDays)),]
  aux$originalDay <- aux$day
  aux$startTime <- rep(NA, times=length(aux[,1]))
  finish <-FALSE 
  i <- 1
  while(finish==FALSE){
    #potentialResources <- resourceSchedule$id[
    #  as.character(resourceSchedule$type) == as.character(aux$unidad[i])]
    potentialResources <- resourceSchedule$id[
      sapply(as.character(resourceSchedule$type), 
             function(x){grepl(x, as.character(aux$unidad[i]))})]
    
    freeTimesIndex <- resourceSchedule$id[order(resourceSchedule$freeTime)]
    freeTimesIndex <- freeTimesIndex[freeTimesIndex %in% potentialResources]
    assignedResource <- freeTimesIndex[1]
    assignedType <- as.character(resourceSchedule$type[resourceSchedule$id == assignedResource])
    
    demanda <- ceiling(aux$demanda[i] / as.numeric(as.character(resources$Capacidad[
      as.character(resources$Tipo.Unidad) == assignedType])))
    
    startTime <- max(resourceSchedule$freeTime[
      resourceSchedule$id == assignedResource], 
      aux$day[i])
    
    #Check if the task can be done in same day
    #if (FALSE) {
    if (floor(startTime) > floor(aux$day[i])){
      aux$day[i] <- floor(startTime)
      aux[i:length(aux[,1]),] <- aux[with(aux[i:length(aux[,1]),], order(day, prioridad, maxResponseDays)),]
    }else{
      if (startTime <= globalParameters$total_days){
        taskDuration <- demanda * aux$tiempo_total[i] / 24
        taskEndTime <- startTime + taskDuration
        resourceSchedule$freeTime[resourceSchedule$id == assignedResource] <-taskEndTime 
        aux$comp_dia[i] <- taskEndTime
        aux$cantidad_viajes[i] <- demanda
        aux$tiempo_total_viajes[i] <-taskDuration
        aux$demanda_open[i] <- 0
        aux$startTime[i] <- startTime
        aux$assignedVehicle[i] <- assignedResource
        aux$assignedType[i] <- as.character(resourceSchedule$type[resourceSchedule$id == assignedResource])      
        
      }
      i <- i + 1 
      # Finish when scheduled all task or all resources will be free after max time
      auxRest <-aux[i:length(aux[,1]),] 
      #minFreeTime <- min(c(auxRest$day[!is.na(auxRest$comp_dia)]), globalParameters$total_days +1) #Avoid empty sets
      finish <- (i > length(aux[,1])) #|| (minFreeTime > globalParameters$total_days)
    }
  }
  aux
}

scheduleTasks3 <- function(tasks, resources, resourceSchedule,
                           globalParameters){
  #aux <- tasks[with(tasks, order(prioridad, maxResponseDays, day)),]
  aux <- tasks[with(tasks, order(prioridad, day, maxResponseDays)),]
  #aux <- tasks[with(tasks, order(day, prioridad, maxResponseDays)),]
  aux$originalDay <- aux$day
  aux$startTime <- NA
  aux$id <- 1:length(aux[,1])
  aux$assignedVehicle <- NA
  aux$assignedType <- NA      
  
  for (currRes in 1:length(resourceSchedule[,1])){
    assignedType <- as.character(resourceSchedule$type[currRes])
    finish <- FALSE
    
    potentialTasksID <- aux$id[
      sapply(as.character(aux$unidad), 
             function(x){grepl(assignedType, x)})]
   

    while((finish == FALSE) && 
          (resourceSchedule$freeTime[currRes] <= globalParameters$total_days)){
      potentialTasks <- aux[aux$id %in% potentialTasksID,]
      potentialTasks <- potentialTasks[potentialTasks$demanda_open > 0,]
      auxPotentialTasks <- potentialTasks[potentialTasks$day <= 
                                            resourceSchedule$freeTime[currRes],]
      
      if (sum(!is.na(auxPotentialTasks[,1]))>0){
        potentialTasks <- auxPotentialTasks
      }else{
        potentialTasks <- potentialTasks[potentialTasks$day == 
                                           min(potentialTasks$day),]
      }

      if (sum(!is.na(potentialTasks[,1]))>0){
        #selectedTaskID <- potentialTasks$id[1]
        selectedTaskID <- potentialTasks$id[which(!is.na(potentialTasks$id))[1]]
        demanda <- aux$demanda[aux$id == selectedTaskID]
        startTime <- max(resourceSchedule$freeTime[currRes], 
                         aux$day[aux$id == selectedTaskID])
        taskDuration <- demanda * aux$tiempo_total[aux$id == selectedTaskID] / 24
        taskEndTime <- startTime + taskDuration
        resourceSchedule$freeTime[currRes] <-taskEndTime 
        aux$comp_dia[aux$id == selectedTaskID] <- taskEndTime
        aux$cantidad_viajes[aux$id == selectedTaskID] <- demanda
        aux$tiempo_total_viajes[aux$id == selectedTaskID] <-taskDuration
        aux$demanda_open[aux$id == selectedTaskID] <- 0
        aux$startTime[aux$id == selectedTaskID] <- startTime
        aux$assignedVehicle[aux$id == selectedTaskID] <- currRes
        aux$assignedType[aux$id == selectedTaskID] <- assignedType      
        
      }else{
        finish <- TRUE
      }
    }
  }
  result <- aux
}

scheduleTasks <- function(tasks, resources, resourceSchedule,
                           globalParameters){
  #aux <- tasks[with(tasks, order(prioridad, maxResponseDays, day)),]
  #aux <- tasks[with(tasks, order(day, prioridad, maxResponseDays)),]
  aux <- tasks

  aux$originalDay <- aux$day
  aux$day[(aux$day - floor(aux$day))==0] <- aux$day[(aux$day - floor(aux$day))==0] + 
    aux$horaInicioMin[(aux$day - floor(aux$day))==0]/24
  aux$startTime <- NA
  aux$assignedVehicle <- NA
  aux$assignedType <- NA  
  aux <- aux[with(tasks, order(prioridad, day, maxResponseDays, horaInicioMin, horaInicioMax)),]
  aux$id <- 1:length(aux[,1])
  
  for (assignedType in unique(as.character(resourceSchedule$type))){
    #assignedType <- as.character(resourceSchedule$type[currRes])
    potentialVehiclesID <- resourceSchedule$id[resourceSchedule$type==assignedType]


    minFreeTime <- min(resourceSchedule$freeTime[
      resourceSchedule$id %in% potentialVehiclesID]) 

        finish <- FALSE
    
    potentialTasksID <- aux$id[
      sapply(as.character(aux$unidad), 
             function(x){grepl(assignedType, x)})]

    while((finish == FALSE) && 
          (minFreeTime <= globalParameters$total_days)){
      
      potentialVehicles <- resourceSchedule[
        resourceSchedule$id %in% potentialVehiclesID,]
      currRes <- potentialVehicles$id[which(potentialVehicles$freeTime == 
                                             min(potentialVehicles$freeTime))[1]]

      potentialTasks <- aux[aux$id %in% potentialTasksID,]
      potentialTasks <- potentialTasks[potentialTasks$demanda_open > 0,]

      auxPotentialTasks <- potentialTasks[potentialTasks$day <= 
                                            resourceSchedule$freeTime[currRes],]      
      if (sum(!is.na(auxPotentialTasks[,1]))>0){
        potentialTasks <- auxPotentialTasks
        
        
        auxPotentialTasks <- potentialTasks[
          potentialTasks$horaInicioMin/24 >= resourceSchedule$freeTime[currRes] - floor(resourceSchedule$freeTime[currRes]),]
        auxPotentialTasks <- auxPotentialTasks[
          potentialTasks$horaInicioMax/24 <= resourceSchedule$freeTime[currRes] - floor(resourceSchedule$freeTime[currRes]),]
        if (sum(!is.na(auxPotentialTasks[,1]))>0){
          potentialTasks <- auxPotentialTasks
        }            
        
      }else{
        potentialTasks <- potentialTasks[potentialTasks$day == 
                                           min(potentialTasks$day),]
      }
      
      if (sum(!is.na(potentialTasks[,1]))>0){
        #selectedTaskID <- potentialTasks$id[1]
        selectedTaskID <- potentialTasks$id[which(!is.na(potentialTasks$id))[1]]
        demanda <- aux$demanda[aux$id == selectedTaskID]
        startTime <- max(resourceSchedule$freeTime[currRes], 
                         aux$day[aux$id == selectedTaskID])
        taskDuration <- demanda * aux$tiempo_total[aux$id == selectedTaskID] / 24
        taskEndTime <- startTime + taskDuration
        resourceSchedule$freeTime[currRes] <-taskEndTime 
        aux$comp_dia[aux$id == selectedTaskID] <- taskEndTime
        aux$cantidad_viajes[aux$id == selectedTaskID] <- demanda
        aux$tiempo_total_viajes[aux$id == selectedTaskID] <-taskDuration
        aux$demanda_open[aux$id == selectedTaskID] <- 0
        aux$startTime[aux$id == selectedTaskID] <- startTime
        aux$assignedVehicle[aux$id == selectedTaskID] <- currRes
        aux$assignedType[aux$id == selectedTaskID] <- assignedType  

        minFreeTime <- min(resourceSchedule$freeTime[
          resourceSchedule$id %in% potentialVehiclesID])  

      }else{
        finish <- TRUE
      }
    }
  }
  result <- aux
}