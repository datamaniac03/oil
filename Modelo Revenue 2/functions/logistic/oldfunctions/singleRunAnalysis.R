require(dplyr)

# Funciones para analisis de simulacion. Requiere un taskScheduled previamente simulado.

numberNonCompleteTasks <- function(taskScheduled){
  # Considera tareas con campo "comp_dia" == NA

  totalTasks <- count(taskScheduled, 'prioridad')
  
  
  
  nonCompleteTasks <- count(taskScheduled[is.na(taskScheduled$comp_dia),], 'prioridad') 
  completeTasks <- count(taskScheduled[!is.na(taskScheduled$comp_dia),], 'prioridad')
  result <- full_join(totalTasks, completeTasks, by=c("prioridad" = "prioridad"))
  result <- full_join(result, nonCompleteTasks, by=c("prioridad" = "prioridad"))
  names(result) <- c("prioridad", "totalTasks", "completeTasks", "nonCompleteTasks")
  as.data.frame(result)
}

numberReScheduledTasks <- function(taskScheduled){
  # Considera tareas con dia de inicio real diferente al programado
  totalTasks <- count(taskScheduled, 'prioridad')
  taskScheduled <- taskScheduled[!is.na(taskScheduled$startTime),]
  reScheduledTasks <- count(taskScheduled[
    floor(taskScheduled$startTime) != floor(taskScheduled$originalDay),], 'prioridad') 
  onTimeTasks <- count(taskScheduled[
    floor(taskScheduled$startTime) == floor(taskScheduled$originalDay),], 'prioridad') 
  result <- full_join(totalTasks, onTimeTasks, by=c("prioridad" = "prioridad"))
  result <- full_join(result, reScheduledTasks, by=c("prioridad" = "prioridad"))
  names(result) <- c("prioridad", "totalTasks", "onTimeTasks","reScheduledTasks")
  as.data.frame(result)
}

numberTasksWithResponseTimeDidLater <- function(taskScheduled){
  # Considera solo tareas con tiempo de respuesta
  taskScheduled <- taskScheduled[taskScheduled$maxResponseDays < Inf,]
  #totalTasks <- length(taskScheduled[,1])
  taskScheduled$budgetFinish <- taskScheduled$day + taskScheduled$maxResponseDays
  taskScheduled$comp_dia[is.na(taskScheduled$comp_dia)] <- 999999999
  taskScheduled$deviation <- taskScheduled$comp_dia - taskScheduled$budgetFinish
  
  totalTasksWithResponseTime <- count(taskScheduled, 'prioridad')
  outOfResponseTime <- count(taskScheduled[
    taskScheduled$deviation > 0,], 'prioridad') 
  onResponseTime <- count(taskScheduled[
    taskScheduled$deviation <= 0,], 'prioridad') 
  
  
  #result <- data.frame(taskWithResponseTime = totalTasks, 
  #                     taskNotFinishOnResponseTime = 
  #                       sum(taskScheduled$deviation >= 0))
  
  result <- full_join(totalTasksWithResponseTime, onResponseTime, by=c("prioridad" = "prioridad"))
  result <- full_join(result, outOfResponseTime, by=c("prioridad" = "prioridad"))
  names(result) <- c("prioridad", "totalTasks", "onResponseTime","outOfResponseTime")
  as.data.frame(result)
}

numberTasksInsideOfTimeWindow <- function(taskScheduled){
  totalTasks <- count(taskScheduled, 'prioridad')
  taskScheduled <- taskScheduled[!is.na(taskScheduled$startTime),]
  taskScheduled$auxStartTime <- 24 * (taskScheduled$startTime - 
                                        floor(taskScheduled$startTime))
  taskOnWindows <- count(taskScheduled[
    ((taskScheduled$auxStartTime >= taskScheduled$horaInicioMin) && 
       (taskScheduled$auxStartTime <= taskScheduled$horaInicioMax)),],
    'prioridad')
  
  taskBeforeWindows <- count(taskScheduled[
    ((taskScheduled$auxStartTime < (taskScheduled$horaInicioMin - 0.001))),],
    'prioridad')
  
  taskAfterWindows <- count(taskScheduled[
    ((taskScheduled$auxStartTime > (taskScheduled$horaInicioMax + 0.001))),],
    'prioridad')
  
 
  result <- full_join(totalTasks, taskOnWindows , by=c("prioridad" = "prioridad"))
  result <- full_join(result, taskBeforeWindows, by=c("prioridad" = "prioridad"))
  result <- full_join(result, taskAfterWindows, by=c("prioridad" = "prioridad"))
  names(result) <- c("prioridad", "totalTasks", "onWindow","beforeWindow", "afterWindow")
  
  # result <- full_join(totalTasks, taskBeforeWindows, by=c("prioridad" = "prioridad"))
  # result <- full_join(result, taskAfterWindows, by=c("prioridad" = "prioridad"))
  # names(result) <- c("prioridad", "totalTasks","beforeWindow", "afterWindow")  
   as.data.frame(result)
}
  

resourcesUsage <- function(taskScheduled){
  result <- summarise(group_by(taskScheduled, 'assignedVehicle', 'assignedType'), 
                      sum(tiempo_total_viajes, na.rm = TRUE))
  names(result) <- c("Assigned Vehicle", "Assigned Type", "Total Time")
  result <- as.data.frame(result[!is.na(result$"Assigned Vehicle"),])
  result
}

resourcesTypeUsage <- function(taskScheduled){
  result <- summarise(group_by(taskScheduled, assignedType),
                      sum(tiempo_total_viajes, na.rm = TRUE))
  names(result) <- c("Assigned Vehicle", "Total Time")
  result <- as.data.frame(result[!is.na(result$"Assigned Vehicle"),])
  result
}

#################################################################
# Tasks Analysis consolidation
#################################################################

consolidateAnalisysOfManySimulations <- function(simulations, 
                                                 scenarios, 
                                                 typeResource,
                                                 analysisFunctionName){
  analysisFunction <- get(analysisFunctionName)
  # Join all evaluation in a single dataframe, with an additional
  # column indicating the number of vehicles availables
  aux <- Reduce(rbind, lapply(1:length(simulations), 
                              function(x){
                                foo <- analysisFunction(simulations[[x]])
                                # If the foo data.frame is empty, fill it with NA
                                if (nrow(foo)<=0){
                                  fieldNames <- names(foo)
                                  foo <- rbind(foo, rep(NA, times=ncol(foo)))
                                  names(foo) <- fieldNames
                                }
                                # Add numVehicles field
                                cbind(
                                  vehicleType = typeResource,
                                  numVehicles = scenarios[[typeResource]][x], 
                                  foo)
                              }))
  aux
}

consolidateManyAnalisys  <- function(simulations, 
                                     scenarios, 
                                     typeResource,
                                     analysisFunctionNameList){
  aux <- lapply(analysisFunctionNameList, function(x){
    consolidateAnalisysOfManySimulations(simulations, scenarios,
                                         typeResource, x)
  })
  result <-Reduce(function(x,y){full_join(x, y, 
                                          by=c("vehicleType" = "vehicleType",
                                          "numVehicles" = "numVehicles",
                                          "prioridad" = "prioridad"))},
                  aux)
  as.data.frame(result)
  
  
}

summarySimulations <- function(simulations, scenarios, typeResource){
  tasksResult <- consolidateManyAnalisys(simulations, scenarios, 
                                         typeResource, c(
                                           "numberNonCompleteTasks", 
                                           "numberReScheduledTasks", 
                                           "numberTasksWithResponseTimeDidLater",
                                           "numberTasksInsideOfTimeWindow"))
  tasksResult
}

