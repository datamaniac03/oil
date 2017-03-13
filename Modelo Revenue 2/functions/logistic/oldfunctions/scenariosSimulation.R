source("functions/logistic/oldfunctions/scheduleResources.R")

makeScenarios <- function(resources, rangeMin = 0.5, rangeMax = 2, 
                          stepSize = 0.5){
  classes <- seq(rangeMin, rangeMax, stepSize)
  values <- lapply(resources$Cantidad,
                   function(x){unique(ceiling(classes*x))})
  names(values) <- resources$Tipo.Unidad
  result <- values
}

simulateScenario_changeOneResource <- function(tasks, 
                                              resources, 
                                              scenarios, 
                                              typeResource,
                                              globalParameters){
  simulations <- lapply(scenarios[[typeResource]], function(x){
    
    print(c("Simulation - ", typeResource, ": " , as.character(x)))

    auxTasks <- tasks[grepl(typeResource, as.character(tasks$unidad)),]
    auxResources <- resources
    auxResources$Cantidad[auxResources$Tipo.Unidad != typeResource] <- 0
    auxResources$Cantidad[auxResources$Tipo.Unidad == typeResource] <- x 
    resourceSchedule <- makeResourceSchedulleStructure(auxResources)
    #print(auxTasks)

    taskScheduled <- scheduleTasks(auxTasks, auxResources, 
                                   resourceSchedule, globalParameters)   
    
  })
  names(simulations)<-sapply(scenarios[[typeResource]],function(x){
    paste(typeResource, "-", x, sep="")
  })
  result <- simulations
}



makeRandomSituations <- function(){
  NA
}