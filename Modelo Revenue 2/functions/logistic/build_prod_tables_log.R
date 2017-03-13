build_prod_table_log <- function(transport) {

  
  file_name <- paste('data/log_metrics/Escenarios_',transport,'.xlsx',sep='')
  #read all data and returns 
  temp_table <- read.xlsx (file_name,sheet = 1)
  
  temp_table <- temp_table[temp_table$prioridad==1,]
  
  out_table <- data.frame(num_cuad=temp_table$numVehicles,
                          prod=(temp_table$completeTasks/temp_table$totalTasks)^0.25)
  

  return(out_table)
    
  
}

generates_prod_table_log <- function() {
  #executes the logistic scripts and save the files.
  gp <- setGlobalParameters() 
  aux <- makeStackLogFromFiles("data/revenue data.xlsx",
                               NA,
                               gp)
  
  
  stack_log <- aux$stack_log
  print(stack_log)
  sp <- aux$shortest_paths
  logisData <- aux$logisData
  rm(aux)
  
  # simulates different resource scenarios
  scenarios <- makeScenarios(logisData$recursos)
  #redefine scenarios
  
  for (transport_type in setdiff(names(scenarios),'Cisterna')) {
    scenarios[[transport_type]] <- 1:10
  }
  
  for (transport_type in setdiff(names(scenarios),'Cisterna')) {
    
    fileName <- paste0('data/log_metrics/Escenarios_',transport_type,'.xlsx',collapse="")

    simulations <- simulateScenario_changeOneResource(
      stack_log, logisData$recursos, scenarios, transport_type, gp)
    
    createExcel_singleRunComparison(simulations, fileName, 
                                    scenarios, transport_type)
  }
  
  #### 3. generates the prod_table_log ####
  transports <- setdiff(names(scenarios),'Cisterna')
  
  prod_table_log <- list()
  for (transport in transports) {
    prod_table_log[[transport]] <- build_prod_table_log(transport)
  }
  
  save(prod_table_log, file = "prod_table_log.RData")
  return(prod_table_log)
}