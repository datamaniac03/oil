require(xlsx)
source("functions/logistic/oldfunctions/singleRunAnalysis.R")

writeXlsx_singleRunAnalysisReport <- function(simulations, fileName,
                                              scenarios, typeResource,
                                              analysisFunctionName,
                                              append=FALSE){
  
  aux <- consolidateAnalisysOfManySimulations(simulations, scenarios,
                                              typeResource,
                                              analysisFunctionName)

  analysisFunctionName <- strtrim(analysisFunctionName, 30)
  write.xlsx(x = aux, file = fileName, sheetName = analysisFunctionName,
             append = append)
}

createExcel_singleRunComparison <- function(simulations, fileName,
                                            scenarios, typeResource,
                                            analysis = c(
                                              "numberNonCompleteTasks",
                                              "numberReScheduledTasks",
                                              "numberTasksWithResponseTimeDidLater",
                                              "resourcesUsage",
                                              "resourcesTypeUsage",
                                              "numberTasksInsideOfTimeWindow")){
  result <- NA
  if (length(analysis)>0){
    writeXlsx_singleRunAnalysisReport(simulations, fileName,
                                      scenarios, typeResource,
                                      analysis[1])
    #if(length(analysis)>1){
    #   lapply(analysis[2:length(analysis)], function(x){
    #     writeXlsx_singleRunAnalysisReport(simulations, fileName,
    #                                       scenarios, typeResource,
    #                                       x, append = TRUE) 
    #   })
    # }
    result <- fileName
  }
  result
}

updateExcelWithTaskSummaryAnalisys <- function(simulations, fileName,
                                               scenarios, typeResource){
  aux <- summarySimulations(simulations, scenarios, typeResource)
  write.xlsx(x = aux, file = fileName, sheetName = paste(typeResource, 
                                                         "taskAnalysis",
                                                         sep="-"),
             append = TRUE)
}