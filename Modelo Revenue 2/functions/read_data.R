library(xlsx)
library(openxlsx)

read_data <- function() {
  file_name <- 'data/revenue data.xlsx'
  #read all data and returns 
  rsm_table <- read.xlsx (file_name,sheet = 1)
  rsm_table[is.na(rsm_table)] <- 0
  
  services <- unique(rsm_table$Servicio)
  
  log_table <- read.xlsx (file_name,sheet = 2)
  bu_log_table <- read.xlsx (file_name,sheet = 3)
  rownames(bu_log_table) <- bu_log_table$Grupo
  
  tasks_table <- list(rsm_table,log_table,services,bu_log_table)
  names(tasks_table) <- c('rsm_table','log_table','services','bu_log_table')
  return(tasks_table)
}

read_instalations <- function(){
  file_name <- 'data/1. Matriz de relacionamiento V03.xlsx'
  instalations_table <- read.xlsx (file_name,sheet = 1)
  rownames(instalations_table) <- instalations_table$`Identificador.Sin.-`
  
  #remove instalations that are not in route
  
  
  reference_table <- read.xlsx (file_name,sheet = 2)
  parameters <- data.frame(precio_crudo=reference_table[reference_table$Campo=='Precio Neto','Valor'] )
  
 oil_price <- read.xlsx (file_name,sheet = 3)
  
  file_table <- list(instalations_table,parameters,oil_price)
  names(file_table) <- c('instalations_table','parameters','oil_price')
  return(file_table)
}

read_declination <- function(){ 
  file_name <- 'data/factor declinacion.xlsx'
  declination <- read.xlsx (file_name,sheet = 1)
  rownames(declination) <- declination$año
  declination <- declination[,-1]
  colnames(declination) <- paste0(rep('ano_',10),1:10)
  return(declination)
}

read_budget_table <- function(){
  file_name <- 'data/IFCO ET 2017 v2 FC.xlsx'
  budget_table <- read.xlsx2 (file_name,sheetName = 1)
  
  budget_table <-budget_table[budget_table$Consider==1,colnames(budget_table)[1:2]]
  budget_table$ET2017 <- round(as.numeric(as.character(budget_table$ET2017)),2)
  return(budget_table)
}

#load WO
read_WO_data <- function() {
  total_years <- 10
  file_name <- 'data/Planificacion WO.xlsx'
  WO_list <- list()
  cant_WO <- rep(NA,total_years)
  for (i in 1:total_years) {
    WO_list[[i]] <- read.xlsx(file_name,sheet=i)
    #eliminar fila 2
    WO_list[[i]] <- WO_list[[i]] [-2,]
    WO_list[[i]] <- WO_list[[i]] [-1,]
    rownames(WO_list[[i]]) <- WO_list[[i]][,1]
    #eliminar col1
    WO_list[[i]] <- WO_list[[i]] [,-1]
    colnames(WO_list[[i]]) <- c('cantidad','dias','total')
    cant_WO[i] <-  as.numeric(WO_list[[i]]['WO Primaria','cantidad'])
    
  }
  parametros_WO <- read.xlsx(file_name,sheet=11)
  p_success <- parametros_WO[2,2]
  WO_declination <- parametros_WO[1,2:11]
  out_list <- list(WO_declination,p_success)
  names(out_list) <- c('WO_declination','p_success')
  return(out_list)
}

