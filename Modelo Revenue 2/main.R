########################################################
# this is the main script that implement all functions #
########################################################

#### 0. reset enviroment ####
rm(list = ls())
cat("\f")


source('functions/read_data.R')
source('functions/simulation.R')
source('functions/get_metrics.R')
source('functions/logistic/simulation.R')
source('functions/build_prod_tables.R')
source("functions/logistic/oldfunctions/logisticSimulation.R")
source('functions/logistic/build_prod_tables_log.R')
source('functions/main_functions.R')

library(rgenoud)

#### 1. read data to generate tables ####
list_data <- read_data()

rsm_table <- list_data [['rsm_table']]

services <- list_data[['services']]



#### 2. define a activity level & n_cuad & other parameters ####
activity_level <- data.frame(PozoBM=385,
                             PozoBES=90,
                             PozoIny=115,
                             Bateria=37, 
                             Satelite=36,
                             Pl_Petroleo=2,
                             Pl_Gas=1,
                             Pl_Agua=3, 
                             Subestacion=37,
                             Gasoducto=37,
                             Motogen= 4,
                             Oleoducto=1,
                             Colector=42,
                             Oficina=1)

# num_cuad <- 3
total_days <- 365
turnos <- NA
set.seed(1)

#### 5. read oil weel data ####
file_table <- read_instalations()
instalations_table <- file_table[['instalations_table']]
parameters <- file_table[['parameters']]
oil_price <-  file_table[['oil_price']]

#deactivate instalations whose father is not included
instalations_table <- deactivate_by_father(instalations_table)

#### 3. generates the prod_table ####
#prod_table <- generates_prod_tables(services)
load("prod_table.RData")

#### 4. generates the logistic_table ####
#prod_table_log <- generates_prod_table_log()
load("prod_table_log.RData")

#### 6. read production declination #####
declination_table <- read_declination()

#### 7. affect declination due to WOs ####
WO_data <- read_WO_data()
declination_table <- affect_declination_wWO(declination_table,
                                                  WO_data,
                                                  instalations_table)

#### 7. build production table #####
net_production_table <- build_production_table(instalations_table,declination_table)
brute_production_table <- build_production_table_br(instalations_table,declination_table)

### the variable x is the representation of a solution  #
pozos_ids <- instalations_table$Detalle=='En prod'&
  instalations_table$Tipo.de.instalación=='Petrolífero'
n_pozos <- sum(pozos_ids)

#
#1:n_pozos binariy variable names names 

#services (max 12)
#(n_pozos+1):(n_pozos+4) n Recorredores
#(n_pozos+5):(n_pozos+8) n Supervisores
#(n_pozos+9):(n_pozos+12) n Mant. Electrico
#(n_pozos+13):(n_pozos+16) n Mant. Mecanico
#(n_pozos+17):(n_pozos+20) n Mant. Instrumental

#logistic (max 10)
#(n_pozos+21):(n_pozos+24) "Articulado Semi"  
#(n_pozos+25):(n_pozos+28) Camion de Vacio"  
#(n_pozos+29):(n_pozos+32) "Potable"  
#(n_pozos+33):(n_pozos+36) "Motobomba"
#(n_pozos+37):(n_pozos+40) "Camion con Hidro" 
#(n_pozos+41):(n_pozos+44) "Portacontenedor"

source('functions/solution_functions.R')
sol <- sample(c(1,0),n_pozos +44, replace=T)
sol_names <- c(rownames(instalations_table[instalations_table$Detalle=='En prod',]),
               paste0('rec',1:4,sep=""),
               paste0('sup',1:4,sep=""),
               paste0('me',1:4,sep=""),
               paste0('mm',1:4,sep=""),
               paste0('mi',1:4,sep=""),
               paste0('as',1:4,sep=""),
               paste0('cv',1:4,sep=""),
               paste0('pot',1:4,sep=""),
               paste0('mot',1:4,sep=""),
               paste0('ch',1:4,sep=""),
               paste0('pc',1:4,sep=""))

pozos_names <- sol_names[1:n_pozos]

#optimize per year
nvars <- length(sol)
Domains <- matrix(c(rep(0,length(sol)),rep(1,length(sol))),ncol=2)

budget_table <- read_budget_table()

load(file = "data/start_val.RData")

for (year in 3:10) {
  print(year)
  optim_sol <-genoud(get_revenue,year=year,print.level=1,
                     max=T,nvars=nvars,Domains = Domains,
                     starting.values = start_val,
                     MemoryMatrix = F,
                     boundary.enforcement = 2,
                     wait.generations=100,
                     
                     data.type.int =T,pop.size = 300,max.generations = 200)
  
  save_xls_1year(optim_sol$par,year)
  save_xls_1year_detail(optim_sol$par,year)
  start_val <- optim_sol$par
}

#save(start_val, file = "data/start_val.RData")

