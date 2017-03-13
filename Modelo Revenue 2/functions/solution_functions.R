#solution functions
get_net_production <- function(sol, year, production_table, instalations_table) {
  
  produccion_neta_real_vec <-  sol[1:n_pozos]*
                  production_table[pozos_names,year]
 
  produccion_neta_real_vec[is.na(produccion_neta_real_vec)] <- 0
  produccion_neta_real <- sum(produccion_neta_real_vec)
  return(produccion_neta_real*365)

}

get_brute_production <- function(sol, year, production_table_br, instalations_table) {
  
  produccion_bruta_real_vec <-sol[1:n_pozos]* #instalations_table[pozos_names,'Producción.Bruta.Real']*
    production_table_br[pozos_names,year]
  
  produccion_bruta_real_vec[is.na(produccion_bruta_real_vec)] <- 0
  produccion_bruta_real <- sum(produccion_bruta_real_vec)
  return(produccion_bruta_real*365)
  
}

convert_from_bin <- function (vect, limit) {
  value <- sum(2^(3:0)*vect)
  if (value>limit) {
    value <- value-limit
  }
  if (value==0) {
    value <- 1
  }
  return(value)
}

get_ncuad_ntrans <- function (sol,n_pozos) {
  n_rec <- convert_from_bin(sol[(n_pozos+1):(n_pozos+4)],12)# n Recorredores
  n_sup <- convert_from_bin(sol[(n_pozos+5):(n_pozos+8)],12) #n Supervisores
  n_me <- convert_from_bin(sol[(n_pozos+9):(n_pozos+12)],12) #n Mant. Electrico
  n_mm <- convert_from_bin(sol[(n_pozos+13):(n_pozos+16)],12) #n Mant. Mecanico
  n_mi <- convert_from_bin(sol[(n_pozos+17):(n_pozos+20)],12) #n Mant. Instrumental
  
  #logistic (max 10)
  n_as <- convert_from_bin(sol[(n_pozos+21):(n_pozos+24)],10) #"Articulado Semi"  
  n_cv <- convert_from_bin(sol[(n_pozos+25):(n_pozos+28)],10) #Camion de Vacio"  
  n_pot <- convert_from_bin(sol[(n_pozos+29):(n_pozos+32)],10)# "Potable"  
  n_mot <- convert_from_bin(sol[(n_pozos+33):(n_pozos+36)],10)# "Motobomba"
  n_ch <- convert_from_bin(sol[(n_pozos+37):(n_pozos+40)],10) #"Camion con Hidro" 
  n_pc <- convert_from_bin(sol[(n_pozos+41):(n_pozos+44)],10) #"Portacontenedor"
  out <- data.frame(n_rec=n_rec,
                    n_sup=n_sup,
                    n_me=n_me,
                    n_mm=n_mm,
                    n_mi=n_mi,
                    
                    n_as=n_as,
                    n_cv=n_cv,
                    n_pot=n_pot,
                    n_mot=n_mot,
                    n_ch=n_ch,
                    n_pc=n_pc)
  return(out)
}

get_modeled_services_costs <- function(sol,n_pozos) {
  out <- sum(get_ncuad_ntrans(sol,n_pozos)*rep(120,11))
  return(out)
}

get_productivities_from_function <- function(sol,n_pozos,prod_table, prod_table_log) {
  n_pozos_act <- sum(sol[1:n_pozos])
  cuad_trans <- get_ncuad_ntrans(sol,n_pozos) 
  productivity_list <- list()
  
  argument <- cuad_trans$n_rec
  A <-2.664
  B <- -0.03
  C <- 2.0179
  
  A1 <- 3.433683
  C1 <-  1.9983556
  B1 <- -0.0331080135 
  
  A2 <- 2
  C2 <-  2.2
  B2 <- -0.0231080135 
  
    #B*(1-(runif(1)*1^-1)*sample(c(1,-1),1))
  
  #services
  argument <- cuad_trans$n_rec
  productivity_list[['Recorredores']] <-  1/(exp(
    -(A1*(argument+3) + B2*n_pozos_act + C1))+1)  #3
  
  argument <- cuad_trans$n_sup  
  productivity_list[['Supervisores']] <-  1/(exp(
    -(A1*(argument+2) + B2*n_pozos_act + C1))+1)  #3
  
  argument <- cuad_trans$n_me
  productivity_list[['Mant. Electrico']] <- 1/(exp(
    -(A1*(argument+2) + B2*n_pozos_act + C1))+1)  #3
  
  argument <- cuad_trans$n_mm
  productivity_list[['Mant. Mecanico']] <-1/(exp(
    -(A1*(argument+4) + B2*n_pozos_act + C1))+1)  #3
    
  argument <- cuad_trans$n_mi
  productivity_list[['Mant. Instrumental']] <-1/(exp(
    -(A1*(argument+2) + B2*n_pozos_act + C1))+1)  #7
    
  argument <- cuad_trans$n_as
  productivity_list[['Articulado Semi']] <-  1/(exp(
    -(A*(argument+6) + B2*n_pozos_act + C))+1)   #3
 
  argument <- cuad_trans$n_cv 
  productivity_list[['Camion de Vacio']] <- 1/(exp(
    -(A1*(argument+4) + B2*n_pozos_act + C1))+1)
             
  
  argument <- cuad_trans$n_pot
  productivity_list[['Potable']] <-  1/(exp(
    -(A*(argument+5) + B2*n_pozos_act + C))+1)  #7
  
  argument <- cuad_trans$n_mot
  productivity_list[['Motobomba']] <- 1/(exp(
    -(A*(argument+7) + B2*n_pozos_act + C))+1)   #1
  
  argument <- cuad_trans$n_ch
  productivity_list[['Camion con Hidro']] <- 1/(exp(
    -(A*(argument+7) + B2*n_pozos_act + C))+1)   #1
  
  argument <- cuad_trans$n_pc
  productivity_list[['Portacontenedor']] <- 1/(exp(
    -(A*(argument+6) + B2*n_pozos_act + C))+1)   #3
  
  productivity_list[['total']] <- 1
  for (name in names(productivity_list)) {
    productivity_list[['total']] <- productivity_list[['total']] *  productivity_list[[name]] 
  }
  return(productivity_list)
}

get_productivities <- function(sol,n_pozos,prod_table, prod_table_log) {
  n_pozos_act <- sum(sol[1:n_pozos])
  cuad_trans <- get_ncuad_ntrans(sol,n_pozos) 
  productivity_list <- list()
  
    #services
    ids_rec <- prod_table[['Recorredores']]$num_cuad==cuad_trans$n_rec&
             prod_table[['Recorredores']]$low_n_pozo<=n_pozos_act&
            prod_table[['Recorredores']]$high_n_pozo>n_pozos_act
  
    productivity_list[['Recorredores']] <- prod_table[['Recorredores']]$prod[ids_rec]
  
    ids_sup <- prod_table[['Supervisores']]$num_cuad==cuad_trans$n_sup&
      prod_table[['Supervisores']]$low_n_pozo<=n_pozos_act&
      prod_table[['Supervisores']]$high_n_pozo>n_pozos_act
    
    productivity_list[['Supervisores']] <- prod_table[['Supervisores']]$prod[ids_sup]
    
    ids_me <- prod_table[['Mant. Electrico']]$num_cuad==cuad_trans$n_me&
      prod_table[['Mant. Electrico']]$low_n_pozo<=n_pozos_act&
      prod_table[['Mant. Electrico']]$high_n_pozo>n_pozos_act
    
    productivity_list[['Mant. Electrico']] <- prod_table[['Mant. Electrico']]$prod[ids_me]
  
    ids_mm <- prod_table[['Mant. Mecanico']]$num_cuad==cuad_trans$n_mm&
      prod_table[['Mant. Mecanico']]$low_n_pozo<=n_pozos_act&
      prod_table[['Mant. Mecanico']]$high_n_pozo>n_pozos_act
    
    productivity_list[['Mant. Mecanico']] <- prod_table[['Mant. Mecanico']]$prod[ids_mm]
    
    ids_mi <- prod_table[['Mant. Instrumental']]$num_cuad==cuad_trans$n_mi&
      prod_table[['Mant. Instrumental']]$low_n_pozo<=n_pozos_act&
      prod_table[['Mant. Instrumental']]$high_n_pozo>n_pozos_act
    
    productivity_list[['Mant. Instrumental']] <- prod_table[['Mant. Instrumental']]$
      prod[ids_mi]
    
    
    #transports
    #if dependency on activity level need to change
    productivity_list[['Articulado Semi']] <- prod_table_log[['Articulado Semi']][
      cuad_trans$n_as,'prod']
                         
    productivity_list[['Camion de Vacio']] <- prod_table_log[['Camion de Vacio']][
      cuad_trans$n_cv,'prod']                                         
    
    productivity_list[['Potable']] <- prod_table_log[['Potable']][
      cuad_trans$n_pot,'prod']    
    
    productivity_list[['Motobomba']] <- prod_table_log[['Motobomba']][
      cuad_trans$n_mot,'prod']   
    
    productivity_list[['Camion con Hidro']] <- prod_table_log[['Camion con Hidro']][
      cuad_trans$n_ch,'prod']   
    productivity_list[['Portacontenedor']] <- prod_table_log[['Portacontenedor']][
      cuad_trans$n_pc,'prod']   
    
    productivity_list[['total']] <- 1
    for (name in names(productivity_list)) {
      productivity_list[['total']] <- productivity_list[['total']] *  productivity_list[[name]] 
    }
    return(productivity_list)
}

get_services_costs <- function(sol,budget_table) {
  budget_costs <- budget_table$ET2017
  names(budget_costs) <-  budget_table$Apertura.OPEX
  return(budget_costs)
}

get_detailed_revenue <- function(sol,year) {
  net_production <- get_net_production(sol, year, net_production_table, instalations_table)
  total_productivity <- get_productivities_from_function(sol,n_pozos,prod_table, prod_table_log)$total
  total_budget_costs <- sum(get_services_costs(sol,budget_table))
  total_service_costs <- get_modeled_services_costs(sol,n_pozos)
  out <- data.frame(net_production=net_production,
                    total_productivity=total_productivity,
                    total_budget_costs=total_budget_costs,
                    total_service_costs=total_service_costs)
  return(out)
}

get_io <- function(sol, year) {
  dr <- get_detailed_revenue(sol,year)
  income <- dr$net_production*parameters$precio_crudo*dr$total_productivity
  outcome <- dr$total_budget_costs + dr$total_service_costs
  return(data.frame(income=income,outcome=outcome))
}

get_revenue <- function(sol,year) {
  dr <- get_io(sol,year)
  revenue <- dr$income - dr$outcome
  return(revenue)
}

get_metrics_sol_1year <- function(sol,year) {
  io <- get_io(sol, year)
  net_production <- get_net_production(sol, year, net_production_table, instalations_table)
  brute_production <- get_brute_production(sol, year, brute_production_table, instalations_table)
  perdidas <- 999
  energia <- 999
  cantidad_pullings <- 99
  personal <- 99
  
  out <- data.frame(income=io$income, outcome=io$outcome,investment=100,
             cash_flow=io$income-io$outcome-100,
             PB=brute_production,
             PN=net_production,
             perdidas=perdidas,
             energia=energia,
             cantidad_pullings=cantidad_pullings,
             personal=personal
             )

  return(out)
}

get_metrics_sol_1year_detail <- function(sol,year) {
  out <- unlist(c(sol[1:n_pozos],
           get_ncuad_ntrans(sol,n_pozos)))
  
  return(out)
}

save_xls_1year <- function(sol,year) {
  file_name <- 'data/out/revenue_simulation.xlsx'

  rev_table_ov <- read.xlsx (file_name,sheet = 1)
  
  rev_table_ov[1:10,year+1] <-as.numeric(get_metrics_sol_1year(sol,year))
  
  # write.xlsx (rev_table_ov,file_name,sheetName = 'Overview', row.names = F)
  
  wb <- loadWorkbook(file_name)
  writeData(wb, sheet = "Overview", rev_table_ov)
  saveWorkbook(wb,file_name,overwrite = T)
  
}

save_xls_1year_detail <- function(sol,year) {
  file_name <- 'data/out/revenue_simulation.xlsx'
  
  rev_table_ov <- read.xlsx (file_name,sheet = 2)
  
  rev_table_ov[1:(n_pozos+11),year+1] <-as.numeric(get_metrics_sol_1year_detail(sol,year))
  
  # write.xlsx (rev_table_ov,file_name,sheetName = 'Overview', row.names = F)
  
  wb <- loadWorkbook(file_name)
  writeData(wb, sheet = "Detail", rev_table_ov)
  saveWorkbook(wb,file_name,overwrite = T)
  
}

get_support_instalations<- function(sol, instalations_table, n_pozos) {
  sol_temp <- sol[1:n_pozos]
  sol_names_temp <- sol_names[1:n_pozos]
  active_pozos <- sol_names_temp[as.logical(sol_temp)]
  f1 <- instalations_table[active_pozos,'Padre']
  f2 <- instalations_table[f1,'Padre']
  fathers_bag <- tabulate(as.factor(c(f1,f2)))
  fathers_names <- levels(as.factor(c(f1,f2)))
  
  #inyectores
  i1 <- instalations_table[active_pozos, 'Pozos.Inyectores.Asociados']
  i1 <- i1[!is.na(i1)]
  i1 <- unlist(strsplit(i1,";"))
  inj_bag <- tabulate(as.factor(i1))
  inj_names <-  levels(as.factor(i1))
  inj_names <- gsub('-','',inj_names)
  inj_names <- inj_names(inj_names %in% out_names)
  
  #buld the output
  out_names <- instalations_table$`Identificador.Sin.-`[
    instalations_table$Tipo.de.instalación!='Petrolífero']
 
  #remove the na 
  inj_bag <- inj_bag[inj_names %in% out_names]
  inj_names <- inj_names[inj_names %in% out_names]

  fathers_bag <- fathers_bag[fathers_names %in% out_names]  
  fathers_names <- fathers_names[fathers_names %in% out_names]

  
  out_df <- data.frame (row.names = out_names, 
                        cant_inst=rep(0,length(out_names)))
  out_df [fathers_names,'cant_inst']<- fathers_bag
  out_df [inj_names,'cant_inst']<- inj_bag
  
}
