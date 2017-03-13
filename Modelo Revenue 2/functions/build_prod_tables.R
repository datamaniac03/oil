build_prod_table <- function(service) {
  out_table <- data.frame(low_n_pozo=numeric(),
                          lev_n_pozo=numeric(),
                          high_n_pozo=numeric(),
                          num_cuad=numeric(),
                          prod=numeric())
  
  lev <- c(50,150,250,350,450,550) 
  for (n_pozos in lev) {

    activity_level <- data.frame(PozoBM=n_pozos,
                                 PozoBES=round(90*n_pozos/385,0),
                                 PozoIny=round(115*n_pozos/385,0),
                                 Bateria=round(37*n_pozos/385,0), 
                                 Satelite=round(36*n_pozos/385,0),
                                 Pl_Petroleo=2,
                                 Pl_Gas=1,
                                 Pl_Agua=3, 
                                 Subestacion=round(37*n_pozos/385,0),
                                 Gasoducto=round(37*n_pozos/385,0),
                                 Motogen= 4,
                                 Oleoducto=1,
                                 Colector=round(42*n_pozos/385,0),
                                 Oficina=1)    
    
  
    for (num_cuad in 1:12) {
      
      out <- get_service_productivity(service,rsm_table,activity_level,turnos,num_cuad,total_days,TRUE)
      
      #just to save the metrics in the file
      get_service_metrics(service,rsm_table,activity_level,turnos,num_cuad,total_days,TRUE)
      print(num_cuad)
      
      prod <- as.numeric(out['productivity.dpy_corr_Pozo.BM'])
      out_table <- rbind.data.frame(out_table, c(n_pozos-50,n_pozos,n_pozos+50,num_cuad,prod))
      
    }
  }
  names(out_table) <- c('low_n_pozo',
                        'lev_n_pozo',
                        'high_n_pozo',
                        'num_cuad',
                        'prod')
  return(out_table)
}

generates_prod_tables <- function (services) {
  prod_table <- list()
  for (service in services) {
    prod_table[[service]] <- build_prod_table(service)
  }

  save(prod_table, file = "prod_table.RData")
  return(prod_table)
}

save_prod_table <- function(prod_table) {
  for (service in names(prod_table)) {
    file_name <- paste('data/out/productivity_tables/prod_',service,'.xlsx')
    write.xlsx2(prod_table[[service]],file_name,
               sheetName = service)
    
  }
}