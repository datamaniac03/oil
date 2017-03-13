build_production_table <- function (instalations_table,declination_table) {
  acum_declination_table <- declination_table
  for (col in 2:ncol(declination_table)) {
    acum_declination_table[,col] <- acum_declination_table[,col-1] *(
       1+ acum_declination_table[,col])
  }
  temp <- data.frame(row.names=instalations_table[,'Identificador.Sin.-'],
                     prod_neta=instalations_table$Producción.Neta.Real )
  prod_vect <- data.frame(row.names= rownames(declination_table),
               prod_neta=temp[ rownames(declination_table),])
  prod_mat <- replicate(10,as.vector(prod_vect))
  production_table <- prod_mat*acum_declination_table
  return(production_table)
}

build_production_table_br <- function (instalations_table,declination_table) {
  acum_declination_table <- declination_table
  for (col in 2:ncol(declination_table)) {
    acum_declination_table[,col] <- acum_declination_table[,col-1] *(
      1+ acum_declination_table[,col])
  }
  temp <- data.frame(row.names=instalations_table[,'Identificador.Sin.-'],
                     prod_neta=instalations_table$Producción.Bruta.Real )
  prod_vect <- data.frame(row.names= rownames(declination_table),
                          prod_neta=temp[ rownames(declination_table),])
  prod_mat <- replicate(10,as.vector(prod_vect))
  production_table <- prod_mat*acum_declination_table
  return(production_table)
}


deactivate_by_father <- function(instalations_table) {
  ids_2_deactive <-
   !(instalations_table$Padre %in% rownames(instalations_table))
  instalations_table$Activa[ids_2_deactive] <- 'padre_NA'
  
  return(instalations_table)
}

pozos_to_WO <- function(instalations_table) {
  ordered_ids <- order(instalations_table$Diferencia.m3)
  out_list <- list()
  for (i in 1:10) {
    out_list[[i]] <- rownames(instalations_table)[ordered_ids[(i*10-9):(i*10)]]
  }
  return(out_list)
}

affect_declination_wWO <- function(declination_table,
                             WO_data,
                             instalations_table) {
  pozos_to_WO_list <-  pozos_to_WO(instalations_table)
  
  for (i in 1:10) {
    dec <- WO_data$WO_declination
    if (i ==1) {
      dec[1] <- WO_data$WO_declination[1] + 1
    }
    rows <- runif(10)<WO_data$p_success
    
    mat <- t(replicate(10,as.numeric( dec[1:(10-i+1)])))
    if(nrow(mat)==1) {
      mat <- t(mat)
    }
    declination_table[pozos_to_WO_list[[i]][rows],i:10] <-  mat[rows,]
    
  }
  return(declination_table)
  
}