generates_log_stack <- function (log_table,bu_log_table) {
  
  stack <- data.frame(grupo=character(),
                      prioridad=numeric(),
                      type_number=numeric(),
                      tarea=character(),
                      unidad=character(),
                      demanda=numeric(),
                      day=numeric(),
                      loc=character(),
                      tiempo_total=numeric(),
                      demanda_open=numeric(),
                      comp_dia=numeric(),
                      cantidad_viajes=numeric(),
                      tiempo_total_viajes=numeric(),
                      respuesta=character(),
                      horaInicioMin=numeric(),
                      horaInicioMax=numeric(),
                      maxResponseDays=numeric(),
                      stringsAsFactors = FALSE)
                      
  for (grupo in bu_log_table$Grupo) {
    rep <- bu_log_table[grupo,'Cantidad.Anual']
    for (i in 1:rep) {
      log_table_temp <- log_table[log_table$Grupo==grupo,]
      
      if(grupo=='WO') {
        act <- sample(unique(log_table_temp$Actividad),1)
        log_table_temp <- log_table_temp[log_table_temp$Actividad==act,]
      }
      
      ini_day_group <- sample(1:332,1)
      stack_temp <-  data.frame(grupo=grupo,
                                prioridad=log_table_temp$Prioridad,
                                type_number=rownames(log_table_temp),
                                tarea=log_table_temp$Tarea,
                                unidad=log_table_temp$Tipo.Unidad,
                                demanda=log_table_temp$Demanda,
                                day=log_table_temp$Inicio. -1 + ini_day_group,
                                loc='not_defined',
                                tiempo_total=log_table_temp$Tiempo.Carga..Descarga/60,
                                demanda_open=log_table_temp$Demanda,
                                comp_dia=NA,
                                cantidad_viajes=NA,
                                tiempo_total_viajes=NA,
                                respuesta='',
                                horaInicioMin=log_table_temp$Inicio..1,
                                horaInicioMax=log_table_temp$Fin.1,
                                maxResponseDays=NA,
                                stringsAsFactors = FALSE)
      
      stack <- rbind.data.frame(stack,stack_temp )
    }
  }
  
  #loop among rows in bu_log_table

  
  return(stack)
}