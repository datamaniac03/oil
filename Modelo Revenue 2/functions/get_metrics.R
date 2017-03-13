get_metrics <- function(service_stack, service_record, num_cuad) {
  # 0. task at the end 
  
  #target_out_sorted <- levels(as.factor(service_stack$priority_num))
  target_out_sorted <- c('1','2','3','9')
  perc_not_completed <- rep(NA,length(target_out_sorted))
  names(perc_not_completed) <- target_out_sorted
  
  for (prio in target_out_sorted) {
    perc_not_completed[prio] <- tapply(!service_stack$completed,service_stack$priority,FUN =mean)[prio]
  }
  temp_line <- perc_not_completed[target_out_sorted]

  names(temp_line) <- paste('perc_not_comp', target_out_sorted,sep="-")
  
  line <- c(num_cuad=num_cuad, temp_line)
  
  # 1. % tareas reprogramadas
  service_stack$reprog <- (- service_stack$day + service_stack$end_task_time)>1 +0
  perc_reprog <- tapply(service_stack$reprog[service_stack$completed], 
                        service_stack$priority[service_stack$completed], mean)
  
  temp_line <- perc_reprog[target_out_sorted]
  names(temp_line) <- paste('perc_reprog', target_out_sorted,sep="-")
  
  line <- c(line, temp_line)
  
  # 2. tiempo de instalacion parado
  dif <-  service_stack$end_task_time - service_stack$day 
  ids_corr <- service_stack$priority %in% c(1,2) & !is.na(service_stack$end_task_time)
  temp_df <- data.frame(dif=dif[ids_corr],priority=service_stack$priority[ids_corr],
                        location=service_stack$location[ids_corr])
  
  
  #Concatenate prior and loc
  temp_df$prio_loc <- paste (temp_df$priority,temp_df$location,sep='_')
  temp_df$prio_loc <- paste ('dpy_', temp_df$prio_loc,sep='')
  
  temp_line <- tapply(temp_df$dif,temp_df$prio_loc, sum)
  
  #loc_prio_names <- levels(as.factor(temp_df$prio_loc))
  loc_prio_names <- 'dpy_1_Pozo.BM'
  
   #c( 'dpy_cor-alta_pozo','dpy_cor-media_pozo')
  
  dpy_line <- rep(NA, length(loc_prio_names))
  names(dpy_line)<- loc_prio_names
  
  for (loc_prio in loc_prio_names) {
    dpy_line[loc_prio] <- temp_line[loc_prio]
  }
  
  line <- c(line,dpy_line)
  
  
  #metrics to build prod table 
  ids_2_consider <- service_stack$corrective&service_stack$location=='Pozo.BM'
  
  perc_completed_corr_Pozo.BM <- mean(service_stack$completed[ids_2_consider])
  
  temp <- service_stack$end_task_time[ids_2_consider] 
  
  temp[is.na(temp)]<- 366
  
  dif <-  temp - service_stack$day[ids_2_consider] 
  
  dpy_corr_Pozo.BM <- sum(dif)
  
  num_cuad <- max(service_record$cuad_id)
  add_line <- c(perc_completed_corr_Pozo.BM=perc_completed_corr_Pozo.BM,
            dpy_corr_Pozo.BM=dpy_corr_Pozo.BM)
  line <- c(line,add_line)
  
  # 3. overtime and free time
  years2eval <-1
  labor_days <- 261
  overtime_py <- sum(record$worked_ot_hours)
  
  total_ava_time <- num_cuad*years2eval*labor_days*8
  total_ava_time_py <- num_cuad*labor_days*8
  
  #task not completed
  task_not_completed_time <- sum(service_stack$total_time[!service_stack$completed])
  task_not_completed_time_py <- task_not_completed_time/years2eval
  
  #  total work load - task not completed = overtime + ontime
  # ontime = ava_time - freetime
  # --->
  
  total_work_load <- sum(service_stack$total_time)
  total_work_load_py <- total_work_load/years2eval
  
  task_completed_time_py <- total_work_load_py - task_not_completed_time_py
  
  #ontime_py <- total_work_load_py - task_not_completed_time_py - overtime_py
  ontime_py <- task_completed_time_py - overtime_py
  
  freetime_py <- total_ava_time_py - ontime_py
  
  temp_line <- t(c(total_work_load_py,task_completed_time_py,task_not_completed_time_py,
                   total_ava_time_py,overtime_py,ontime_py,freetime_py))
  names(temp_line) <- c('total_work_load_py','task_completed_time_py','task_not_completed_time_py',
                        'total_ava_time_py','overtime_py','ontime_py','freetime_py')
  
  metric_line <- c(line, temp_line)
  
  return(metric_line)
  
  
}

# get_metrics_2_build_table <- function(service_stack, service_record) {
#   
#   ids_2_consider <- service_stack$corrective&service_stack$location=='Pozo.BM'
#   
#   perc_completed_corr_Pozo.BM <- mean(service_stack$completed[ids_2_consider])
#   
#   temp <- service_stack$end_task_time[ids_2_consider] 
#   
#   temp[is.na(temp)]<- 366
#   
#   dif <-  temp - service_stack$day[ids_2_consider] 
#   
#   dpy_corr_Pozo.BM <- sum(dif)
#   
#   num_cuad <- max(service_record$cuad_id)
#   line <- c(num_cuad=num_cuad, 
#             perc_completed_corr_Pozo.BM=perc_completed_corr_Pozo.BM,
#             dpy_corr_Pozo.BM=dpy_corr_Pozo.BM)
#   
#   return(line)
#   
# }

get_service_productivity <- function(service,rsm_table,activity_level,turnos,num_cuad,total_days,save_file) {
#get_service_productivity <- function(service_stack,activity_level,turnos,num_cuad,total_days,save_file) {  
  
  #total days pozo.BM
  total_days_pozo.BM <- activity_level$PozoBM*total_days

  #turnos ignored
  #### 3. create tasks_stack ####
  
  tasks_stack <<- create_tasks_stack(rsm_table, activity_level)
  
  #### 4. assign tasks_stack ####
  
  service_stack <<- tasks_stack[[service]] 
  
  service_record <<- assign_stack(service_stack,turnos,num_cuad,total_days)
  
  out_line_temp <- get_metrics(service_stack,service_record,num_cuad)
  out_line <- out_line_temp[c(  #'num_cuad',
                              'perc_completed_corr_Pozo.BM',            
                              'dpy_corr_Pozo.BM')]
  
  #productivity 

  productivity <- 1-out_line['dpy_corr_Pozo.BM']/total_days_pozo.BM
  
  out_line <- c(service=service,num_cuad, out_line, productivity=productivity)
  
  if(save_file) {
    filename <- paste('data/service_productivity_',service,'.csv',sep='')
    # cat(names(out_line),file=filename,append=T,sep=',')
    # cat("\n",file=filename,append=T,sep='')
    cat(out_line,file=filename,append=T,sep=',')
    cat("\n",file=filename,append=T,sep='')
  }
  return(out_line)
} 

get_service_metrics<- function(service,rsm_table,activity_level,turnos,num_cuad,total_days,save_file) {
  
  #turnos ignored
  #### 3. create tasks_stack ####
  tasks_stack <<- create_tasks_stack(rsm_table, activity_level)
  
  #### 4. assign tasks_stack ####
  service_stack <<- tasks_stack[[service]] 
  
  service_record <<- assign_stack(service_stack,turnos,num_cuad,total_days)
  
  out_line <- get_metrics(service_stack,service_record,num_cuad)
  
  out_line <- c(service=service,out_line)
  
  if(save_file) {
    filename <- paste('data/service_metrics.csv')
    # cat(names(out_line),file=filename,append=T,sep=',')
    # cat("\n",file=filename,append=T,sep='')
    cat(out_line,file=filename,append=T,sep=',')
    cat("\n",file=filename,append=T,sep='')
  }
  return(out_line)
}