
generates_stack <- function (service,rsm_table,task_repetitions) {
  temp_table <- rsm_table [rsm_table$Servicio==service,]
  temp_task_rep <-task_repetitions [rsm_table$Servicio==service]
  task_id <- 1  
  
  tot_rows <- sum(temp_task_rep)
  stack <- data.frame(task_id = numeric(tot_rows), location = character(tot_rows), 
                      tiempo= numeric(tot_rows),priority = character(tot_rows), 
                      day = numeric(tot_rows),comp_perc=as.numeric(0),cuad = NA,completed= FALSE,
                      corrective =F,ini_task_time =NA,end_task_time=NA,stringsAsFactors=FALSE)
  
  for (row in 1:nrow(temp_table)) {
    for (i in 1:temp_task_rep[row]) {
      #simulate location
      probs <- temp_table[row,10:23]
      
      
      set.seed(sum(utf8ToInt(service))+i+row)
      simulated_location <- sample(names(temp_table)[10:23],1,prob = probs)

      stack[task_id,] <- data.frame(task_id=task_id, location =as.character(simulated_location),
                                    tiempo= temp_table[row,'Duracion'] ,
                                    priority = temp_table[row,'Prioridad'], 
                                    day = runif(1, min = 1, max = 365),
                           comp_perc=as.numeric(0),cuad = NA,completed= FALSE,
                           corrective =temp_table[row,'Tipo']=='Correctiva',ini_task_time =NA,end_task_time=NA,
                           stringsAsFactors=FALSE)

      task_id <- task_id +1
    }
  }
  
  
  rownames(stack) <- NULL
  
  #sort the daily stack by prority target
  order1 <- stack$priority
  stack$priority_num <- order1
  stack <- stack[order(stack$priority_num,stack$day),] 
  
  stack$stack_id <- 1:nrow(stack)
  row.names(stack) <- stack$stack_id
  
  stack$transport_time <- as.numeric(0)
  stack$tiempo <- as.numeric(stack$tiempo)/60
  stack$spec_loc <- NA
  stack$total_time <- stack$tiempo
  stack$completed <- stack$completed==1
  stack$corrective <- stack$corrective==1
  stack$reprog <- NA
  stack$comp_perc <- as.numeric(stack$comp_perc)
  return(stack)
}

create_tasks_stack <- function (rsm_table,  activity_level) {

  task_repetitions <- round(as.matrix(rsm_table[,10:(ncol(rsm_table)-1)]) %*% 
                              as.matrix(t(activity_level)) * rsm_table$Frecuencia.x.Instalacion,0)
  
  services <- unique(rsm_table$Servicio)
  tasks_stack <- list()
  for (ser in services) {
    tasks_stack[[ser]] <- generates_stack(ser,rsm_table,task_repetitions)
  }
  return(tasks_stack)
}

assign_stack <- function(service_stack,turnos,num_cuad,total_days) {
  labor <- T
  
  source('functions/assignment.R')
  record <<- data.frame(cuad_id=integer(), 
                       stack_id=integer(),
                       day=integer(),
                       worked_reg_hours = double(),
                       worked_ot_hours = double(),     
                       stringsAsFactors=FALSE) 
  
  total_days_set <- 1:total_days
  labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
  
  if (labor) {
    labor_days_id <-  (1:total_days)[labor_days_bool]
  } else {
    labor_days_id <-  (1:total_days)
  }
  for (day in 1:total_days) {
    #for (day in 1:2) {
    #print(c('day:',day))
    #condition if labor day
    if (day %in% labor_days_id) {
      day_record(day,num_cuad)
    }
  }
  service_record <- record
  return(service_record)
}