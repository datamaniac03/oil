#### 04. functions ####
get_open_task <- function(service_stack, day_time, corrective) {
  #this function can also be used to get the next open task by doing day_time = day +17/24
  if(corrective) {
    ids_corr <- service_stack$corrective
  } else {
    ids_corr <- !service_stack$corrective
  }
  
  relevant_ids <- service_stack$day<day_time & is.na(service_stack$cuad) & ids_corr
  id <-which(relevant_ids)[1]
  return (service_stack[id,])
}

#create cuad record
initialize_cuad_record <- function (day, num_cuad) {
  cuad_record <<- data.frame(cuad_id = 1:num_cuad, 
                            day_time2free= day + 8/24,lunch=FALSE,end_day=FALSE, ot=FALSE,stringsAsFactors=FALSE)
  return(cuad_record)
}

insert_row_record <- function (row) {
  record[nrow(record)+1,]<<-row
}

complete_task <- function(task,end_task) {
  task$comp_perc <- 1
  task$completed <- TRUE
  task$end_task_time <- end_task
  return(task)
}

assign_task_cuad <- function(stack_id,cuad_id,service_stack) {
  turnos <- 2 
  
  # 1-          Lunes a Viernes 9 hs
  # 2-          Lunes a Viernes 12 hs
  # 3-          Lunes a Domingos 12 hs
  
  #declare some values
  task <- service_stack[stack_id,]
  task$cuad <- cuad_id
  free_time <- cuad_record[cuad_id,'day_time2free']
  task_ini_time <- service_stack[stack_id,'day']
  duration <- task$tiempo*(1-task$comp_perc)
  day <- floor(free_time)
  if (turnos ==1) {
    lunch_ini_time <- day + 12/24
    lunch_fin_time <- day + 13/24
    day_fin_time <- day + 17/24
    day_fin_time_wot <- day + 21/24
  } else if (turnos ==2) {
    lunch_ini_time <- day + 12/24
    lunch_fin_time <- day + 13/24
    day_fin_time <- day + 20/24
    day_fin_time_wot <- day + 23.9/24
  }
  ini_time <- max(free_time,task_ini_time)
  lunch <- cuad_record[cuad_id,'lunch']
  end_task <- ini_time + (duration + task$transport_time)/24
  if (is.na(task$ini_task_time)) {
    task$ini_task_time  <- ini_time
  }
  
  if (!lunch) { #if not lunch
    if (end_task < lunch_ini_time) { #ends before lunch
      #update service_stack 
      #task$comp_perc <- 1
      #task$completed <- TRUE
      #task$end_task_time <- end_task
      
      #alternative way with function
      task <- complete_task(task,end_task)
      
      #update cuad_record
      cuad_record[cuad_id,'day_time2free'] <<-end_task
      #append_record
      insert_row_record(c(cuad_id,stack_id,day,duration,0))
      
    } 
    else { #ends after luch 
      #print('ends after lunch')
      
      end_task <- end_task + 1/24 + task$transport_time/24
      cuad_record[cuad_id,'lunch'] <<- TRUE
      
      if (end_task < day_fin_time) { #finishes before the end of the day
        
        task <- complete_task(task,end_task)
        
        #update cuad_record
        cuad_record[cuad_id,'day_time2free'] <<- end_task
        #append_record
        insert_row_record( c(cuad_id,stack_id,day,duration,0))
        
      } 
      else { #finishes after the end of the day.
        #print('finishes after the end of the day')
        #update cuad_record
        cuad_record[cuad_id,'end_day'] <<- TRUE
        posible_overtime <- sum(cuad_record$ot)==0 & task$corrective
        
        if (posible_overtime) { #the cuad works overtime
          #print('ot')
          cuad_record[cuad_id,'ot'] <<- TRUE
          
          if (end_task < day_fin_time_wot) { #finishes before the day_fin_time_wot
            #print('finishes before the end of the day wot')
            ot <- (end_task - day_fin_time)*24 
            
            #update service_stack
            task <- complete_task(task,end_task)
            
            #update cuad_record
            cuad_record[cuad_id,'day_time2free'] <<- end_task
            #append_record
            insert_row_record(c(cuad_id,stack_id,day,duration,ot))
            
          } else { #finishes after the day_fin_time_wot
            #print('finishes after the end of the day wot')
            
            ot <- 4
            worked_hours <- (day_fin_time_wot - 1/24 - 2*task$transport_time/24 - ini_time)*24
            if (worked_hours<0) {stop("worked_hours<0")}
            task$comp_perc <- (task$tiempo*task$comp_perc + worked_hours)/task$tiempo 
            cuad_record[cuad_id,'day_time2free'] <<- day_fin_time_wot
            #append_record
            insert_row_record(c(cuad_id,stack_id,day,worked_hours,4))
            
          }
          
        } else { #overtime not possible
          
          #print (c(day_fin_time , 1/24 , 2*task$transport_time/24 , ini_time))
          worked_hours <- (day_fin_time - 1/24 - 2*task$transport_time/24 - ini_time)*24
          if (worked_hours<0) {stop("worked_hours<0")}
          #print(worked_hours)
          
          task$comp_perc <- (task$tiempo*task$comp_perc + worked_hours)/task$tiempo 
          cuad_record[cuad_id,'day_time2free'] <<- day_fin_time
          #append_record
          insert_row_record(c(cuad_id,stack_id,day,worked_hours,0))
        }
      }
    }
  } 
  else { #already lunch
    
    if (end_task < day_fin_time) { #finishes before the end of the day
      #print('finishes before the end of the day')
      #update service_stack
      #alternative way with function
      task <- complete_task(task,end_task)
      
      #update cuad_record
      cuad_record[cuad_id,'day_time2free'] <<- end_task
      #append_record
      insert_row_record(c(cuad_id,stack_id,day,duration,0))
      
      
    } 
    else { #finishes after the end of the day.
      #print('finishes after the end of the day')
      #update cuad_record
      cuad_record[cuad_id,'end_day'] <<- TRUE
      
      posible_overtime <- sum(cuad_record$ot)==0 & task$corrective
      
      if (posible_overtime) { #the cuad works overtime
        #print('ot')
        cuad_record[cuad_id,'ot'] <<- TRUE
        
        if (end_task < day_fin_time_wot) { #finishes before the day_fin_time_wot
          #print('finishes before the end of the day wot')
          ot <- (end_task - day_fin_time)*24 
          
          #update service_stack
          #alternative way with function
          task <- complete_task(task,end_task)
          
          #update cuad_record
          cuad_record[cuad_id,'day_time2free'] <<- end_task
          #append_record
          insert_row_record(c(cuad_id,stack_id,day,duration,ot))
          
        } else { #finishes after the day_fin_time_wot
          #print('finishes after the end of the day wot')
          
          ot <- 4
          worked_hours <- (day_fin_time_wot - task$transport_time/24 - ini_time)*24
          if (worked_hours<0) {stop("worked_hours<0")}
          task$comp_perc <- (task$tiempo*task$comp_perc + worked_hours)/task$tiempo 
          cuad_record[cuad_id,'day_time2free'] <<- day_fin_time_wot
          #append_record
          insert_row_record( c(cuad_id,stack_id,day,worked_hours,4))
          
        }
        
      } else { #overtime not possible
        #print('ot not possible')
        
        worked_hours <- (day_fin_time - task$transport_time/24 - ini_time)*24
        if (worked_hours<0) {
          #print("worked_hours<0")
          cuad_record[cuad_id,'day_time2free'] <<- day_fin_time
          cuad_record[cuad_id,'end_day'] <<- TRUE
        } else {
          task$comp_perc <- (task$tiempo*task$comp_perc + worked_hours)/task$tiempo 
          cuad_record[cuad_id,'day_time2free'] <<- day_fin_time
          #append_record
          insert_row_record(c(cuad_id,stack_id,day,worked_hours,0))
        }
      }
    }
  }
  
  #print (task)
  service_stack[stack_id,] <- task
  return(service_stack)
}

get_available_cuad_id <- function (cuad_record) {
  #verify lunch time
  if (sum(!cuad_record$end_day)==0) { return(NULL) }
  index <- which.min(cuad_record$day_time2free[!cuad_record$end_day])
  cuad_id <- cuad_record$cuad_id[!cuad_record$end_day][index]
  
  time2free <- cuad_record$day_time2free[cuad_id]
  day <- floor(time2free)
  ini_lunch <- day + 12/24
  fin_lunch <- day +13/24
  time_to_lunch <- ini_lunch - time2free
  go_lunch <- !cuad_record$lunch[cuad_id] & time_to_lunch<1/24
  #print('go_lunch',cuad_id)
  if (go_lunch) {
    cuad_record[cuad_id,'lunch'] <<- TRUE
    cuad_record[cuad_id,'day_time2free'] <<- floor(cuad_record[cuad_id,'day_time2free'])+13/24
  }
  return(cuad_id)
}

day_record <- function(day,num_cuad) {
  turnos <-2
  if(turnos==1) {
    day_fin_time <- 20/24 
  } else if (turnos==2) {
    day_fin_time <- 20/24
  }
  #print('day_record')
  #print(day)
  
  cuad_record <<- initialize_cuad_record (day, num_cuad)
  #loop among incompleted tasks and assign
  incompleted_tasks_ids <- service_stack$comp_perc>0&!service_stack$completed
  
  for (row in which(incompleted_tasks_ids)) {
    #print('assigning incompleted tasks')
    service_stack <<- assign_task_cuad(row,service_stack[row,'cuad'],service_stack)
  }
  
  continue_day <- TRUE
  i<-1
  while(continue_day){
    #print(c('interation:',i))
    cuad_id <- get_available_cuad_id(cuad_record)
    
    if(length(cuad_id)==0) {
      #print('length(cuad_id)==0')
      continue_day<- FALSE
    } else {
      
      ot_logic <- sum(cuad_record$ot)>0
      if (ot_logic) {
        #verify_end_day()
      }
      cuad_id <- get_available_cuad_id(cuad_record)
      
      if (length(cuad_id)>0) {
        
        #get open corrective task
        task <- get_open_task (service_stack, cuad_record[cuad_id,'day_time2free'], TRUE)
        #print('corrective task:')
        #print(task)
        
        if(!is.na(task$task_id)) {
          #print('assigment:')
          #print(c(task$stack_id,cuad_id))
          service_stack <<- assign_task_cuad(task$stack_id,cuad_id,service_stack)
        } else { #not a corrective task
          #verify end of the day
          #verify_end_day()
          #print(cuad_id)
          #print('pred task:')
          
          task <- get_open_task (service_stack, cuad_record[cuad_id,'day_time2free'], FALSE)
          #print(task)
          
          
          if(!is.na(task$task_id)) {
            #print('assigment:')
            #print(c(task$stack_id,cuad_id))
            service_stack <<- assign_task_cuad(task$stack_id,cuad_id,service_stack)
          } else  { #verify a next corr stack
            
            task <- get_open_task (service_stack, day_fin_time, TRUE)
            #print('corrective task:')
            #print(task)
            
            if(!is.na(task$task_id)) {
              #print('no open tasks')
              #verify lunch of cuad vs task 
              go_lunch <- (task$day > day + 12/24) &!cuad_record[cuad_id,'lunch']
              
              #print(go_lunch)
              if (go_lunch) {
                #print('')
                cuad_record[cuad_id,'lunch'] <<- TRUE
                cuad_record[cuad_id,'day_time2free'] <<- floor (cuad_record[cuad_id,'day_time2free']) + 13/24
              } else { 
                
                #print('assigment:')
                #print(c(task$stack_id,cuad_id))
                service_stack <<- assign_task_cuad(task$stack_id,cuad_id,service_stack)
              }
            } else {
              #print('else')
              continue_day = FALSE
            }
          }
        }
      }
      i<- i+1
    }
    #print(cuad_record)
    #print(service_stack[1:10,])
  }
}
