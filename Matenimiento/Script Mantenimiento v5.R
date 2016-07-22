######################################
# Simulation Oil optimization SCRIPT #
######################################

#### 00. libraries and delete env ####
rm(list = ls())
require(xlsx)
require(gdata)
require(igraph)

#### 01. read files with tareas & parameters ####
#setwd("C:/Users/mferrari/Desktop/R - Demo - Mantenimiento")
tareas = read.xls ("Tareas Mantenimiento vDemo.xlsx", sheet = 3, header = FALSE)


years2eval <- 4
days1y <- 240
total_days <- years2eval*days1y

#clean up tareas
#keep columns 1, 3, 4, 10:14, 17
tareas <- tareas[-1,c(1,3,4,11:15,18)]

#rename columns
colnames(tareas) <- c('descrip','q1y','tiempo','perc_Planta_pet','perc_Planta_agua','perc_Planta_gas',
                      'perc_bat','perc_pozo','type')

#keep rows with description
tareas <- tareas [tareas$descrip!='',]

#convert q1y and tiempo into number
tareas$q1y <- as.numeric(as.character(tareas$q1y))
tareas$tiempo <- as.numeric(as.character(tareas$tiempo))
tareas$type <- as.character(tareas$type)

#define the order of the priority
target <- c('cor-alta','cor-media','pred-alta','prev-mant-alta','cor-baja','pred-baja','prev-mant-baja')

cor_id <- tareas$type %in% c('cor-alta','cor-media','cor-baja')
tareas_c <- tareas[cor_id,]
tareas_p <- tareas[!cor_id,]

#reset rownames
rownames(tareas_p) <- seq(length=nrow(tareas_p))

#### 02. read files with routes ####

rutas = read.xls ("Tareas Mantenimiento vDemo.xlsx", sheet = 8, header = TRUE)
puntos = read.xls ("Tareas Mantenimiento vDemo.xlsx", sheet = 9, header = TRUE)

#clean up
rutas <- rutas[,c(3:6,2)]
rutas$Ruta <- gsub('\xed','i',rutas$Ruta)
puntos$Activo <- gsub('\xed','i',puntos$Activo)

#keeps point Activos and with Category
colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')

#attention with the Sí instead of Si depending on how to read the text
puntos <- puntos[puntos$Activo=='Si'&puntos$Categoria!='Fuera'&puntos$Categoria!='#N/A',c(2,3,5,6)]

#all point in at least one route
punt <- c(as.character(rutas$origen),as.character(rutas$destino))
not.present <- !(puntos$Identif %in% punt)
#show those not present.
puntos$Identif[not.present]

#keep only those points present in at least one route
puntos <- puntos[!not.present,]

#rownames puntos
rownames(puntos) <- puntos$Identif

#### 03. intitialize list of nodes: pozos, baterias and plantas ####
pozos <- as.character(puntos$Identif)

plantas <- c('Planta Norte', 'Planta La Petiza')

#replace 42-S by 42S
baterias <- gsub("-","",unique(puntos$Colector))

#remove blanks
baterias <- baterias [ baterias !=""]

#remove not present
not.present <- !(baterias %in% punt)
baterias <- baterias[!not.present]

nodes <- c(pozos, plantas, baterias)

#### 04. explore graph ####
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)
comp <- components(g)

#remove not connected points in the graph
puntos.not_connect <- names(comp$membership[comp$membership!=1])
rutas <- rutas[!(rutas$origen %in% puntos.not_connect),]
rutas[!(rutas$destino %in% puntos.not_connect),]

#declare again the graph
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)

#just in case remove not connected
pozos <- pozos[!(pozos %in% puntos.not_connect)]
baterias <- baterias[!(baterias %in% puntos.not_connect)]

nodes <- c(pozos, plantas, baterias)

#measure the distance between Base and each node
# calculate all shortest paths from base
sp <- shortest.paths(g, 1,
                     weights = rutas$tiempo)

#convert sp as days. round trip
sp <- sp/60/60*2

#### 05. initialize the calendar of manteniance task predictive and preventive ####
#create a stack of activities predictive and preventive
stack_p.rows <- sum(tareas_p$q1y)*years2eval

#this stack object has these features
stack_p <- data.frame(task_id = numeric(stack_p.rows), location = character(stack_p.rows), 
                      tiempo= numeric(stack_p.rows),priority = character(stack_p.rows), 
                   day = numeric(stack_p.rows),stringsAsFactors=FALSE)

#initialize stack_p
k <- 1

for (y in 1:years2eval) {
  for (i in 1:nrow(tareas_p)) {
    for (j in 1:tareas_p[i,'q1y']) {
      probab <- tareas_p[i, c('perc_Planta_pet','perc_Planta_agua', 'perc_Planta_gas',
                          'perc_bat', 'perc_pozo')]
      loc <- sample(c('pet','agua','gas','bat','pozo'),1, prob = probab)
      stack_p[k,] <- c(i,loc,tareas_p[i,'tiempo'],tareas_p[i,'type'],sample(1:total_days,1))
      k <- k+1
    }
  }
}

#convert to numeric the columns
stack_p$task_id <- as.numeric(stack_p$task_id)
stack_p$tiempo <- as.numeric(stack_p$tiempo)
stack_p$day <- as.numeric(stack_p$day)

#### 06. incorporate specific place and transport time ####
spec_loc <- rep(NA, nrow(stack_p))

spec_loc[stack_p$location =='pet'] <- 'Planta La Petiza'

#revisar 
spec_loc[stack_p$location =='gas'] <- 'Planta Norte'

#revisar 
spec_loc[stack_p$location =='agua'] <- 'Planta Norte'

#pozos and baterias are assigned randomply
spec_loc[stack_p$location =='pozo'] <- sample(pozos, sum(stack_p$location=='pozo'),TRUE)

#pozos and baterias are assigned randomply
spec_loc[stack_p$location =='bat'] <- sample(baterias, sum(stack_p$location=='bat'),TRUE)

#calculate trans time
stack_p$spec_loc <- spec_loc
stack_p$transport_time <- sp[,spec_loc]

stack_p$total_time <- stack_p$tiempo + stack_p$transport_time

#### 07. initialize the calendar of mantenience task corrective ####
stack_c.rows <- sum(tareas_c$q1y)*years2eval

#this stack object has these features
stack_c <- data.frame(task_id = numeric(stack_c.rows), location = character(stack_c.rows), 
                      tiempo= numeric(stack_c.rows),priority = character(stack_c.rows), 
                      day = numeric(stack_c.rows),stringsAsFactors=FALSE)


k <- 1

for (y in 1:years2eval) {
  for (i in 1:nrow(tareas_c)) {
    for (j in 1:tareas_c[i,'q1y']) {
      probab <- tareas_c[i, c('perc_Planta_pet','perc_Planta_agua', 'perc_Planta_gas',
                              'perc_bat', 'perc_pozo')]
      loc <- sample(c('pet','agua','gas','bat','pozo'),1, prob = probab)
      stack_c[k,] <- c(i,loc,tareas_c[i,'tiempo'],tareas_c[i,'type'],runif(1,0 + 17/24,total_days + 17/24))
      k <- k+1
    }
  }
}

#convert to numeric
stack_c$task_id <- as.numeric(stack_c$task_id)
stack_c$tiempo <- as.numeric(stack_c$tiempo)
stack_c$day <- as.numeric(stack_c$day)

#### 08. incorporate specific place and transport time ####
spec_loc <- rep(NA, nrow(stack_c))

spec_loc[stack_c$location =='pet'] <- 'Planta La Petiza'

#revisar 
spec_loc[stack_c$location =='gas'] <- 'Planta Norte'

#revisar 
spec_loc[stack_c$location =='agua'] <- 'Planta Norte'

#pozos and baterias are assigned randomply
spec_loc[stack_c$location =='pozo'] <- sample(pozos, sum(stack_c$location=='pozo'),TRUE)

#pozos and baterias are assigned randomply
spec_loc[stack_c$location =='bat'] <- sample(baterias, sum(stack_c$location=='bat'),TRUE)

#calculate trans time
stack_c$spec_loc <- spec_loc
stack_c$transport_time <- sp[,spec_loc]

stack_c$total_time <- stack_c$tiempo + stack_c$transport_time

#### 09. create a summary of tasks to complete ####
total_task <- rep(NA,length(target))
names(total_task) <- target

#sum stack_c
for (prio in target) {
  total_task[prio] <- sum(stack_c$priority==prio) +sum(stack_p$priority==prio)
}

#measure the total work load
total_work_load <- sum(stack_c$total_time) + sum(stack_p$total_time)
total_work_load_py <- total_work_load/years2eval

#### 10. function create daily record ####
create_daily_record <- function(stack_p,stack_c,stack_reprog, day,num_cuad) {
  
  #create a matrix with cuadrillas
  cuad_record <- data.frame(cuad_id = 1:num_cuad, 
                            day_time2free= day + 8/24,lunch='no',end_day=FALSE, stringsAsFactors=FALSE)
  
  end_day <- function(cuad_record) {
    cuad_record$lunch[cuad_record$day_time2free < ini_lunch] <<- 'yes'
    cuad_record$end_day <<- TRUE
  }
  
  assign_task <- function(daily_stack,cuad_record,task_id,cuad_id,ini_cuad) {
    if (ini_cuad) {
      ini_task <- cuad_record[cuad_id,'day_time2free']
    } else {
      ini_task <- daily_stack[task_id,'day']
    }
    #update times
    daily_stack[task_id,'ini'] <<- ini_task
    fin_temp <- ini_task + daily_stack[task_id,'total_time']/24
    daily_stack[task_id,'fin'] <<- fin_temp
    cuad_record[cuad_id,'day_time2free'] <<- fin_temp
    
    #update cuad
    daily_stack[task_id,'cuad'] <<- cuad_record[cuad_id,'cuad_id']
    
    #update cuad record lunch and end_day
    #if no lunch and fin_temp>lunch_ini ->> END DAY=T
    #if fin_temp> fin time ->>END DAY=T
    one_cond <- cuad_record[cuad_id,'lunch']=='no' & fin_temp>ini_lunch
    two_cond <- fin_temp> fin_time

    if (one_cond | two_cond) {
      cuad_record[cuad_id,'end_day']<<- TRUE 
    }
    
  }
  
  #get pred and corr tasks
  daily_p <- stack_p[stack_p$day == day,]
  daily_c <- stack_c[stack_c$day > day+17/24-1 & stack_c$day< (day +17/24),]
  
  #create a list of activities to be completed
  daily_stack <- rbind(daily_c,daily_p)
  daily_stack <- rbind(daily_stack,stack_reprog)
  
  #sort the daily stack by prority target
  daily_stack$order1 <- match(daily_stack$priority,target)
  daily_stack <- daily_stack[order(daily_stack$order1,daily_stack$day,daily_stack$total_time),] 

  #add columns to daily_stack
  daily_stack$ini <- NA
  daily_stack$fin <- NA
  daily_stack$cuad <- NA
  
  #corr alta y media before 8 am.  the day ends at 16hs
  ini_time <- day + 8/24
  ini_lunch <- day + 12/24
  fin_lunch <- day + 13/24
  fin_time <- day + 17/24
  continue_day = TRUE
  
  while (continue_day) { 
    #exist a cuad to assign?
    if (sum(!cuad_record$end_day)>0) {
      #get the minimum time that has not ended the day
      cuad_id <- cuad_record$cuad_id[!cuad_record$end_day][which.min(
                  cuad_record$day_time2free[!cuad_record$end_day])] 
      
      #get the free time of that cuad
      free_daytime <- cuad_record[cuad_id,2]
      
      #get possible task id to complete
      tasks2assign.id <- is.na(daily_stack$ini)
      corr_tasks2assign.id <- daily_stack$order1 %in% c(1,2)& tasks2assign.id
      pre_tast2assign.id <- !(daily_stack$order1 %in% c(1,2))& tasks2assign.id
      
      #there exist a corr task? -> assign
      
      #is there a CORRECTIVE task (prior alta y media, not baja) in the stack that show up?
      corr_task2a_pos.id <- corr_tasks2assign.id & (daily_stack$day <= free_daytime)
      if (sum(corr_task2a_pos.id)>0){
        #get the first roww and assign
        task_id <- min(which(corr_task2a_pos.id == TRUE))
        assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE)
        
      } else { #no more corrective so far
        
        #is there a PREDICTIVE task in the stack? #
        pre_task2a_pos.id <- pre_tast2assign.id & (daily_stack$day < free_daytime)
    
        if (sum(pre_task2a_pos.id) <= 0){ 
          #no more predictive
          
          #is there a corrective to show up during the day?
          if(sum(corr_tasks2assign.id)<=0) {
            #no more task to assign. Not corrective not predictive
            #END DAY
            continue_day <- FALSE
            end_day(cuad_record)
            
          } else {
            #there are corrective to show up during the day
            task_id <- min(which(corr_tasks2assign.id == TRUE))
            assign_task(daily_stack,cuad_record,task_id,cuad_id,FALSE)
          }
        
        } else {
          #there are predictive tasks to assign
          
          #is it the begining of the day?
          if(free_daytime==day + 8/24) {
            
            #assign the predictive task    
            task_id <- min(which(pre_task2a_pos.id == TRUE))
            assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE)
            
          } else {
            #middle of the day
            
            #already had lunch?
            if(cuad_record[cuad_id,'lunch']!='yes') {
              #no lunch yet
              
              #is there a pred task that finishes before ini_lunch? 
              pre_task2a_pos.id <- pre_tast2assign.id & 
                                    (daily_stack$total_time/24 + free_daytime <= ini_lunch) &
                                    (daily_stack$day < free_daytime)
              
              if (sum(pre_task2a_pos.id)>0) {
                #get the task and assign
                task_id <- min(which(pre_task2a_pos.id == TRUE))
                assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE) 
              } else {
                #cuad must to have lunch
                cuad_record[cuad_id,'lunch'] <- 'yes'
                cuad_record[cuad_id,'day_time2free'] <- fin_lunch
              }
            } else {
              #already had lunch.
              #is there a pred task that finishes before fin_time? 
              pre_task2a_pos.id <- pre_tast2assign.id & 
                (daily_stack$total_time/24 + free_daytime <= fin_time) &
                (daily_stack$day < free_daytime)
              
              if (sum(pre_task2a_pos.id)>0) {
                #get the task and assign
                task_id <- min(which(pre_task2a_pos.id == TRUE))
                assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE) 
              } else {
                
                #no more pred task to assign. Is there any corrective???
                #is there a corrective to show up during the day?
                if(sum(corr_tasks2assign.id)<=0) {
                  #no more task to assign. Not corrective not predictive
                  #END DAY
                  continue_day <- FALSE
                  end_day(cuad_record)
                  
                } else {
                  #there are corrective to show up during the day
                  task_id <- min(which(corr_tasks2assign.id == TRUE))
                  assign_task(daily_stack,cuad_record,task_id,cuad_id,FALSE)
                }
              }
            }
          }
        }
      }
    } else {
      continue_day = FALSE
    }
  }
  
  reprog_id <- is.na(daily_stack$ini)
  daily_stack_reprog <- daily_stack[reprog_id,1:8]
  daily_record <- daily_stack[!reprog_id,]
  return (list(daily_record, daily_stack_reprog))
}

#### 11. call the function to generate the record ####

num_cuad_loop <- seq(2,12,2)
out <- NULL

for (num_cuad in num_cuad_loop) {
  print(c('num_cuad: ',num_cuad))
    
  stack_reprog <- NULL
  
  result_stack <- NULL
  for (day in 1:total_days) {
    #print(day)
    daily_stack <- create_daily_record(stack_p,stack_c,stack_reprog, day,num_cuad)
    stack_reprog <- daily_stack[[2]]
    result_stack <- rbind(result_stack,daily_stack[[1]])
  }
  
  #### 08. define metrics
  # 0. task at the end 
  #define NA with names
  perc_not_completed <- rep(NA,length(target))
  names(perc_not_completed) <- target

  #loop among target
  for (prio in target) {
    perc_not_completed[prio] <- sum(stack_reprog$priority==prio)/total_task[prio]
  }
  
  #assign the same order as target
  temp_line <- perc_not_completed[target]
  
  #change the names
  names(temp_line) <- paste('perc_not_comp', target,sep="-")
  
  #append the line
  line <- c(num_cuad=num_cuad, temp_line)

  # 1. % tareas reprogramadas
  result_stack$reprog <- (result_stack$ini - result_stack$day)>1 +0
  perc_reprog <- rep(NA,length(target))
  names(perc_reprog) <- target
  
  temp_line <- tapply(result_stack$reprog, result_stack$priority, mean)
  #loop among target
  for (prio in target) {
    perc_reprog[prio] <- temp_line[prio]
  }
  
  #assign the same order as target
  temp_line <- perc_reprog[target]
  
  #change the names
  names(temp_line) <- paste('perc_reprog', target,sep="-")
  
  #add the line
  line <- c(line, temp_line)
  
  # 2. tiempo de instalacion parado
  dif <-  result_stack$ini -result_stack$day 
  ids_corr <- result_stack$priority %in% target[1:2]
  temp_df <- data.frame(dif=dif[ids_corr],priority=result_stack$priority[ids_corr],
                        location=result_stack$location[ids_corr])
  #concatenate prior and loc
  temp_df$prio_loc <- paste (temp_df$priority,temp_df$location,sep='_')
  temp_df$prio_loc <- paste ('dpy_', temp_df$prio_loc,sep='')
  
  temp_line <- tapply(temp_df$dif,temp_df$prio_loc, sum)
  
  loc_prio_names <- c('dpy_cor-alta_agua','dpy_cor-alta_bat','dpy_cor-alta_gas','dpy_cor-alta_pet',
                         'dpy_cor-alta_pozo',
                         'dpy_cor-media_agua','dpy_cor-media_bat','dpy_cor-media_gas','dpy_cor-media_pet',
                         'dpy_cor-media_pozo')
  
  dpy_line <- rep(NA, length(loc_prio_names))
  names(dpy_line)<- loc_prio_names
  
  for (loc_prio in loc_prio_names) {
    dpy_line[loc_prio] <- temp_line[loc_prio]
  }
  
  line <- c(line,dpy_line)
  
  # 3. overtime and free time
  hour_fin <- result_stack$fin - floor(result_stack$fin)
  overtime_py <- sum(hour_fin[hour_fin> 17/24]-17/24)*24/years2eval
  
  total_ava_time <- num_cuad*years2eval*days1y*8
  total_ava_time_py <- num_cuad*days1y*8
  
  #task not completed
  task_not_completed_time <- sum(stack_reprog$total_time)
  task_not_completed_time_py <- task_not_completed_time/years2eval
  
  #  total work load - task not completed = overtime + ontime
  # ontime = ava_time - freetime
  # --->
  ontime_py <- total_work_load_py - task_not_completed_time_py - overtime_py
  freetime_py <- total_ava_time_py - ontime_py
  
  temp_line <- t(c(total_work_load_py,total_ava_time_py,overtime_py,task_not_completed_time_py,
                 ontime_py,freetime_py))
  names(temp_line) <- c('total_work_load_py','total_ava_time_py','overtime_py',
                    'task_not_completed_time_py','ontime_py','freetime_py')
  
  line <- c(line, temp_line)
  
  if (is.null(out)) {
    out <- line
  } else {
    out <- rbind(out,line)
  }

} # end loop section 5

#### 12. export ####
#export data.out to a excel

rownames(out)<-NULL

#append output to "Modelo de sensibilidad vR.xlsm" in "Output Data" sheet
wb <- loadWorkbook("Tareas Mantenimiento vDemo.xlsx")
sheets <- getSheets(wb)

removeSheet(wb, sheetName="Output Data")
newSheet <- createSheet(wb, sheetName="Output Data")

addDataFrame(as.data.frame(out), newSheet)
saveWorkbook(wb,"Tareas Mantenimiento vDemo.xlsx")


#### 10. to do
#include transport in the model
#include 1 hr lunch