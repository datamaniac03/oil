######################################
# Simulation Oil optimization SCRIPT #
######################################

#### 00. libraries and delete env ####
rm(list = ls())
cat("\f")
require(xlsx)
require(gdata)
require(igraph)

#### 01. read files with tareas & parameters ####
#parameters
years2eval <- 4
days1y <- 365

total_days <- years2eval*days1y
total_days_set <- 1:total_days
labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
labor_days_id <-  (1:total_days)[labor_days_bool]

#setwd("C:/Users/mferrari/Desktop/R - Demo - Mantenimiento")
tareas_m = read.xls ("Tareas Mantenimiento vDemo2.xlsx", sheet = 3, header = TRUE)
tareas_e = read.xls ("Tareas Mantenimiento vDemo2.xlsx", sheet = 2, header = TRUE)
tareas_i = read.xls ("Tareas Mantenimiento vDemo2.xlsx", sheet = 6, header = TRUE)

target <- c('cor-alta','cor-media','pred-alta','prev-mant-alta','cor-baja','pred-baja','prev-mant-baja')
target_out_sorted <- target[c(1:2,5,3,6,4,7)]


fix_format <- function(tareas_sheet) {
  tareas_out <- tareas_sheet[-1,c(1,2,3,6:14,17)]
  colnames(tareas_out) <- c('descrip','q1y','tiempo','perc_Planta_pet','perc_Planta_agua','perc_Planta_gas',
                          'perc_bat','perc_pozo','perc_sat','perc_subest','perc_gasoducto','perc_moto','type')
  #keep rows with description
  tareas_out <- tareas_out [tareas_out$descrip!='',]
  
  #convert q1y and tiempo into number
  tareas_out$q1y <- as.numeric(as.character(tareas_out$q1y))
  tareas_out$tiempo <- as.numeric(as.character(tareas_out$tiempo))
  tareas_out$type <- as.character(tareas_out$type)
  tareas_out <- tareas_out[tareas_out$type %in% target,]
  
  return(tareas_out)
}

tareas_m <- fix_format(tareas_m)
tareas_e <- fix_format(tareas_e)
tareas_i <- fix_format(tareas_i)

#### 02. read files with routes ####
rutas = read.xls ("Tareas Mantenimiento vDemo2.xlsx", sheet = 8, header = TRUE)
puntos = read.xls ("Tareas Mantenimiento vDemo2.xlsx", sheet = 9, header = TRUE)

#clean up
rutas <- rutas[,c(3:6,2)]
rutas$Ruta <- gsub('\xed','i',rutas$Ruta)
puntos$Activo <- gsub('\xed','i',puntos$Activo)

#keeps point Activos and with Category
colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')

#attention with the Sí instead of Si depending on how to read the text
puntos <- puntos[puntos$Activo=='Si',c(2,3,4,6)]

#all point in at least one route
punt <- c(as.character(rutas$origen),as.character(rutas$destino))
not.present <- !(puntos$Identif %in% punt)
#show those not present.
puntos$Identif[not.present]

#keep only those points present in at least one route
puntos <- puntos[!not.present,]

#rownames puntos
rownames(puntos) <- puntos$Identif

#### 03. explore graph ####
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)
comp <- components(g)

#remove not connected points in the graph
puntos.not_connect <- names(comp$membership[comp$membership!=1])
rutas <- rutas[!(rutas$origen %in% puntos.not_connect),]
rutas[!(rutas$destino %in% puntos.not_connect),]

#declare again the graph
g <- graph_from_edgelist(as.matrix(rutas[,1:2]),directed = FALSE)

#measure the distance between Base and each node
# calculate all shortest paths from base
sp <- shortest.paths(g,as.numeric(V(g)["Base"]),weights = rutas$tiempo)

#convert sp as days. round trip
sp <- sp/60/60*2

#just in case remove not connected
puntos <- puntos[!(rownames(puntos) %in% puntos.not_connect),]

#### 04. initialize list of pozos, baterias, satelites, plantas ####
pozos <- rownames(puntos[puntos$Tipo=='Pozo',])
baterias <-rownames(puntos[puntos$Tipo=='Bateria',])
satelites <-rownames(puntos[puntos$Tipo=='Satelite',])
plantas_pet <- c('Planta Central', 'Planta La Petiza')
plantas_agua <- c('Planta Sur', 'Planta Norte','Planta Oeste')
plantas_gas <- c('Planta Gas')
subestacion <- c('Subestacion')
gasoducto <- c('Gasoducto')
motogenerador <- c('Motogenerador')

#### 05. loop among type of tasks ####
tareas_loop <- list(tareas_e,tareas_m,tareas_i)
sheet_name_loop <- c('Out_tareas_electicas', 'Out_tareas_mecanicas','Out_tareas_insturmental')

#for (tar_id in 2) {
for (tar_id in 1:3) {
  tareas <- tareas_loop[[tar_id]]
  sheet_name <- sheet_name_loop[tar_id]
  print(sheet_name)
  
#### 06. intitialize list of nodes: pozos, baterias and plantas ####
cor_id <- tareas$type %in% c('cor-alta','cor-media','cor-baja')
tareas_c <- tareas[cor_id,]
tareas_p <- tareas[!cor_id,]

#reset rownames
rownames(tareas_p) <- seq(length=nrow(tareas_p))
rownames(tareas_c) <- seq(length=nrow(tareas_c))

#### 07. initialize the calendar of manteniance task predictive and preventive ####
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
    probab <- tareas_p[i, c('perc_Planta_pet','perc_Planta_agua', 'perc_Planta_gas',
                            'perc_bat', 'perc_pozo','perc_sat','perc_subest','perc_gasoducto','perc_moto')]
    for (j in 1:tareas_p[i,'q1y']) {
      
      loc <- sample(c('pet','agua','gas','bat','pozo','sat','subest','gasoducto','moto'),1, prob = probab)
      stack_p[k,] <- c(i,loc,tareas_p[i,'tiempo'],tareas_p[i,'type'],sample(labor_days_id,1))
      k <- k+1
    }
  }
}

#convert to numeric the columns
stack_p$task_id <- as.numeric(stack_p$task_id)
stack_p$tiempo <- as.numeric(stack_p$tiempo)
stack_p$day <- as.numeric(stack_p$day)

#### 08. incorporate specific place and transport time ####

specific_location <- function(stack_x) {
  spec_loc <- rep(NA, nrow(stack_x))
  spec_loc[stack_x$location =='pet'] <- sample(plantas_pet,sum(stack_x$location=='pet'),TRUE)
  spec_loc[stack_x$location =='gas'] <- sample(plantas_gas,sum(stack_x$location=='gas'),TRUE)
  spec_loc[stack_x$location =='agua'] <- sample(plantas_agua,sum(stack_x$location=='agua'),TRUE)
  spec_loc[stack_x$location =='pozo'] <- sample(pozos, sum(stack_x$location=='pozo'),TRUE)
  spec_loc[stack_x$location =='bat'] <- sample(baterias, sum(stack_x$location=='bat'),TRUE)
  spec_loc[stack_x$location =='sat'] <- sample(satelites, sum(stack_x$location=='sat'),TRUE)
  spec_loc[stack_x$location =='subest'] <- sample(subestacion, sum(stack_x$location=='subest'),TRUE)
  spec_loc[stack_x$location =='gasoducto'] <- sample(gasoducto, sum(stack_x$location=='gasoducto'),TRUE)
  spec_loc[stack_x$location =='moto'] <- sample(motogenerador, sum(stack_x$location=='moto'),TRUE)
  return(spec_loc)
}

#calculate trans time
stack_p$spec_loc <- specific_location(stack_p)

stack_p$transport_time <- sp[,stack_p$spec_loc]

stack_p$total_time <- stack_p$tiempo + stack_p$transport_time

#### 09. initialize the calendar of mantenience task corrective ####
stack_c.rows <- sum(tareas_c$q1y)*years2eval

#this stack object has these features
stack_c <- data.frame(task_id = numeric(stack_c.rows), location = character(stack_c.rows), 
                      tiempo= numeric(stack_c.rows),priority = character(stack_c.rows), 
                      day = numeric(stack_c.rows),stringsAsFactors=FALSE)


k <- 1

for (y in 1:years2eval) {
  for (i in 1:nrow(tareas_c)) {
    probab <- tareas_c[i, c('perc_Planta_pet','perc_Planta_agua', 'perc_Planta_gas',
                            'perc_bat', 'perc_pozo','perc_sat','perc_subest','perc_gasoducto','perc_moto')]
    
    for (j in 1:tareas_c[i,'q1y']) {

      loc <- sample(c('pet','agua','gas','bat','pozo','sat','subest','gasoducto','moto'),1, prob = probab)
      stack_c[k,] <- c(i,loc,tareas_c[i,'tiempo'],tareas_c[i,'type'],runif(1,0 + 17/24,total_days + 17/24))
      k <- k+1
    }
  }
}

#convert to numeric
stack_c$task_id <- as.numeric(stack_c$task_id)
stack_c$tiempo <- as.numeric(stack_c$tiempo)
stack_c$day <- as.numeric(stack_c$day)

#### 10. incorporate specific place and transport time ####
#calculate trans time
stack_c$spec_loc <- specific_location(stack_c)

stack_c$transport_time <- sp[,stack_c$spec_loc]

stack_c$total_time <- stack_c$tiempo + stack_c$transport_time

#### 11. create a summary of tasks to complete ####
total_task <- rep(NA,length(target))
names(total_task) <- target

#sum stack_c stack_p
for (prio in target) {
  total_task[prio] <- sum(stack_c$priority==prio) +sum(stack_p$priority==prio)
}

#measure the total work load
total_work_load <- sum(stack_c$total_time) + sum(stack_p$total_time)
total_work_load_py <- total_work_load/years2eval

#### 12. function create daily record ####
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

#### 13. loop call the function to generate the record ####
#num_cuad_loop <- c(6)
num_cuad_loop <- seq(6,12,2)
out <- NULL

for (num_cuad in num_cuad_loop) {
  print(c('num_cuad: ',num_cuad))
    
  stack_reprog <- NULL
  
  result_stack <- NULL
  
  for (day in 1:total_days) {
    #print(day)
    #condition if labor day
    if (day %in% labor_days_id) {
    
      daily_stack <- create_daily_record(stack_p,stack_c,stack_reprog, day,num_cuad)
      stack_reprog <- daily_stack[[2]]
      result_stack <- rbind(result_stack,daily_stack[[1]])
    } else {
      stack_reprog <- rbind(stack_reprog, stack_c[stack_c$day > day+17/24-1 & stack_c$day< (day +17/24),])
    }
  }
  
  #### 08. define metrics
  # 0. task at the end 
  perc_not_completed <- rep(NA,length(target_out_sorted))
  names(perc_not_completed) <- target_out_sorted

  for (prio in target_out_sorted) {
    perc_not_completed[prio] <- sum(stack_reprog$priority==prio)/total_task[prio]
  }
  temp_line <- perc_not_completed[target_out_sorted]
  names(temp_line) <- paste('perc_not_comp', target_out_sorted,sep="-")
  
  line <- c(num_cuad=num_cuad, temp_line)

  # 1. % tareas reprogramadas
  result_stack$reprog <- (result_stack$ini - result_stack$day)>1 +0
  perc_reprog <- tapply(result_stack$reprog, result_stack$priority, mean)
  
  temp_line <- perc_reprog[target_out_sorted]
  names(temp_line) <- paste('perc_reprog', target_out_sorted,sep="-")
  
  line <- c(line, temp_line)
  
  # 2. tiempo de instalacion parado
  dif <-  result_stack$ini -result_stack$day 
  ids_corr <- result_stack$priority %in% target[1:2]
  temp_df <- data.frame(dif=dif[ids_corr],priority=result_stack$priority[ids_corr],
                        location=result_stack$location[ids_corr])
  
  #Concatenate prior and loc
  temp_df$prio_loc <- paste (temp_df$priority,temp_df$location,sep='_')
  temp_df$prio_loc <- paste ('dpy_', temp_df$prio_loc,sep='')
  
  temp_line <- tapply(temp_df$dif,temp_df$prio_loc, sum)
  
  loc_prio_names <- c('dpy_cor-alta_agua','dpy_cor-alta_bat','dpy_cor-alta_gas','dpy_cor-alta_gasoducto','dpy_cor-alta_moto',
                      'dpy_cor-alta_pet', 'dpy_cor-alta_pozo','dpy_cor-alta_sat','dpy_cor-alta_subest','dpy_cor-media_agua',
                      'dpy_cor-media_bat','dpy_cor-media_gas','dpy_cor-media_gasoducto','dpy_cor-media_moto',
                      'dpy_cor-media_pet','dpy_cor-media_pozo','dpy_cor-media_sat','dpy_cor-media_subest')
  
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
  
  #names(line) <- c('Cuadrillas','% Correct Alta Sin completar','% Correct Media Sin completar',
  #            '% Correct Baja Sin completar','% Prevent Alta Sin completar','% Prevent Baja Sin completar',
  #            '% Predic Alta Sin completar','% Predic Baja Sin completar','% Correct Alta Reprogramada',
  #            '% Correct Media Reprogramada','% Correct Baja Reprogramada','% Prevent Alta Reprogramada',
  #            '% Prevent Baja Reprogramada','% Predic Alta Reprogramada','% Predic Baja Reprogramada',
  #            'Hs Correct Alta Agua','Hs Correct Alta Bateria','Hs Correct Alta Gas','Hs Correct Alta Petroleo',
  #            'Hs Correct Alta Pozo','Hs Correct Media Agua','Hs Correct Media Bateria',
  #            'Hs Correct Media Gas','Hs Correct Media Petroleo','Hs Correct Media Pozo',
  #            'Tiempo Total Trabajo Hs','Tiempo Prom Tarea Hs','Tiempo extra Hs','Hs Tareas Reprogramadas',
  #           'Hs Tareas Planificadas','Hs Tiempo Libre')
  
  
  #define hours stop of plantas (only corr alta)
  plantas_tot <- c(plantas_pet,plantas_agua,plantas_gas)
  resolution <- 10
  plantas_inactive <- rep(0, length(plantas_tot))
  names(plantas_inactive) <- plantas_tot
  
  for (planta in plantas_tot){
    
    #define the vector inactive by plant
    inactive <- rep(0,total_days*resolution)
    
    #verify the result stack with cor-alta and planta
    indexes_res <- result_stack$spec_loc==planta&result_stack$priority=='cor-alta'
    
    #sequence initial_time:fin_time
    initial_time <- round(result_stack[indexes_res,'day'],1)*resolution
    fin_time <- round(result_stack[indexes_res,'fin'],1)*resolution
    
    #mark inactive vector with 1
    for (i in 1:length(initial_time)) {
      inactive[initial_time[i]:min(fin_time[i],total_days*resolution)] <- 1
    }
    
    #verify if there is cor-alta task not completed
    indexes_rep <- stack_reprog$spec_loc==planta&stack_reprog$priority=='cor-alta'
    go_on <- sum(indexes_rep)>0 
    
    if (go_on) {
      initial_time <- round(stack_reprog[indexes_rep,'day'],1)*resolution
      
      for (i in 1:length(initial_time)) {
        inactive[initial_time[i]:total_days*resolution] <- 1
      }
    }
    plantas_inactive[planta] <- mean(inactive)
  }
  
  names(plantas_inactive) <- paste('perc_corr-alta_',names(plantas_inactive),sep='')
  
  line <- c(line,plantas_inactive)
  
  if (is.null(out)) {
    out <- line
  } else {
    out <- rbind(out,line)
  }

} # end loop section 5

#### 14. export ####
#export data.out to a excel

rownames(out)<-NULL

wb <- loadWorkbook("Tareas Mantenimiento vDemo2.xlsx")
removeSheet(wb, sheetName=sheet_name)
yourSheet <- createSheet(wb, sheetName=sheet_name)
addDataFrame(out, yourSheet,showNA=TRUE,row.names=FALSE)
saveWorkbook(wb, "Tareas Mantenimiento vDemo2.xlsx")

directory <- 'out_asignacion/'
file_name <- paste(c(directory,'c_',num_cuad,'_',sheet_name,'.xlsx'),collapse='')
write.xlsx(result_stack, file=file_name)

if (dim(stack_reprog)[1]>0){
  file_name_2 <- paste(c(directory,'c_',num_cuad,'_',sheet_name,'_incomp.xlsx'),collapse='')
  write.xlsx(stack_reprog, file=file_name_2)
}
}
