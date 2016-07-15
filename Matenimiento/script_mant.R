######################################
# Simulation Oil optimization SCRIPT #
######################################

#### 00. libraries and delete env ####
rm(list = ls())
require(gdata)
require(WriteXLS)

#### 01. read files & parameters ####
tareas = read.xls ("Tareas de Mtto validadas V3.xlsx", sheet = 3, header = TRUE)
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

#### 02. initialize the calendar of manteniance task predictive and preventive ####
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

#### 03. initialize the calendar of mantenience task corrective ####
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
      stack_c[k,] <- c(i,loc,tareas_c[i,'tiempo'],tareas_c[i,'type'],runif(1,1,total_days+1))
      k <- k+1
    }
  }
}

#convert to numeric
stack_c$task_id <- as.numeric(stack_c$task_id)
stack_c$tiempo <- as.numeric(stack_c$tiempo)
stack_c$day <- as.numeric(stack_c$day)

#### 04. create a summary of tasks to complete ####
total_task <- rep(NA,length(target))
names(total_task) <- target

#sum stack_c
for (prio in target) {
  total_task[prio] <- sum(stack_c$priority==prio) +sum(stack_p$priority==prio)
}

#measure the total work load
total_work_load <- sum(stack_c$tiempo) + sum(stack_p$tiempo)
total_work_load_py <- total_work_load/years2eval

#### 05. function create daily record ####
create_daily_record <- function(stack_p,stack_c,stack_reprog, day,num_cuad) {
  
  assign_task <- function(daily_stack,cuad_record,task_id,cuad_id,ini_cuad) {
    if (ini_cuad) {
      ini_task <- cuad_record[cuad_id,'day_time2free']
    } else {
      ini_task <- daily_stack[task_id,'day']
    }
    #update times
    daily_stack[task_id,'ini'] <<- ini_task
    fin_temp <- ini_task + daily_stack[task_id,'tiempo']/24
    daily_stack[task_id,'fin'] <<- fin_temp
    cuad_record[cuad_id,'day_time2free'] <<- fin_temp
    
    #update cuad
    daily_stack[task_id,'cuad'] <<- cuad_record[cuad_id,'cuad_id']
  }
  
  #get pred task
  daily_p <- stack_p[stack_p$day == day,]
  daily_c <- stack_c[stack_c$day > day+16/24-1 & stack_c$day< (day +16/24),]
  
  #create a list of activities to be completed
  daily_stack <- rbind(daily_c,daily_p)
  daily_stack <- rbind(daily_stack,stack_reprog)
  
  #sort the daily stack by prority target
  daily_stack$order1 <- match(daily_stack$priority,target)
  daily_stack <- daily_stack[order(daily_stack$order1,daily_stack$day,daily_stack$tiempo),] 

  #add columns to daily_stack
  daily_stack$ini <- NA
  daily_stack$fin <- NA
  daily_stack$cuad <- NA
  
  #create a matrix with cuadrillas
  cuad_record <- data.frame(cuad_id = 1:num_cuad, 
                            day_time2free= day + 8/24,stringsAsFactors=FALSE)
  
  #corr alta y media before 8 am.  the day ends at 16hs
  ini_time <- day + 8/24
  fin_time <- day + 16/24
  
  ## loop starts here
  continue_day = TRUE
  while (continue_day) { 
    
    #select the cuad that frees first
    cuad_id <- which.min(cuad_record$day_time2free)
    
    #get the free time of that cuad
    free_daytime <- cuad_record[cuad_id,2]
    
    #get possible task id to complete
    tasks2assign.id <- is.na(daily_stack$ini)
    corr_tasks2assign.id <- daily_stack$order1 %in% c(1,2)& tasks2assign.id
    pre_tast2assign.id <- !(daily_stack$order1 %in% c(1,2))& tasks2assign.id
    
    #is there a task to complete in the stack?
    if (sum(tasks2assign.id)>0) {
      
      #is there a cuad that frees before 16hs?
      if (free_daytime<fin_time) {
        
        #is there a CORRECTIVE task (prior alta y media, not baja) in the stack that show up?
        corr_task2a_pos.id <- corr_tasks2assign.id & (daily_stack$day <= free_daytime)
        if (sum(corr_task2a_pos.id)>0){
          #get the first roww and assign
          task_id <- min(which(corr_task2a_pos.id == TRUE))
          assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE)
            
        } else {
          
          #is there a task in the pred stack to start given that ini+tiempo < fin_tiempo?
          #also consider that free(cuad) > day(task)
          pre_task2a_pos.id <- pre_tast2assign.id & (daily_stack$tiempo/24 + free_daytime <= fin_time)&
                              (daily_stack$day < free_daytime)
          
          if (sum(pre_task2a_pos.id)>0){
            task_id <- min(which(pre_task2a_pos.id == TRUE))
            assign_task(daily_stack,cuad_record,task_id,cuad_id,TRUE)
          } else {
            
            #if there is no pred stack. the free time goes to the min( first corr day time , fin_time)
            corr_task2a_pos.id <- corr_tasks2assign.id & (daily_stack$day < fin_time)
            if (sum(corr_task2a_pos.id)>0){
              #get the first roww and assign
              task_id <- min(which(corr_task2a_pos.id == TRUE))
              assign_task(daily_stack,cuad_record,task_id,cuad_id,FALSE)
              
            } else {
              print('end day 1')
              continue_day = FALSE
            }
          }
          #
        }
        
      } else { 
        #end day# 
        print('end day 2')
        continue_day = FALSE
      }
    } else { 
      #end day# 
      print('end day 3')
      continue_day = FALSE
    }
  }
  reprog_id <- is.na(daily_stack$ini)
  daily_stack_reprog <- daily_stack[reprog_id,1:5]
  daily_record <- daily_stack[!reprog_id,]
  return (list(daily_record, daily_stack_reprog))
}

#### 06. call the function to generate the record ####
num_cuad <- 10
stack_reprog <- NULL

result_stack <- NULL
for (day in 1:total_days) {
  print(day)
  daily_stack <- create_daily_record(stack_p,stack_c,stack_reprog, day,num_cuad)
  stack_reprog <- daily_stack[[2]]
  result_stack <- rbind(result_stack,daily_stack[[1]])
}

#### 07. define metrics ####
# 0. task at the end 
perc_not_completed <- rep(NA,length(target))
names(perc_not_completed) <- target

for (prio in target) {
  perc_not_completed[prio] <- sum(stack_reprog$priority==prio)/total_task[prio]
}

# 1. % tareas reprogramadas
result_stack$reprog <- (result_stack$ini - result_stack$day)>1 +0
perc_reprog <- tapply(result_stack$reprog, result_stack$priority, mean)

# 2. tiempo de instalacion parado
dif <-  result_stack$ini -result_stack$day 
ids_corr <- result_stack$priority %in% target[1:2]
temp_df <- data.frame(dif=dif[ids_corr],priority=result_stack$priority[ids_corr],
                      location=result_stack$location[ids_corr])


corr_times <- as.vector(t(with(temp_df,tapply(dif, list(priority, location), sum))/years2eval))

# 3. overtime and free time
hour_fin <- result_stack$fin - floor(result_stack$fin)
overtime_py <- sum(hour_fin[hour_fin> 16/24])*24/years2eval

total_ava_time <- num_cuad*years2eval*days1y*8
total_ava_time_py <- num_cuad*days1y*8

#task not completed
task_not_completed_time <- sum(stack_reprog$tiempo)
task_not_completed_time_py <- task_not_completed_time/years2eval

#  total work load - task not completed = overtime + ontime
# ontime = ava_time - freetime
# --->
ontime_py <- total_work_load_py - task_not_completed_time_py - overtime_py
freetime_py <- total_ava_time_py - ontime_py

#### 08. Solution analysis and display ####

data.out <- c(jornadas)

nr <- length(jornadas)
pozos.trab <- rep(NA,nr)

#print the solution
for (i in 1:nr){
  pozos.trab[i] <- paste (sol[[i]], collapse=',')
}

data.out <- cbind.data.frame(data.out,pozos.trab)

data.out <- cbind.data.frame(data.out,t.m)

complete.lines <- function(route) {
  route.lines <- rep(NA, length(route)*2-1)
  route.lines[1] <- route[1]
  for (i in 2:length(route)){
    l <- rutas[rutas$origen==route[i-1]&rutas$destino==route[i],'ruta']
    if (length(l)==0) {
      l <- rutas[rutas$origen==route[i]&rutas$destino==route[i-1],'ruta']
    }
    #if (length(l)>1) { print (l)}
    route.lines[i*2-2] <- l[1]
    
    route.lines[i*2-1] <- route[i]
  }
  return(route.lines)
}

#print the route detail 
detailed.route <- function(route){
  
  route.d1 <- rownames(as.matrix(get.shortest.paths(g,as.numeric(V(g)["Base"]),as.numeric(V(g)[route[1]]), weights = rutas$tiempo)$vpath[[1]]))
  
  
  route.d1.p <- paste(complete.lines(route.d1),collapse=",")
  route.tot <- route.d1.p
  
  if (length(route)>1) {
    for (i in 2:length(route)){
      route.di <- rownames(as.matrix(get.shortest.paths(g,as.numeric(V(g)[route[i-1]]),as.numeric(V(g)[route[i]]), weights = rutas$tiempo)$vpath[[1]]))
      route.di.p <- paste(complete.lines(route.di),collapse=",")
      route.tot <- paste(route.tot, route.di.p,sep=";")
    }
  }
  
  route.df <- rownames(as.matrix(get.shortest.paths(g,as.numeric(V(g)[route[length(route)]]),as.numeric(V(g)["Base"]), weights = rutas$tiempo)$vpath[[1]]))
  route.df.p <- paste(complete.lines(route.df),collapse=",")
  route.tot <- paste(route.tot, route.df.p,sep=";")
  return (route.tot)
}

detailed <- rep(NA,nr)

for (i in 1:length(sol)){
  detailed[i] <- detailed.route(sol[[i]])
}

data.out <- cbind.data.frame(data.out,detailed)

colnames(data.out)[1] <- 'jornada'

cuadrilla <- NULL
day <- rep(NA,nrow(data.out))
turno <- rep(NA,nrow(data.out))

for (i in tabulate(data.out$jornada)) {
  cuadrilla <- c(cuadrilla,sample(seq(1,i)))
}
day <-  floor((data.out$jornada+1)/2) 

turno <- rep('tarde',nrow(data.out))
turno [(data.out$jornada+1)/2 == day] <- 'manana'

data.out <- cbind.data.frame(cuadrilla,data.out)
data.out <- cbind.data.frame(turno,data.out)
data.out <- cbind.data.frame(day,data.out)

#add coord
rownames(coord)<-coord$Puntos.de.Operacion 

get.coord <- function (det.rout) {
  segments <- strsplit(det.rout,';')[[1]]
  det.rout.coord <- NULL
  for (seg in segments) {
    nodes <- strsplit(seg,',')[[1]]
    det.rout.coord <- paste(det.rout.coord,paste(coord[nodes,2], collapse=','),sep=';')
  }
  det.rout.coord <- substring(det.rout.coord, 2)
  return(det.rout.coord)
}

det.coord <- rep(NA,length(detailed)) 
for (i in 1:length(detailed)){
  det.coord[i] <- get.coord(detailed[i])
}

data.out <- cbind.data.frame(data.out,det.coord)

#sort data frame
data.out<-data.out[ order(data.out[,'jornada']), ]

#export data.out to a excel
WriteXLS(data.out, "modelo_resuelto.xlsx")

tapply(data.out$av.time,data.out$cuadrilla,sum)


