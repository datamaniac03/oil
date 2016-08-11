######################################
# Simulation Oil optimization SCRIPT #
######################################

#### 00. libraries and delete env ####
rm(list = ls())
cat("\f")
require(xlsx)
require(gdata)
require(igraph)
set.seed(10)

#### 01. read files with tareas & parameters ####
#parameters

years2eval <- 4
days1y <- 365

total_days <- years2eval*days1y
total_days_set <- 1:total_days
labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
labor_days_id <-  (1:total_days)[labor_days_bool]

#setwd("C:/Users/mferrari/Desktop/Mantenimiento Xavier")
tareas_log = read.xls ("Tareas Logistica para modelo v04.xlsx", sheet = 1, header = TRUE)

recursos = read.xls ("Tareas Logistica para modelo v04.xlsx", sheet = 2, header = TRUE)
rownames(recursos) = recursos$tipo


tareas_log <- tareas_log[,c(1:5,9,13)]
colnames(tareas_log) <- c('descrip','categoria','prioridad','q1y','tipo_vehiculo','tiempo_carg_desc',
                            'instalacion')

rutas = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 7, header = TRUE)
puntos = read.xls ("Tareas Mantenimiento vMArtin.xlsx", sheet = 8, header = TRUE)

#clean up
rutas <- rutas[,c(3:6,2)]

#keeps point Activos and with Category
colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')

#attention with the S? instead of Si depending on how to read the text
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

#### 02. explore graph ####
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

#initialize list of pozos, baterias, satelites, plantas
pozos <- rownames(puntos[puntos$Tipo=='Pozo',])
baterias <-rownames(puntos[puntos$Tipo=='Bateria',])
satelites <-rownames(puntos[puntos$Tipo=='Satelite',])
plantas_pet <- c('Planta Central', 'Planta La Petiza')
plantas_agua <- c('Planta Sur', 'Planta Norte','Planta Oeste')
plantas_gas <- c('Planta Gas')
subestacion <- c('Subestacion')
gasoducto <- c('Gasoducto')
motogenerador <- c('Motogenerador')

#### 03. initialize the calendar of logistics needs everithing but PURGA ####
#create a stack of activities predictive and preventive
stack.rows <- sum(tareas_log$q1y)*years2eval

#this stack object has these features
stack_task <- data.frame(task_id = numeric(stack.rows), categoria =  character(stack.rows), 
                         vehiculo =  character(stack.rows), 
                         spec_loc = character(stack.rows), 
                    prioridad = numeric(stack.rows), tiempo= numeric(stack.rows),
                      day = numeric(stack.rows),stringsAsFactors=FALSE)

sample_instalation <- function (instalation){

  if(instalation=='Todas') {
    out <- sample(puntos[,'Identif'],1)
  } else if (instalation=='Planta') {
    out <- sample(c(plantas_pet,plantas_gas,plantas_agua),1)
  } else {
    out <- sample(puntos[puntos[,'Tipo']==instalation,'Identif'],1)
  } 
  return (out)
}

#initialize stack of necesities
k <- 1

for (y in 1:years2eval) {
  for (i in 1:nrow(tareas_log)) {
    categ <- tareas_log[i,'categoria']
    
    for (j in 1:tareas_log[i,'q1y']) {
      
      
      loc <- sample_instalation(as.character(tareas_log[i,'instalacion']))
      
      if ( categ=='Pulling') {
        lab_days_sample <- runif(1,((y-1)*365),(y*365))
      } else {
        lab_days_sample <- sample((((y-1)*365+1):(y*365))[labor_days_bool[((y-1)*365+1):(y*365)]],1)
      }
      stack_task[k,] <- c(i,as.character(tareas_log[i,'categoria']),as.character(tareas_log[i,'tipo_vehiculo']),as.character(loc),tareas_log[i,'prioridad'],
                       tareas_log[i,'tiempo_carg_desc'],lab_days_sample)
      k <- k+1
      
      
      
    }
  }
}

#convert to numeric the columns
stack_task$task_id <- as.numeric(stack_task$task_id)
stack_task$tiempo <- as.numeric(stack_task$tiempo)
stack_task$day <- as.numeric(stack_task$day)
stack_task$prioridad <- as.numeric(stack_task$prioridad)

# 08. incorporate specific place and transport time
stack_task$transport_time <- sp[,stack_task$spec_loc]
stack_task$total_time <- stack_task$tiempo + stack_task$transport_time
  

#### 04. function create daily record ####
create_record <- function(stack_task,num_cuad,vehiculo) {
  
    #vehiculo<- 'Camion de vacio'
    #num_cuad <- 3
    #turno <- '12r'
  
    #get pred and corr tasks
    daily_stack <- stack_task[stack_task$vehiculo == vehiculo ,]
    
    #create a matrix with cuadrillas
    cuad_record <- data.frame(cuad_id = 1:num_cuad, 
                              day_time2free= 0 , stringsAsFactors=FALSE)
    
    
    #function assign task
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
    }
     
    #sort the daily stack by prority target
    daily_stack <- daily_stack[order(daily_stack$day,daily_stack$prioridad,daily_stack$total_time),] 
    
    #add columns to daily_stack
    daily_stack$ini <- NA
    daily_stack$fin <- NA
    daily_stack$cuad <- NA
    
    continue <- TRUE
    
    while(continue) {
      
      #get the cuad that frees first
      cuad_id <- which.min(cuad_record$day_time2free)
      
      #does the cuad finishes after the end of the four years?
      fin <- min(cuad_record$day_time2free)>(total_days+1)
      if (!fin) {
        
        #get possible task id to complete
        tasks2assign.id <- is.na(daily_stack$ini)
        
        fin <- sum(tasks2assign.id)==0
        if (!fin) {
          
          #is there any task that comes up before the day time to free
          old_tasks_bool <- daily_stack$day<cuad_record[cuad_id,'day_time2free']&tasks2assign.id
          ini_cuad <- sum(old_tasks_bool)>0
          
          if(ini_cuad) { 
            task_id <- min(which(old_tasks_bool))
            #print('#assign old task')

            assign_task (daily_stack,cuad_record,task_id,cuad_id,ini_cuad)
            
          } else {
            #print('#no old tasks, look for future tasks')
            task_id <- min( sort.int(daily_stack$day,index.return=T)$ix[tasks2assign.id])
              
            #assign task
            assign_task (daily_stack,cuad_record,task_id,cuad_id,ini_cuad)
            
          } 
          
        } else {
          continue = FALSE
          #print('#no more tasks')
        }
      } else {
        continue=FALSE
        #print('no more cuads to assign')
      }
    } #end while continue
  
    #return values
    ids_out <- is.na(daily_stack$ini)
    daily_stack_reprog <- daily_stack[ids_out,1:9]
    daily_record <- daily_stack[!ids_out,]
    return (list(daily_record, daily_stack_reprog))
}  

#Camion con hidro
create_daily_record <- function(stack_task,stack_reprog, vehiculo, day, num_cuad) {

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
    fin_temp <- ini_task + daily_stack[task_id,'total_time']/25
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
  vehic_ids <- stack_task$vehiculo==vehiculo
  
  daily_stack <- stack_task[vehic_ids&stack_task$day > day+17/24-1 & stack_task$day< (day +17/24),]
  
  #create a list of activities to be completed
  daily_stack <- rbind(daily_stack,stack_reprog)
  
  #sort the daily stack by prority target
  daily_stack <- daily_stack[order(daily_stack$prioridad,daily_stack$day,daily_stack$total_time),] 
  
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
      corr_tasks2assign.id <- daily_stack$prioridad %in% c(1,2)& tasks2assign.id
      pre_tast2assign.id <- !(daily_stack$prioridad %in% c(1,2))& tasks2assign.id
      
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
  daily_stack_reprog <- daily_stack[reprog_id,1:9]
  daily_record <- daily_stack[!reprog_id,]
  
  return (list(daily_record, daily_stack_reprog))
}    

#### 05. loop call the function to generate the record ####
#loop among vehicles
out=NULL
for (vehic in recursos$tipo[recursos$modelo=='si']){
  num_cuad_real = recursos[vehic,'q']
  for (num_cuad in (max(1,num_cuad_real-1):(num_cuad_real+1))){
    
    #if turno is 8 or 12 hr
    if (recursos[vehic,'turno']=='12r'){
       result <- create_record(stack_task,num_cuad,vehic)
       result_stack <- result[[1]]
       stack_reprog <- result[[2]]
       
    } else if (recursos[vehic,'turno']=='9d'){
      stack_reprog = NULL
      result_stack = NULL
      for (day in 1:total_days) {
        if (day %in% labor_days_id) {
          daily_stack <- create_daily_record(stack_task,stack_reprog,vehic, day,num_cuad)
          stack_reprog <- daily_stack[[2]]
     
          result_stack <- rbind(result_stack,daily_stack[[1]])
          
        } else {
          
          stack_reprog <- rbind(stack_reprog, stack_task[stack_task$day > day+17/24-1 & 
                                                           stack_task$day< (day +17/24),])
        }
      }
    }
    
    print(vehic)
    print(num_cuad)
    print (dim(result_stack))
 
    
    #### 08. define metrics
    
    #perc_transports_not_achieved
    perc_transports_not_achieved <- nrow(stack_reprog)/(nrow(result_stack)+nrow(stack_reprog))
    
    #perc_transports_reprogramed
    result_stack$reprog <- (result_stack$ini - result_stack$day)>1 +0
    perc_transports_reprogramed <-  mean(result_stack$reprog)
    
    #average delay in
    result_stack$delay <- result_stack$ini - result_stack$day
    average_delay <- mean(result_stack$delay)   
    
    #overtime
    hour_fin <- result_stack$fin - floor(result_stack$fin)
    overtime_py <- sum(hour_fin[hour_fin> 17/24]-17/24)*24/years2eval
    
    line <- data.frame( vehic=vehic, num_cuad=num_cuad, perc_transports_not_achieved=perc_transports_not_achieved,
               perc_transports_reprogramed=perc_transports_reprogramed,average_delay=average_delay,
               overtime_py=overtime_py)
    
    if (is.null(out)) {
      out <- line
    } else {
      out <- rbind(out,line)
    }
    
  } # end loop cuadrillas
} # end loop vehicle

#### 06. export data.out to a excel ####

rownames(out)<-NULL

sheet_name = 'out_modelo'
wb <- loadWorkbook("Tareas Logistica para modelo v04.xlsx")
removeSheet(wb, sheetName=sheet_name)
yourSheet <- createSheet(wb, sheetName=sheet_name)
addDataFrame(out, yourSheet,showNA=TRUE,row.names=FALSE)
saveWorkbook(wb,"Tareas Logistica para modelo v04.xlsx")

