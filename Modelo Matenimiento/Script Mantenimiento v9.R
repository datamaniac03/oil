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
years2eval <- 1
days1y <- 365

total_days <- years2eval*days1y
total_days_set <- 1:total_days
labor_days_bool <- rep(c(rep(TRUE,5),rep(FALSE,2)),total_days/6)[1:total_days]
labor_days_id <-  (1:total_days)[labor_days_bool]

#setwd("C:/Users/mferrari/Desktop/Mantenimiento Xavier")
tareas_m = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 3, header = TRUE)
tareas_e = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 1, header = TRUE)
tareas_i = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 5, header = TRUE)

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
  
  #convert percentage into number
  
  perc_names <- c('perc_Planta_pet','perc_Planta_agua','perc_Planta_gas',
                  'perc_bat','perc_pozo','perc_sat','perc_subest','perc_gasoducto','perc_moto')
  for (name in perc_names){
    tareas_out[,name] <- as.numeric(sub("%", "",tareas_out[,name]))
  }
  return(tareas_out)
}

tareas_m <- fix_format(tareas_m)
tareas_e <- fix_format(tareas_e)
tareas_i <- fix_format(tareas_i)

#### 02. read files with routes ####
rutas = read.xls ("Tareas Mantenimiento vMartin.xlsx", sheet = 7, header = TRUE)
puntos = read.xls ("Tareas Mantenimiento vMArtin.xlsx", sheet = 8, header = TRUE)

#clean up
rutas <- rutas[,c(3:6,2)]
rutas$Ruta <- gsub('\xed','i',rutas$Ruta)
puntos$Activo <- gsub('\xed','i',puntos$Activo)

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
sheet_name_loop <- c('Output Mant. Elect.', 'Output Mant. Mecanic.','Output Mant. Instrument.')

for (tar_id in 1:3) { ## principal loop

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
  

#### 12. functions ####
# append the two stacks and add some features and order by day.
stack_tot <- rbind(stack_c,stack_p)

stack_tot$comp_perc <- 0
stack_tot$cuad <- NA
stack_tot$completed <- FALSE
stack_tot$corrective <- startsWith(stack_tot$priority,"cor")
stack_tot$end_task_time <- NA

rownames(stack_tot) <- NULL

#sort the daily stack by prority target
order1 <- match(stack_tot$priority,target)
stack_tot$priority_num <- order1
stack_tot <- stack_tot[order(stack_tot$priority_num,stack_tot$day),] 

stack_tot$stack_id <- 1:nrow(stack_tot)
row.names(stack_tot) <- stack_tot$stack_id

get_open_task <- function(stack_tot, day_time, corrective) {
  #this function can also be used to get the next open task by doing day_time = day +17/24
  if(corrective) {
    ids_corr <- stack_tot$corrective
  } else {
    ids_corr <- !stack_tot$corrective
  }

  relevant_ids <- stack_tot$day<day_time & is.na(stack_tot$cuad) & ids_corr
  id <-which(relevant_ids)[1]
  return (stack_tot[id,])
}

#create cuad record
initialize_cuad_record <- function (day, num_cuad) {
  cuad_record <- data.frame(cuad_id = 1:num_cuad, 
                          day_time2free= day + 8/24,lunch=FALSE,end_day=FALSE, ot=FALSE,stringsAsFactors=FALSE)
  return(cuad_record)
}

record <- data.frame(cuad_id=integer(), 
                          stack_id=integer(),
                          day=integer(),
                          worked_reg_hours = double(),
                          worked_ot_hours = double(),     
                          stringsAsFactors=FALSE) 

insert_row_record <- function (row) {
  record[nrow(record)+1,]<<-row
}

end_day_cuads <- function (){
  
  ids_to_end <- 17/24-(cuad_record$day_time2free- floor(cuad_record$day_time2free)) <0.5/24
  cuad_record$end_day[ids_to_end]<- TRUE
  
}

assign_task_cuad <- function(stack_id,cuad_id) {

  task <- stack_tot[stack_id,]
  #print('records to assign')
  #print(task)
  #print(cuad_record[cuad_id,])
  task$cuad <- cuad_id
  
  free_time <- cuad_record[cuad_id,'day_time2free']
  task_ini_time <- stack_tot[stack_id,'day']
  
  duration <- task$tiempo*(1-task$comp_perc)
  
  day <- floor(free_time)
  lunch_ini_time <- day + 12/24
  lunch_fin_time <- day + 13/24
  day_fin_time <- day + 17/24
  day_fin_time_wot <- day + 21/24
  
  ini_time <- max(free_time,task_ini_time)
  
  if (!cuad_record[cuad_id,'lunch']) { #if not lunch
    #print('not lunch')
    
    end_task <- ini_time + (duration + task$transport_time)/24

    if (end_task < lunch_ini_time) { #ends before lunch
      #print('ends before lunch')
      #normal assigment 100%completed
     
      #update stack_tot
      task$comp_perc <- 1
      task$completed <- TRUE
      task$end_task_time <- end_task
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
        #print('finishes before the end of the day')
        #update stack_tot
        task$comp_perc <- 1
        task$completed <- TRUE
        task$end_task_time <- end_task
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
            
            #update stack_tot
            task$comp_perc <- 1
            task$completed <- TRUE
            task$end_task_time <- end_task
            #update cuad_record
            cuad_record[cuad_id,'day_time2free'] <<- end_task
            #append_record
            insert_row_record(c(cuad_id,stack_id,day,duration,ot))
          
          } else { #finishes after the day_fin_time_wot
            #print('finishes after the end of the day wot')
            #print(end_task)
            #print(day_fin_time_wot)
            ot <- 4
            worked_hours <- (day_fin_time_wot - 1/24 - 2*task$transport_time/24 - ini_time)*24
            if (worked_hours<0) {stop("worked_hours<0")}
            task$comp_perc <- (task$tiempo*task$comp_perc + worked_hours)/task$tiempo 
            cuad_record[cuad_id,'day_time2free'] <<- day_fin_time_wot
            #append_record
            insert_row_record(c(cuad_id,stack_id,day,worked_hours,4))
            
          }
       
        } else { #overtime not possible
          #print('ot not possible     XXXXXXXXXXXXXX   ')
          #print('ot not possible     XXXXXXXXXXXXXX   ')
          #print('ot not possible     XXXXXXXXXXXXXX   ')
          #print('ot not possible     XXXXXXXXXXXXXX   ')

          
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
    #print('cuad already lunch')
    
    end_task <- ini_time + (duration + task$transport_time)/24
    
    if (end_task < day_fin_time) { #finishes before the end of the day
      #print('finishes before the end of the day')
      #update stack_tot
      task$comp_perc <- 1
      task$completed <- TRUE
      task$end_task_time <- end_task
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
          
          #update stack_tot
          task$comp_perc <- 1
          task$completed <- TRUE
          task$end_task_time <- end_task
          #update cuad_record
          cuad_record[cuad_id,'day_time2free'] <<- end_task
          #append_record
          insert_row_record(c(cuad_id,stack_id,day,duration,ot))
          
        } else { #finishes after the day_fin_time_wot
          #print('finishes after the end of the day wot')
          #print(end_task)
          #print(day_fin_time_wot)
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
  stack_tot[stack_id,] <<- task
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

verify_end_day <- function() {
  ids_to_end <- -(cuad_record$day_time2free - floor(cuad_record$day_time2free)) + 17/24 < 0.5/24
  cuad_record$end_day[ids_to_end] <<- TRUE
  cuad_record$day_time2free[ids_to_end] <<- floor(cuad_record$day_time2free)[ids_to_end] + 17/24 
}

day_record <- function(day,num_cuad) {
  #print('day_record')
  #print(day)
  
  cuad_record <<- initialize_cuad_record (day, num_cuad)
  #loop among incompleted tasks and assign
  incompleted_tasks_ids <- stack_tot$comp_perc>0&!stack_tot$completed
  
  for (row in which(incompleted_tasks_ids)) {
    #print('assigning incompleted tasks')
    assign_task_cuad(row,stack_tot[row,'cuad'])
  }
  
  continue_day <- TRUE
  i<-1
  while(continue_day){
    #print(c('interation:',i))
    cuad_id <- get_available_cuad_id(cuad_record)
    
    if(length(cuad_id)==0) {
      continue_day<- FALSE
    } else {
      
      ot_logic <- sum(cuad_record$ot)>0
      if (ot_logic) {
        verify_end_day()
      }
      cuad_id <- get_available_cuad_id(cuad_record)
      
      if (length(cuad_id)>0) {
      
      #get open corrective task
      task <- get_open_task (stack_tot, cuad_record[cuad_id,'day_time2free'], TRUE)
      #print('corrective task:')
      #print(task)
      
      if(!is.na(task$task_id)) {
        #print('assigment:')
        #print(c(task$stack_id,cuad_id))
        assign_task_cuad(task$stack_id,cuad_id)
      } else { #not a corrective task
        #verify end of the day
        verify_end_day()
        #print(cuad_id)
        #print('pred task:')
        
        task <- get_open_task (stack_tot, cuad_record[cuad_id,'day_time2free'], FALSE)
        #print(task)

        
        if(!is.na(task$task_id)) {
          #print('assigment:')
          #print(c(task$stack_id,cuad_id))
          assign_task_cuad(task$stack_id,cuad_id)
        } else  { #verify a next corr stack
          
          task <- get_open_task (stack_tot, day + 16/24, TRUE)
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
              assign_task_cuad(task$stack_id,cuad_id)
            }
          } else {
            continue_day = FALSE
          }
        }
      }
    }
    i<- i+1
    }
  }

}

#### 13. loop among days & num_cuad ####
num_cuad_loop <- c(8,10,12)

out=NULL

for (num_cuad in num_cuad_loop ) {


#reset stack tot
stack_tot$comp_perc <- 0
stack_tot$cuad <- NA
stack_tot$completed <- FALSE
stack_tot$end_task_time <- NA



for (day in 1:total_days) {
  print(c('day:',day))
  #condition if labor day
  if (day %in% labor_days_id) {
    day_record(day,num_cuad)
  }
}

#### 14. define metrics ####
# 0. task at the end 
perc_not_completed <- rep(NA,length(target_out_sorted))
names(perc_not_completed) <- target_out_sorted

for (prio in target_out_sorted) {
  perc_not_completed[prio] <- tapply(!stack_tot$completed,stack_tot$priority,FUN =mean)[prio]
}
temp_line <- perc_not_completed[target_out_sorted]
names(temp_line) <- paste('perc_not_comp', target_out_sorted,sep="-")

line <- c(num_cuad=num_cuad, temp_line)

# 2. tiempo de instalacion parado
dif <-  stack_tot$end_task_time - stack_tot$day 
ids_corr <- stack_tot$priority %in% target[1:2] & !is.na(stack_tot$end_task_time)
temp_df <- data.frame(dif=dif[ids_corr],priority=stack_tot$priority[ids_corr],
                      location=stack_tot$location[ids_corr])


#Concatenate prior and loc
temp_df$prio_loc <- paste (temp_df$priority,temp_df$location,sep='_')
temp_df$prio_loc <- paste ('dpy_', temp_df$prio_loc,sep='')

temp_line <- tapply(temp_df$dif,temp_df$prio_loc, sum)

loc_prio_names <- c( 'dpy_cor-alta_pozo','dpy_cor-media_pozo')

dpy_line <- rep(NA, length(loc_prio_names))
names(dpy_line)<- loc_prio_names

for (loc_prio in loc_prio_names) {
  dpy_line[loc_prio] <- temp_line[loc_prio]
}

line <- c(line,dpy_line)

# 3. overtime and free time
overtime_py <- sum(record$worked_ot_hours)

total_ava_time <- num_cuad*years2eval*days1y*8
total_ava_time_py <- num_cuad*days1y*8

#task not completed
#task_not_completed_time <- sum(stack_tot$total_time)
#task_not_completed_time_py <- task_not_completed_time/years2eval

#  total work load - task not completed = overtime + ontime
# ontime = ava_time - freetime
# --->
#ontime_py <- total_work_load_py - task_not_completed_time_py - overtime_py
#freetime_py <- total_ava_time_py - ontime_py

temp_line <- t(c(total_work_load_py,total_ava_time_py,overtime_py))
names(temp_line) <- c('total_work_load_py','total_ava_time_py','overtime_py')

line <- c(line, temp_line)

if (is.null(out)) {
  out <- line
} else {
  out <- rbind(out,line)
}

print(out)
} #loop among num_cuad 

#### 15. export data.out to a excel ####
rownames(out)<-NULL

wb <- loadWorkbook("Tareas Mantenimiento vMartin.xlsx")
removeSheet(wb, sheetName=sheet_name)
yourSheet <- createSheet(wb, sheetName=sheet_name)
addDataFrame(out, yourSheet,showNA=TRUE,row.names=FALSE)
saveWorkbook(wb,"Tareas Mantenimiento vMartin.xlsx")

directory <- 'out_asignacion/'
file_name <- paste(c(directory,'c_',num_cuad,'_',sheet_name,'.xlsx'),collapse='')
write.xlsx(stack_tot, file=file_name)

 
} #loop among sheets in excel

