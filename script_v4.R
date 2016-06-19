###################################
# Routing Oil optimization SCRIPT #
###################################

#### 00. libraries and delete env ####
rm(list = ls())
require(gdata)
require(igraph)
require(WriteXLS)

#### 01. read files ####
rutas = read.xls ("Servicio Recorredores_v04.xlsx", sheet = 2, header = TRUE)
puntos = read.xls ("Servicio Recorredores_v04.xlsx", sheet = 3, header = TRUE)
datos = read.xls ("Servicio Recorredores_v04.xlsx", sheet = 1, header = TRUE)

#labor <- as.numeric(as.character(datos[2,3]))
labor <- as.numeric(as.character(datos[2,'Valor']))
tiempo_jor <- 225
iterations <- 15000

min.input.number <- 8

factor.savings <- 1 #used to balance the solution

#clean up
rutas <- rutas[,c(3:6,2)]
rutas$Ruta <- gsub('\xed','i',rutas$Ruta)
puntos$Activo <- gsub('\xed','i',puntos$Activo)

#keeps point Activos and with Category
colnames(rutas)<- c('origen','destino','distancia','tiempo','ruta')

#attention with the Sí instead of Si depending on how to read the text
puntos <- puntos[puntos$Activo=='Si'&puntos$Categoria!='Fuera'&puntos$Categoria!='#N/A',c(2,5,6)]

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

#remove not connected points in the node list
puntos <- puntos[!(puntos$Identif %in% puntos.not_connect),]

# calculate all shortest paths from base
sp <- shortest.paths(g, 1,
                   weights = rutas$tiempo)

# declare a matrix of sp
sp.m <- matrix (0, ncol=nrow(puntos)+1,nrow=nrow(puntos)+1)
rownames(sp.m) <- c('Base',as.character(puntos$Identif))
colnames(sp.m) <- c('Base',as.character(puntos$Identif))

# calculate sp matrix (can be improved)
for (i in rownames(sp.m)) {
  print(i)
  id_i <- as.numeric(V(g)[i])
  for (j in colnames(sp.m)) {
    id_j <- as.numeric(V(g)[j])
    if (sp.m[i,j] ==0) {
      s <- shortest.paths(g, id_i, id_j, weights = rutas$tiempo)
      sp.m[i,j] <- s
      sp.m[j,i] <- s
    }
  }
}

#### 03. functions ####

#create a list of times for each point
tiempos.detail <- function(route) {
  #Chequed!!
  n.points <- length(route)
  times <- rep(0,(n.points+1))
  times[1] <- sp.m ['Base',route[1]]
  times[n.points+1] <- sp.m [route[n.points],'Base']
  if (n.points >1) {
    for (i in 2:n.points) {
      times[i] <-  sp.m [route[i-1],route[i]]
    }
  } 
  return(times/60)
}

tiempos.total <- function(route) {
  #chequed!
  n.points <- length(route)
  #print(c('t.t',as.character(n.points)))

  t.transport <- sum(tiempos.detail(route))
  t.pozo <- labor*n.points
  return(c(t.transport,t.pozo,n.points,tiempo_jor-t.transport-t.pozo))
}

#create a list of times resulting of removing each point
tiempos.saved <- function(route) {
  #checked
  n.points <- length(route)
  times.s <- rep(0,n.points)
  times <- tiempos.detail(route)
  if (n.points==1){
    times.s[1] <- sum(times)
  } else {
    #first element
    times.s[1] <- times [1] + times[2] - sp.m ['Base', route[2]]/60 
    #last element
    times.s[n.points] <- times [n.points] + times[n.points+1] - sp.m [route[n.points-1],'Base']/60
    if (n.points>2) {
      for (i in 2:(n.points-1)) {
        times.s[i] <- times [i] + times[i +1] - sp.m [route[i-1],route[i+1]]/60
      }
    }
  }
  times.s [times.s<0] <- 0
  return (times.s)
}

convert.index <- function(i) {
  if (i<1) {
    i <- i + 21
  }
  if (i>21) {
    i <- i - 21
  }
  return (i)
}

mask.visitas <- function(visitas.n) {
  #input binary vector
  #boolean mask
  cat <- sum(visitas.n)
  temp <- rep(1,21)
  
  #pozos A
  if (cat==2) {
    days <- 7
    gap <- 2
  } else {
    days <- 10
    gap <- 3
  }
  
  #loop among days
  for (d in days.id[which(visitas.n==1)]) {
    temp2 <- rep(0,21)
    for (j in (d+days-gap):(d+days+gap)) {
      temp2[convert.index(j)] <- 1
    }
    
    for (j in (d-days-gap):(d-days+gap)) {
      temp2[convert.index(j)] <- 1
    }
    temp <- temp & temp2
  }
  return(which(rep(temp,rep(2,21))[1:41]))
}

tiempos.2add <- function(route,node) {
  n.points <- length(route)  
  #print(c('t.2a',n.points))
  times.2 <- rep(0,n.points+1)
  times.d <- tiempos.detail(route)
  times.2[1] <- sp.m['Base',node] + sp.m[node, route[1]]
  times.2[n.points+1] <- sp.m[route[n.points],node] + sp.m[node, 'Base'] 
  if (n.points>1) {
    for (i in 2:n.points) {
      times.2[i] <- sp.m[route[i-1],node] + sp.m[node,route[i]]
    }
  }
  return(times.2/60-times.d)
}

where2add <- function(route,node, saving) {
  #print('where2add')
  t2 <- tiempos.2add(route, node)
  
  if (node %in% route) {
    print('node already in route')
    return (0)
  } else if (sum(saving > t2)==0) {
    print('cost greather than saving')
    return (0)
  } else {
    td <- tiempos.total(route)[4]
    pos <- sample(which(saving > t2),1)
    if (td < t2[pos] + labor) {
      print('time increases the 240 min')
      return (0)
    } else {
      print('to be added')
      return (pos)
    }
  }
}

add.node <- function  (route,node, position) {
  route <- append (route, node, position-1)
  return (route)
}

remove.node <- function (route,position) {
  route <- route [-position]
  return (route)
}

move.cycle <- function (sol.id, new.jor) {
  #update jornadas
  old.jor <- jornadas[sol.id] 
  jornadas[sol.id] <<- new.jor
  #update visitas
  route2move <- sol[[sol.id]]
  visitas[old.jor,route2move] <<- 0
  visitas[new.jor,route2move] <<- 1
}

#### 04.0. start modeling initiation ####
# A 3 times, B 2 times, C, D, X 1time
set.seed(1)

# select in the bag 
#order by category
puntos <- puntos[order(puntos$Categoria),]

#solution empty list
sol <- list()

#### 04.1. initialization pozos ####
# initial solution each turno selects 4 points.(adjust according to the length of bag)
# 41 half-days, odds-mornings even-evenings.
# initial solution A) 2 cicles per half with 4 A nodes
jornadas <- NULL
#matrix of visitas
visitas <- matrix(0, ncol= nrow(puntos), nrow = 41)
colnames(visitas) <- puntos$Identif

#initialize A point
bag<- as.character(puntos$Identif[puntos$Categoria=='A'])
jor <- 1

for (i in 1:length(bag)) {
  c1 <- bag[i]
  
  if (jor+28>41) { jor <- 1}
  
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor)
  visitas [jor,c1 ] <- 1
  
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor+14)
  visitas [jor+14,c1 ] <- 1
  
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor+28)
  visitas [jor+28,c1] <- 1 
  
  #add 1 to jor
  jor <- jor +1
} 

#initialize B points
bag<- as.character(puntos$Identif[puntos$Categoria=='B'])
jor <- 1

for (i in 1:length(bag)) {
  #two cicles earch hjor
  c1 <- bag[i]
 
  if (jor+20>41) { jor <- 1} 
  
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor)
  visitas [jor,c1 ] <- 1
  
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor+20)
  visitas [jor+20,c1 ] <- 1
  
  jor <- jor +1
}

#initialize Other points
bag<- as.character(puntos$Identif[puntos$Categoria!='A'&puntos$Categoria!='B'])

for (i in 1:length(bag)) {
  #two cicles earch hjor
  #select the jor with lower cicles
  aa <- tabulate(jornadas)
  names(aa) <- 1:41  
  jor <- as.numeric(names(sort(aa)[1]))
  
  #same as before
  c1 <- bag[i]
  sol <- append (sol, list(c1))
  jornadas <- c(jornadas,jor)
  visitas [jor,c1 ] <- 1
}

#### 04.2 start modeling loop (local search) ####
min.input <- min.input.number

#calculates time and reshape as matrix
t.m <- do.call(rbind, lapply(sol,tiempos.total))
colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')

#decalare a vector with the days (len 41)
days.id <- c(rep(1:20,rep(2,20)),21)

k<- 1
while (k <= iterations) {
#define global evaluations of solutions
#initial diagnisys
turnos <- nrow(t.m)
tiempo.ocioso <- sum(t.m[,'av.time'])
print(c('k:',k,'-turnos:',turnos,'-tiempo.ocioso:',tiempo.ocioso))

#select a tour to extract one point. (proportional to 9-n.points)
#select more probability to the jornadas with more than 2 cicles
aa <- tabulate(jornadas)
names(aa) <- 1:41  

#always it has to be at least one cylce in in jornada
aa <- aa -1
aa[aa<0] <- 0
fact <-1



if (k>0.6*iterations) {
  tab.jor <- tabulate(jornadas)
  max.jor <- max(tab.jor)
  if (max.jor < min.input) {
    min.input <- max.jor
    print (paste(c('found solution of ',max.jor,' jornadas.'),sep='') )
    accept <- readline(prompt="Accept? (y/n): ")
    if (accept =='y') {break}
  }
  if (k==iterations) { 
    print (paste(c('Max. iter reached. Solution: ',max.jor,' jornadas.'),sep='') )
    accept <- readline(prompt="Accept this solution (y) or continue exploring (n)? (y/n): ")
    k <- 1
    if (accept =='y') {break} 
    
  }  
  #balance beteween morning and afternoon
  for (jor in seq(1,39,2)) {
    go.on <- abs(tab.jor[jor] - tab.jor[jor+1])>1
    if (go.on) {
      morning.high <- tab.jor[jor]> tab.jor[jor+1]
      if(morning.high){
        #pick up one cycle in jor and move it to jor+1
        sol.id2move <- sample (which(jornadas==jor),1)
        move.cycle(sol.id2move,jor+1)
      } else {
        #pich up one cylce in jor+1 and move it to jor
        sol.id2move <- sample (which(jornadas==(jor+1)),1)
        move.cycle(sol.id2move,jor)
        
      }
    }
  }
  
}

#select a jor to extract
jor2extract <- as.numeric(sample(names(aa), 1, prob = aa))
#print(c('jor2e: ', jor2extract))

#select the tour to extract 
tours2e <- which(jornadas==jor2extract)
#print(c('tours2e' ,tours2e ))

tour2extract <- sample(tours2e,1, prob=t.m[tours2e,'av.time'])
#print(c('tour2extract' ,tour2extract ))
#declare route to work
route2e <- sol[[tour2extract]]

#calculate the times saved
t.s <- tiempos.saved(route2e)

#select the point to extract
pos2e <- sample(1:length(route2e),1, prob = t.s)
node <- route2e[pos2e]

#calculate savings
saving <- t.s[pos2e] 
saving <- saving*fact #fact not used

#where to add the node?
#case if it is A or B
if (puntos[node,'Categoria']=='A'|puntos[node,'Categoria']=='B') {

  visitas.node <- visitas[,node]
  
  #remove the actual point
  visitas.node[jor2extract]<-0
  
  #lenth 41 ones where visits
  jor2add <- mask.visitas(visitas.node)
  tour2add.id <- which(jornadas %in% jor2add)
  
  tour2add <- sample(tour2add.id, 1,prob=t.m[tour2add.id,'av.time'])
} else {
  tour2add <- sample(1:nrow(t.m), 1,prob=t.m[,'av.time'])
}

route2a <- sol[[tour2add]]

pos2a  <- where2add(route2a,node,saving)

  if (pos2a>0) {
    #add the node in tour2a
    sol[[tour2add]] <- add.node(sol[[tour2add]],node, pos2a)
    
    #update times
    #recalculate the t.m add
    t.m[tour2add,] <- tiempos.total(sol[[tour2add]])
        
    #update visitas
    if (jornadas[tour2add]!=jornadas[tour2extract]) {
      visitas[jornadas[tour2add],node] <- 1
      try(if(visitas[jornadas[tour2extract],node]!=1) stop ("something went wrong"))
      visitas[jornadas[tour2extract],node]<- 0
    }
    
    #remove node from tour2extract
    sol[[tour2extract]] <- remove.node(sol[[tour2extract]],pos2e)
    #update visitas
    
    #what happens whan the sol tourtoextract is empty.
    if (length(sol[[tour2extract]])==0){
      #print('removing the last node')
      #remove prom sol and from t.m
      sol[[tour2extract]] = NULL
      t.m <- t.m[-tour2extract,]
      jornadas <- jornadas[-tour2extract]
    } else { #tour2extract is not empty
      #recalculate tm in tour2extract
      t.m[tour2extract,] <- tiempos.total(sol[[tour2extract]])
    } 
  } else {     #continues all cases where pos>0  
  }
  k <- k+1
} #for k

#### 05. balancing the solution ####
#idea 1. move the nodes b c d x from the longest cycles. (in time and in nodes)
#idea 2. cut in half the longest cycles. 
jor2create <-max.jor - tab.jor
jor2create.tot <- sum(jor2create)
for (i in (turnos+1):(turnos+jor2create.tot)){
  sol[[i]] <- c('Base')
}
jornadas[(turnos+1):(turnos+jor2create.tot)] <- rep(1:41, jor2create)

#recalculate t.m
t.m <- do.call(rbind, lapply(sol,tiempos.total))
colnames(t.m) <- c('t.transport','t.pozo','n.points','av.time')

t<-1
while (t < 3*iterations) {
  #to do
  #select one tour to extract
  max.points <- max(t.m[,'n.points'])
  prob2e =  tiempo_jor - t.m[,'av.time']
  prob2e <- prob2e *as.numeric(t.m[,'n.points']>2)
  
  id2extract <- sample(1:nrow(t.m),1, prob =  prob2e)
  
  #select a node to extract
  route2extract <- sol[[id2extract]]
  node2extract.id <- sample(1:length(route2extract),1,prob=tiempos.saved(route2extract))
  node <- route2extract[node2extract.id]
  
  #select a tour to add
  id2add <- sample(1:nrow(t.m),1, prob = t.m[,'av.time'])
  
  #select a id to add
  route2add <- sol[[id2add]]
  
  go.on <- !(puntos[node,"Categoria"] %in% c('A','B'))

  if (go.on) {  
  
  #what happens if it is base
    if (route2add[1] == 'Base') {
      #add the node to the new cylce removing Base
      sol[[id2add]] <- c(node)
      #remove the node from the extracting cycle
      sol[[id2extract]] <- remove.node(sol[[id2extract]],node2extract.id)
      
      #update visitas
      verification <- (visitas[jornadas[id2extract],node] == 1)
      print(verification)
      visitas[jornadas[id2extract],node] <- 0
      visitas[jornadas[id2add],node] <- 1
      
      #update t.m
      t.m[id2extract,] <- tiempos.total(sol[[id2extract]])
      t.m[id2add,] <- tiempos.total(sol[[id2add]])
    }
    
    #what happens if it is not base
    else {
      #verify condition
      t.s <- tiempos.saved(route2extract)
      saving <- t.s[node2extract.id]
      
      if (length(route2add)==1) {
        factor.savings <- 4
      }
      
      saving <- factor.savings*saving
      #what condition has to be satisfied to 
      pos2add <- where2add(  route2add ,node,saving )
      
      
      if(pos2add>0) {
        if(length(route2extract)>2) {
          
          print('Updating solution !!!!')
          
          #add the node
          sol[[id2add]] <- add.node(sol[[id2add]] ,node, pos2add)
                    
          #remove the node
          sol[[id2extract]] <- remove.node(sol[[id2extract]],node2extract.id)
          
          #update visitas
          verification <- (visitas[jornadas[id2extract],node] == 1)
          print('sa')
          print(verification)
          visitas[jornadas[id2extract],node] <- 0
          visitas[jornadas[id2add],node] <- 1
          
          #update t.m
          t.m[id2extract,] <- tiempos.total(sol[[id2extract]])
          t.m[id2add,] <- tiempos.total(sol[[id2add]])
        }
      }
    }
  }
print (t)

  
  t <- t +1
}

#### 05. Solution analysis and display ####

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
  for (i in 2:length(route)){
    route.di <- rownames(as.matrix(get.shortest.paths(g,as.numeric(V(g)[route[i-1]]),as.numeric(V(g)[route[i]]), weights = rutas$tiempo)$vpath[[1]]))
    route.di.p <- paste(complete.lines(route.di),collapse=",")
    route.tot <- paste(route.tot, route.di.p,sep=";")
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

#sort data frame
data.out<-data.out[ order(data.out[,'jornada']), ]

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

#export data.out to a excel
WriteXLS(data.out, "modelo_resuelto.xlsx")

tapply(data.out$av.time,data.out$cuadrilla,sum)
