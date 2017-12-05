nucleos <- 10
tareas <- 100

#####TIEMPO DE TAREAS#####
tiempo.tareas <- data.frame()
tiempo.tareas <- cbind(sample(nucleos, tareas, replace=TRUE), round(runif(1:tareas, 1, 50)))
colnames(tiempo.tareas) <- c("Nucleo","Tiempo")
Nucleo <- sort(tiempo.tareas[,1], decreasing = TRUE)
aux.tareas <- data.frame()
aux.tareas <- cbind(Nucleo,tiempo.tareas[,2])

#####TIEMPO DE NUCLEOS#####+
tiempo.nucleos <- data.frame()
tiempo.nucleos <- cbind(nucleos:1, rep(sum(aux.tareas[,2]), nucleos))
tiempo.nucleo <- tiempo.nucleos
colnames(tiempo.nucleos) <- c("Nucleo", "Tiempo maximo")

#####SUMA TOTAL DE TAREAS CON EL NUCLEO MAYOR#####
suma.mayor <- 0
for(k in 1:tareas){
  if(aux.tareas[k,1]==nucleos){
    suma.mayor <- suma.mayor + aux.tareas[k,2]
  }
}

#####ASIGNACION DE TAREAS EN EL TIEMPO CERO#####
tarea.realizada <- rep(0, tareas)
tareas.pendientes <- data.frame()
nucleo.p <- c()
tiempo.p <- c()
t.n1 <- c()
t.n2 <- c()
for(i in 1:nucleos){
  suma.tarea <- 0
  for(j in 1:tareas){
   if(tiempo.nucleo[i,1]==aux.tareas[j,1]){       
     suma.tarea <- suma.tarea + aux.tareas[j,2]
     if(suma.tarea <= suma.mayor){
        tiempo.nucleo[i,2] <- tiempo.nucleo[i,2] - aux.tareas[j,2]
        tarea.realizada[j] <- 1
        t.n1 <- c(t.n1, nucleos+1-i)
        t.n2 <- c(t.n2, aux.tareas[j,2])
     }
     else {
       suma.tarea <- suma.tarea - aux.tareas[j,2]
       nucleo.p <- c(nucleo.p, aux.tareas[j,1])
       tiempo.p <- c(tiempo.p, aux.tareas[j,2])
       tareas.pendientes <- cbind(nucleo.p, tiempo.p)
     }
   }
  }
}

nucleo.tarea <- cbind(t.n1, t.n2)
data <- data.frame()
for(j in 1:nucleos){
  suma <- 0
  for(i in 1:length(nucleo.tarea[,1])){
    if(nucleo.tarea[i,1]==j){
      suma <- suma + nucleo.tarea[i,2]
    }
  }
  data <- rbind(data, suma)
}

barplot(t(data), col="sky blue")

#####SEGUNDA ETAPA DE ASIGNACION###
#while(length(tareas.pendientes)==0){
n <- max(tareas.pendientes[,1])
suma.mayor2 <- 0
for(i in 1:length(tareas.pendientes[,1])){
  if(tareas.pendientes[i,1]==n){
    suma.mayor2 <- suma.mayor2 + 1
  }
}
m <- max(tareas.pendientes[1:suma.mayor2, 2])
tiempo.nucleo[1,2] <- tiempo.nucleo[1,2] - m
vec.aux <- tareas.pendientes[1:suma.mayor2,2]==m
indice <- 1
for(i in 1:suma.mayor2){
  if(vec.aux[i]!=TRUE){
    indice <- indice + 1
  }
  else{
    break
  }
}
t.n12 <- c(nucleos)
t.n22 <- c(tareas.pendientes[indice,2])
tareas.pendientes <- tareas.pendientes[-(indice),]

tareas.pendientes2 <- data.frame()
nucleo.p2 <- c()
tiempo.p2 <- c()

###########################################
####CHECAR BIEN############################
###########################################


# for(i in 2:nucleos){
#   suma.tarea2 <- 0
#   for(j in 1:length(tareas.pendientes[,1])){
#     if(tiempo.nucleo[i,1]>=tareas.pendientes[j,1]){
#       suma.tarea2 <- suma.tarea2 + tareas.pendientes[j,2]
#       if(suma.tarea2 <= m){
#         tiempo.nucleo[i,2] <- tiempo.nucleo[i,2] - tareas.pendientes[j,2]
#         t.n12 <- c(t.n12, nucleos+1-i)
#         t.n22 <- c(t.n22, tareas.pendientes[j,2])
#         break
#       }
#       else {
#         suma.tarea2 <- suma.tarea2 - tareas.pendientes[j,2]
#         nucleo.p2 <- c(nucleo.p2, tareas.pendientes[j,1])
#         tiempo.p2 <- c(tiempo.p2, tareas.pendientes[j,2])
#         tareas.pendientes2 <- cbind(nucleo.p2, tiempo.p2)
#       }
#     }
#   }
# }

###########################################
####CHECAR BIEN############################
###########################################

nucleo.tarea2 <- cbind(t.n12, t.n22)
data2 <- data.frame()
for(j in 1:nucleos){
  suma2 <- 0
  for(i in 1:length(nucleo.tarea2[,1])){
    if(nucleo.tarea2[i,1]==j){
      suma2 <- suma2 + nucleo.tarea2[i,2]
    }
  }
  data2 <- rbind(data2, suma2)
}

barplot(t(data2), col="pink")


#}