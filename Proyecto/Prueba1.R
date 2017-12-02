nucleos <- 4
tareas <- 15

#####TIEMPO DE TAREAS#####
tiempo.tareas <- data.frame()
tiempo.tareas <- cbind(sample(nucleos, tareas, replace=TRUE), round(runif(1:tareas, 1, 50)))
colnames(tiempo.tareas) <- c("Nucleo","Tiempo")
Nucleo <- sort(tiempo.tareas[,1], decreasing = TRUE)
aux.tareas <- data.frame()
aux.tareas <- cbind(Nucleo,tiempo.tareas[,2])

#####TIEMPO DE NUCLEOS#####
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
for(i in 1:nucleos){
  suma.tarea <- 0
  for(j in 1:tareas){
   if(tiempo.nucleo[i,1]==aux.tareas[j,1]){       
     suma.tarea <- suma.tarea + aux.tareas[j,2]
     if(suma.tarea <= suma.mayor){
        tiempo.nucleo[i,2] <- tiempo.nucleo[i,2] - aux.tareas[j,2]
        tarea.realizada[j] <- 1
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

#####VECTOR SOLO DE MAYOR NUCLEO (INCOMPLETO)#####
###PRUEBA (YA HACE QUE SE ELIMINE BIEN)
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
  if(vec.aux[i]==FALSE){
    indice <- indice + 1
  }
}
tareas.pendientes <- tareas.pendientes[-(indice),]

tarea.realizada2 <- rep(0, tareas)
tareas.pendientes2 <- data.frame()
nucleo.p2 <- c()
tiempo.p2 <- c()
for(i in 2:n+1){
  suma.tarea2 <- 0
  for(j in 1:length(tareas.pendientes[,1])){
    if(tiempo.nucleo[i,1]==tareas.pendientes[j,1]){       
      suma.tarea2 <- suma.tarea2 + tareas.pendientes[j,2]
      if(suma.tarea2 <= m){
        tiempo.nucleo[i,2] <- tiempo.nucleo[i,2] - tareas.pendientes[j,2]
        tarea.realizada2[j] <- 1
      }
      else {
        suma.tarea2 <- suma.tarea2 - tareas.pendientes[j,2]
        nucleo.p2 <- c(nucleo.p2, tareas.pendientes[j,1])
        tiempo.p2 <- c(tiempo.p2, tareas.pendientes[j,2])
        tareas.pendientes2 <- cbind(nucleo.p2, tiempo.p2)
      }
    }
  }
}
