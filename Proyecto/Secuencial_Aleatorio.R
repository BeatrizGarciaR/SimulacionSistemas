tiempo.p1 <- data.frame()
for(g in 1:5){
  t1 <- Sys.time()
  nucleos <- 8
  tareas <- 1000
  
  tiempo.tareas <- data.frame()
  tiempo.tareas <- cbind(sample(nucleos, tareas, replace=TRUE), round(runif(1:tareas, 1, 20)))
  colnames(tiempo.tareas) <- c("Nucleo","Tiempo")
  tareas.original <- tiempo.tareas
  colnames(tareas.original) <- c("Nucleo","Tiempo")
  
  tiempo.nucleos <- data.frame()
  tiempo.nucleos <- cbind(1:nucleos, rep(sum(tiempo.tareas[,2]), nucleos))
  tiempo.nucleo <- tiempo.nucleos
  colnames(tiempo.nucleos) <- c("Nucleo", "Tiempo maximo")
  
  datos <- data.frame()
  datos.aux <- data.frame(nucleo=double(), tarea=double(), tiempo.tarea=double(), tiempo.ingreso=double(), tiempo.nucleo=double(), tiempo.salida=double(), activo=character())
  for(i in 1:nucleos){
    datos.aux <- cbind(1:nucleos, 0, 0, 0, 0, 0, "No")
  }
  colnames(datos.aux) <- c("Nucleo", "Tarea", "Tiempo Tarea", "Momento Ingreso", "Tiempo en nucleo", "Momento Salida", "Activo")
  
  s <- 0
  c <- 0
  for(k in 1:round(tareas*2.8)){
    for(i in 1:tareas){
      for(j in sample(1:nucleos)){
        if(tiempo.tareas[i,1] < tiempo.nucleo[j,1]+1){
          if(datos.aux[j,7]=="No"){
            datos.aux[j,] <- c(datos.aux[j,1], i, tiempo.tareas[i,2], k-1, c, s, "Si")
            tiempo.nucleos[j,2] <- tiempo.nucleos[j,2] - tareas.original[i,2]
            tiempo.tareas[i,] <- Inf 
            break 
          }
          break
        }
      }
    }
    
    for(p in 1:nucleos){
      if(k != 1){
        if(datos.aux[p,5]!= datos.aux[p,3]){
          datos.aux[p,5] <- as.numeric(datos.aux[p,5]) + 1
          if(datos.aux[p,5]==datos.aux[p,3]){
            if(datos.aux[p,7]=="Si"){
              datos.aux[p,6] <- k
              datos <- rbind(datos, as.numeric(datos.aux[p,1:6]))
              datos.aux[p,] <- c(datos.aux[p,1], 0, 0, 0, 0, 0, "No")
            }
          }
        }
      }
    }
  }
  
  for(l in 1:length(datos[,1])){
    if(datos[l,4] == 0){
      datos[l,4] <- 1
    }
  }
  colnames(datos) <- c("Nucleo", "Tarea", "Tiempo Tarea", "Momento Ingreso", "Tiempo en nucleo", "Momento Salida")
  
  a <- datos[,6]-datos[,4]
  
  q <- c()
  for(n in 1:nucleos){
    r <- 0
    for(m in 1:length(datos[,1])){
      if(datos[m,1]==n){
        r <- r + 1
      }
    }
    q <- c(q, r)
  }
  
  q1 <- c()
  for(n1 in 1:nucleos){
    r1 <- 0
    for(m1 in 1:length(tareas.original[,1])){
      if(tareas.original[m1,1]==n1){
        r1 <- r1 + 1
      }
    }
    q1 <- c(q1, r1)
  }
  
  ash <- data.frame()
  ash <- cbind(q1,q)
  
  png(paste("EjecuciónRandom_",g,".png"))
  t <- tiempo.nucleo[,2] - tiempo.nucleos[,2]
  barplot(t, main="Tiempo de ejecución", sub=paste(tareas,"tareas,",nucleos,"núcleos"), xlab="Núcleos", ylab="Tiempo de ejecución", col="pink")
  graphics.off()
  
  png(paste("ComparaciónRandom_",g,".png"))
  barplot(t(ash), beside=T, main=paste(tareas,"tareas,",nucleos,"núcleos"), xlab="Núcleos", ylab="Cantidad de tareas", legend=c("Inicio", "Asignada"))
  graphics.off()
  
  t2 <- Sys.time()
  time <- difftime(t2,t1,units="secs")
  print(time)
  tiempo.p1 <- rbind(tiempo.p1, time)
}

ttiempo.p1 <- as.vector(t(tiempo.p1))
plot(ttiempo.p1, xlab="Repetición", ylab="Tiempo de ejecución", type="o", col="green",pch=19, main="Tiempo en segundos")
#lines(ttiempo.p1, type="o", col="green", pch=19)
