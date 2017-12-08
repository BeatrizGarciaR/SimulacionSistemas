library(lattice)
nucleos <- 4
tareas <- 10

#####TIEMPO DE TAREAS#####
tiempo.tareas <- data.frame()
tiempo.tareas <- cbind(sample(nucleos, tareas, replace=TRUE), round(runif(1:tareas, 1, 20)))
colnames(tiempo.tareas) <- c("Nucleo","Tiempo")
tareas.original <- tiempo.tareas
colnames(tareas.original) <- c("Nucleo","Tiempo")

#####TIEMPO DE NUCLEOS#####
tiempo.nucleos <- data.frame()
tiempo.nucleos <- cbind(1:nucleos, rep(sum(tiempo.tareas[,2]), nucleos))
tiempo.nucleo <- tiempo.nucleos
colnames(tiempo.nucleos) <- c("Nucleo", "Tiempo maximo")

datos <- data.frame()
#datos <- data.frame(nucleo=double(), tarea=double(), tiempo.ingreso=double(), tiempo.salida=double(), tiempo.tarea=double())
# for(j in 1:tareas){
#   datos <- cbind(rep("NA", tareas), "NA", "NA", "NA", "NA")
# }
# colnames(datos) <- c("Nucleo", "Tarea", "Momento Ingreso", "Momento salida", "Tiempo Tarea")
datos.aux <- data.frame(nucleo=double(), tarea=double(), tiempo.tarea=double(), tiempo.ingreso=double(), tiempo.nucleo=double(), tiempo.salida=double(), activo=character())
for(i in 1:nucleos){
  datos.aux <- cbind(1:nucleos, 0, 0, 0, 0, 0, "No")
}
colnames(datos.aux) <- c("Nucleo", "Tarea", "Tiempo Tarea", "Momento Ingreso", "Tiempo en nucleo", "Momento Salida", "Activo")

####### ASIGNACION  ##########
s <- 0
c <- 0
for(k in 1:100){
  for(i in 1:tareas){
    for(j in 1:nucleos){
      if(tiempo.tareas[i,1] <= tiempo.nucleo[j,1]){
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
for(l in 1:tareas){
  if(datos[l,4]==0){
    datos[l,4] <- 1
  }
}
colnames(datos) <- c("Nucleo", "Tarea", "Tiempo Tarea", "Momento Ingreso", "Tiempo en nucleo", "Momento Salida")

a <- tiempo.nucleo[,2] - tiempo.nucleos[,2]
plot(datos[,1], datos[,2])
barplot(1:nucleos, a)

