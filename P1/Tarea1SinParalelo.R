start.process <- Sys.time()

repetir <- 100 #repeticiones
duracion <- 200 #pasos 

datos <-  data.frame()

for (dimension in 1:8) {
  resultado <- sapply(1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           suma <- 0
                           for (t in 1:duracion) {
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             
                             
                             origen <- rep (0, dimension)
                             
                             if (all(origen == pos)) {
                               suma <- suma + 1
                             }
                           }
                           return(suma)
                         })
  datos <- rbind(datos, resultado)
}

png("Dimension8.png")
boxplot(data.matrix(datos), use.cols=FALSE, ylab="Distancia m\u{E1}xima", xlab="Dimensi\u{F3}n", main="Dimension 1:30")
graphics.off()

end.process <- Sys.time()

Time.process <- end.process - start.process
print(Time.process)