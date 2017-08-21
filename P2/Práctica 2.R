proba <- c(seq(0.1, 0.9, 0.1))
datos <- data.frame()
for (prob in proba){
  
vector <- c()
for (i in 1:15){
library(parallel)
dim <- 10
num <-  dim^2
r <- rbinom(num, size=1, prob)
actual <- matrix(r, nrow=dim, ncol=dim)
suppressMessages(library("sna"))

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:9) {
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  if (sum(siguiente) == 0) { # todos murieron
    break;
  }
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
}
vector[i] <- iteracion
stopCluster(cluster)
}
datos <- rbind(datos, vector)
}
png("Imagen.png")
boxplot(data.matrix(datos), use.cols=FALSE, ylab="Iteraciones", xlab="Probabilidades", names=c("0,1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9"))
graphics.off()