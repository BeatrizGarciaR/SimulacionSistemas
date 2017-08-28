primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n > i) && (n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
desde <- 1000
hasta <- 3000
original <- desde:hasta
invertido <- hasta:desde
replicas <- 10
original.v <- data.frame()
invertido.v <- data.frame()
aleatorio.v <- data.frame()
obs.nucleos.v <- data.frame()

suppressMessages(library(doParallel))
for (j in 1:detectCores()){ 
  registerDoParallel(makeCluster(j))
  ot <-  numeric() #ordenamiento 1
  it <-  numeric() #ordenamiento 2
  at <-  numeric() #ordenamiento 3
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    tot <- matrix(ot)
    tit <- matrix(it)
    tat <- matrix(at)
  }
  original.v <- rbind(original.v, tot)
  invertido.v <- rbind(invertido.v, tit)
  aleatorio.v <- rbind(aleatorio.v, tat)
  stopImplicitCluster()
}
obs.nucleos.v <- rbind(original.v, invertido.v, aleatorio.v)

#Construccion para mejor vision de replicas por ordenamientos con distintos nucleos
nucleo1 <- cbind(original.v[1:replicas,], invertido.v[1:replicas,], aleatorio.v[1:replicas,])
nucleo2 <- cbind(original.v[(replicas+1):(2*replicas),], invertido.v[(replicas+1):(2*replicas),], aleatorio.v[(replicas+1):(2*replicas),])
nucleo3 <- cbind(original.v[(2*replicas+1):(3*replicas),], invertido.v[(2*replicas+1):(3*replicas),], aleatorio.v[(2*replicas+1):(3*replicas),])
nucleo4 <- cbind(original.v[(3*replicas+1):(4*replicas),], invertido.v[(3*replicas+1):(4*replicas),], aleatorio.v[(3*replicas+1):(4*replicas),])
#nucleo5 <- cbind(original.v[(4*replicas+1):(5*replicas),], invertido.v[(4*replicas+1):(5*replicas),], aleatorio.v[(4*replicas+1):(5*replicas),])
#nucleo6 <- cbind(original.v[(5*replicas+1):(6*replicas),], invertido.v[(5*replicas+1):(6*replicas),], aleatorio.v[(5*replicas+1):(6*replicas),])
#nucleo7 <- cbind(original.v[(6*replicas+1):(7*replicas),], invertido.v[(6*replicas+1):(7*replicas),], aleatorio.v[(6*replicas+1):(7*replicas),])
#nucleo8 <- cbind(original.v[(7*replicas+1):(8*replicas),], invertido.v[(7*replicas+1):(8*replicas),], aleatorio.v[(7*replicas+1):(8*replicas),])
Nucleos <- cbind(nucleo1, nucleo2, nucleo3, nucleo4) #nucleo5, nucleo6, nucleo7, nucleo8)
colnames(Nucleos) <- c(rep(seq(c("1","2","3")), detectCores()))

tiempo.prom <- data.frame()
for(k in 1:(3*detectCores())){
  time <- sum(Nucleos[,k])
  tiempo.prom <- rbind(tiempo.prom, time/replicas)
}

#Construccion para ver mejor el total de observaciones
Observaciones <- matrix(ncol=3, nrow=dim(obs.nucleos.v[1]))
Observaciones[,1] <- cbind(rep(c(seq(1:detectCores())),each=replicas))
Observaciones[,2] <- cbind(rep(c(seq(1:3)),each=(replicas*detectCores())))
Observaciones[,3] <- rbind(as.matrix(obs.nucleos.v))
colnames(Observaciones) <- c("Nucleo","Ordenamientos","TiempoEjecucion")

Promedio<-matrix( ncol=3,nrow = dim(tiempo.prom)[1])
Promedio[,1]<-cbind(rep(c(seq( 1: detectCores())), each = 3)) #numero de ordenamientos
Promedio[,2]<-cbind(rep(seq( 1:3 ), detectCores()))
Promedio[,3]<-rbind(as.matrix(tiempo.prom))
colnames(Promedio)<-c("Nucleos","Ordenamiento","TiempoPromedio")

boxplot(TiempoPromedio~Ordenamiento, data=Promedio,xlab="Ordenamiento")
boxplot(TiempoPromedio~Nucleos, data=Promedio, xlab="Nucleos")
boxplot(TiempoEjecucion~Ordenamientos, data=Observaciones, xlab="Ordenamientos")
boxplot(TiempoEjecucion~Nucleo, data=Observaciones, xlab="Nucleos")

barplot(as.matrix(t(Promedio[,3])),col=c(5,6,7), xlab="Ordenamientos con distintos nucleos", ylab="Tiempo promedio")
barplot(as.matrix(Nucleos), col=c(5,6,7), xlab="Ordenamientos con distintos nucleos", ylab="Tiempos de ejecucion")

#Comparaciones con la prueba de Kruskal Wallis
a <- kruskal.test(nucleo1~nucleo2)
b <- kruskal.test(nucleo1~nucleo3)
c <- kruskal.test(nucleo1~nucleo4) 
d <- kruskal.test(nucleo2~nucleo3)
e <- kruskal.test(nucleo2~nucleo4)
f <- kruskal.test(nucleo3~nucleo4)

print(a)
print(b)
print(c)
print(d)
print(e)
print(f)