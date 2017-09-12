suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
runs <- c(75000, 100000, 150000, 220000, 300000, 450000, 660000, 800000, 1000000, 2000000, 3000000, 4000000)

pi <- function(){
  xs <- runif(cantidad,min=-0.5,max=0.5)
  ys <- runif(cantidad,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/cantidad)*4
  return(mc.pi)
}

replicas <- 20
Aproximacion <- data.frame()
for (cantidad in runs){
  for (n in 1:replicas){
    cuantos <- cantidad
    tiempo1 <- Sys.time()
    aprox.pi <- foreach(i= replicas, .combine=c) %dopar% pi()
    tiempo2 <- Sys.time()   
    Aproximacion <- rbind(Aproximacion, c(aprox.pi, tiempo2-tiempo1))
  }
}
stopImplicitCluster()
colnames(Aproximacion)<-c("Pi","Tiempo")

plot(Aproximacion[,1], xlab="Distintas corridas", ylab="Aproximación de pi")
segments(0, 3.141592, replicas*length(runs), 3.141592, col=2)

plot(Aproximacion[,2], xlab="Distintas corridas", ylab="Tiempo de ejecución")
