inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png") # dibujamos f(x) para ver como es
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

desde <- 3
hasta <- 7
pedazo <- 50000
parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
replicas <- 5
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

Aproximacion <- data.frame()
muestra<-c(500, 1000, 2500, 5000, 9000, 15000, 36000)
valor.integral <- c()
for (cantidad in muestra){
  for (n in 1:replicas){
cuantos <- cantidad
tiempo1 <- Sys.time()
montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
tiempo2 <- Sys.time()
integral <- sum(montecarlo) / (cuantos * pedazo)
valor.integral <- ((pi / 2) * integral)
Aproximacion <- rbind(Aproximacion, c(valor.integral, tiempo2-tiempo1))
}
}
stopImplicitCluster()
colnames(Aproximacion)<-c("Integral","Tiempo")

plot(Aproximacion[,1], xlab="Distintos tamaños de muestras", ylab="Valor de Integral")
segments(0, 0.048834, replicas*length(muestra), 0.048834, col=2)
