tiempo.p <- data.frame()
for(rep in 1:5){
  
time1 <- Sys.time()

library(testit)
library(parallel)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + max
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

mutar <- function(i){
  if (runif(1) < pm) {
    a <- mutacion(p[i,], n)
    return(a)
  }
  else{
    return(NULL)
  }
}

reproducir <- function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  a <- hijos[1:n] # primer hijo
  b <- hijos[(n+1):(2*n)] # segundo hijo
  c <- rbind(a, b)
  return(as.data.frame(c))
}

objetivos <- function(i){
  return(objetivo(p[i,], valores))
}

factibles.f <- function(i){
  return(factible(p[i,], pesos, capacidad))
}

pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- 200
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
pm <- 0.05
rep <- 50
tmax <- 50
mejores <- double()
cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, c("mutar", "reproducir", "objetivos", "valores", "pesos", "capacidad" ,"factibles.f", "factible", "objetivo", "pm", "mutacion", "reproduccion", "n"))
for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  clusterExport(cluster, "p")
  mutados <- parSapply(cluster, 1:tam, mutar)
  for(i in 1:length(mutados)){ 
    if(!is.null(mutados[[i]])){
      p <- rbind(p, mutados[[i]])
    }
  }
  
  clusterExport(cluster, c("p", "tam"))
  h <- parSapply(cluster, 1:rep , reproducir)
  for(i in 1:rep){
    a <- as.data.frame(h[,i])
    b <- a[1, 1:50]
    colnames(b) <- names(p)
    c <- a[1, 51:100]
    colnames(c) <- names(p)
    p <- rbind(p, b)
    p <- rbind(p, c)
  }
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  clusterExport(cluster, c("p", "tam", "factibles.f"))
  obj <- parSapply(cluster, 1:tam, objetivos)
  fact <- parSapply(cluster, 1:tam,  factibles.f)
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}
stopCluster(cluster)
png("p10p.png", width=600, height=300)
plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
points(1:tmax, mejores, pch=15)
abline(h=optimo, col="green", lwd=3)
graphics.off()
print(paste(mejor, (optimo - mejor) / optimo))
time2 <- Sys.time()
time <- time2 - time1
print(time)
tiempo.p <- rbind(tiempo.p, time)
}
ttiempo.p <- as.vector(t(tiempo.p))
plot(ttiempo.p, xaxt="n", ylim=c(1,7), col=4, xlab="Repeticiones",type="o", ylab="Tiempo de ejecución")

