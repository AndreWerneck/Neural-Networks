rm(list = ls())

### dados do tabuleiro

nc <- 30

xref <- matrix(rnorm(2*nc, mean = 0,sd = 0.4), ncol = 2)

clust1 <- xref + matrix(c(2,2),byrow = T,ncol = 2, nrow = nc)
clust2 <- xref + matrix(c(4,2),byrow = T,ncol = 2, nrow = nc)
clust3 <- xref + matrix(c(3,2),byrow = T,ncol = 2, nrow = nc)
clust4 <- xref + matrix(c(1,1),byrow = T,ncol = 2, nrow = nc)
clustall <- rbind(clust1,clust2,clust3,clust4)

plot(clustall[,1],clustall[,2])

### Define K 

k <- 4

### Escolhe K centros

iseq <- sample(nc*4)

centros <- clustall[iseq[1:k],]
plot(clustall[,1],clustall[,2],xlim = c(0,6),ylim = c(0,3))
par(new=T)
plot(centros[1,1],clustall[1,2],xlim = c(0,6),ylim = c(0,3),col = 'red')
par(new=T)
plot(centros[2,1],clustall[2,2],xlim = c(0,6),ylim = c(0,3),col = 'green')
par(new=T)
plot(centros[3,1],clustall[3,2],xlim = c(0,6),ylim = c(0,3),col = 'blue')
par(new=T)
plot(centros[4,1],clustall[4,2],xlim = c(0,6),ylim = c(0,3),col = 'yellow')

### Atribui Centros 
for (j in 1:100)
{
  auxmax1 <- matrix(centros[1,],byrow = T, ncol = 2, nrow = 4 * nc)
  auxmax2 <- matrix(centros[2,],byrow = T, ncol = 2, nrow = 4 * nc)
  auxmax3 <- matrix(centros[3,],byrow = T, ncol = 2, nrow = 4 * nc)
  auxmax4 <- matrix(centros[4,],byrow = T, ncol = 2, nrow = 4 * nc)
  
  D1 <- rowSums((auxmax1 - clustall)^2)
  D2 <- rowSums((auxmax2 - clustall)^2)
  D3 <- rowSums((auxmax3 - clustall)^2)
  D4 <- rowSums((auxmax4 - clustall)^2)
  
  Dall <- cbind(D1,D2,D3,D4)
  
  ci <- apply(Dall,1,which.min)
  
  centros[1,] <- colMeans(clustall[which(ci == 1),])
  centros[2,] <- colMeans(clustall[which(ci == 2),])
  centros[3,] <- colMeans(clustall[which(ci == 3),])
  centros[4,] <- colMeans(clustall[which(ci == 4),])
}

plot(clustall[which(ci == 1),1],clustall[which(ci == 1),2],xlim = c(0,6),ylim = c(0,3),col = 'red', xlab = 'X',ylab = 'Y')
par(new = T)
plot(clustall[which(ci == 2),1],clustall[which(ci == 2),2],xlim = c(0,6),ylim = c(0,3),col = 'blue',xlab = 'X',ylab = 'Y')
par(new = T)
plot(clustall[which(ci == 3),1],clustall[which(ci == 3),2],xlim = c(0,6),ylim = c(0,3),col = 'green',xlab = 'X',ylab = 'Y')
par(new = T)
plot(clustall[which(ci == 4),1],clustall[which(ci == 4),2],xlim = c(0,6),ylim = c(0,3),col = 'yellow',xlab = 'X',ylab = 'Y')



#par(new = T)
#plot(clustall[,1],clustall[,2],xlim = c(0,6),ylim = c(0,3))
#par(new=T)
#plot(centros[1,1],clustall[1,2],xlim = c(0,6),ylim = c(0,3),col = 'red',pch = 2)
#par(new=T)
#plot(centros[2,1],clustall[2,2],xlim = c(0,6),ylim = c(0,3),col = 'green',pch = 2)
#par(new=T)
#plot(centros[3,1],clustall[3,2],xlim = c(0,6),ylim = c(0,3),col = 'blue',pch = 2)
#par(new=T)
#plot(centros[4,1],clustall[4,2],xlim = c(0,6),ylim = c(0,3),col = 'yellow', pch = 2)

### function K-medias ------ 

k_means <- function(xin,p){
  
  ## xin é o conjunto de dados de entrada
  ## p ou k é o número de centros
  
  #algoritmo: selecionar um numero k de centros aleatoriamente, calcular a distancia entre os pontos e o centro mais próximo e ir classifciando dessa forma
  
  # pega as dimensoes
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  
  
}  
  





