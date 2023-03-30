rm(list=ls())
library('plot3D')

pdfnvar<-function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))

# x -> vetor de dados
# m -> vetor de medias
# K -> matriz de covariancia
# n -> dimensao (nao precisava)

s1<-0.2 #std1
s2<-0.2 #std2
ro<-0 #correlacao

K1<-matrix(c(s1^2,ro*s1*s2,ro*s2*s1,s2^2),byrow = T, ncol=2)

s21<-0.2 #std1
s22<-0.2 #std2
ro2<-0 #correlacao

K2<-matrix(c(s21^2,ro2*s21*s22,ro2*s22*s21,s22^2),byrow = T, ncol=2)


n<-2


## Conjunto de Treinamento ------- 

N <- 30

xc1 <- matrix(rnorm(2*N),ncol = 2)*0.4 +2
xc2 <- matrix(rnorm(2*N),ncol = 2)*0.7 +4

plot(xc1[,1],xc1[,2],col='red',xlim = c(0,6),ylim = c(0,6))
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim = c(0,6),ylim = c(0,6))

# define hiper parametros 

p1<-3
p2<-4

h<-0.5 # estimar por meio dos dados depois

centrosc1 <- list()
centrosc2 <- list()

## escolher centros

# classe 1 
N1 <- dim(xc1)[1]
iseq <- sample(N1)

for (i in 1:p1) {
  centrosc1[[i]] <- xc1[iseq[i],]
}

# classe 2 
N2 <- dim(xc2)[1]
iseq <- sample(N2)

for (i in 1:p2) {
  centrosc2[[i]] <- xc1[iseq[i],]
}

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)

M1 <- matrix(1,nrow = length(seqi),ncol = length(seqj))
M2 <- matrix(1,nrow = length(seqi),ncol = length(seqj))
M3 <- matrix(1,nrow = length(seqi),ncol = length(seqj))


ci <- 0
for (i in seqi) {
  ci<-ci+1
  cj<-0
  for (j in seqj) {
    pxc1 <- 0
    pxc2 <- 0
    cj<-cj+1
    x<-as.matrix(c(i,j),byrow=T,ncol=1)
    for (k in 1:p1) pxc1 <- pxc1 + pdfnvar(x,matrix(centrosc1[[k]],ncol = 1),K1,n)
    M1[ci,cj]<- pxc1
    for (k in 1:p2) pxc2 <- pxc2 + pdfnvar(x,matrix(centrosc2[[k]],ncol = 1),K2,n)
    M2[ci,cj]<- pxc2
    
    M3[ci,cj] <- 1.0*(pxc1>pxc2) # classificador
    
  }
  
}

contour(seqi,seqj,M1)
persp3D(seqi,seqj,M1)

contour(seqi,seqj,M2)
persp3D(seqi,seqj,M2)

contour(seqi,seqj,M3)
persp3D(seqi,seqj,M3)

ribbon3D(seqi,seqj,M3,clim = c(0,2),colkey = F)
scatter3D(xc1[,1],xc1[,2],matrix(0,nrow = dim(xc1)[1]),add=T,col='blue',colkey = F)
scatter3D(xc2[,1],xc2[,2],matrix(0,nrow = dim(xc2)[1]),add=T,col='red',colkey = F)

xall <- rbind(xc1,xc2)
yall <- rbind(matrix(-1,nrow = dim(xall)[1]/2,ncol = 1), matrix(1,nrow = dim(xall)[1]/2,ncol = 1))

# calcula matrix H

H <- matrix(nrow = (2*N),ncol = (p1+p2))

for (i in 1:(2*N)) {
  x<-as.matrix(xall[i,])
  for (k in 1:p1) H[i,k] <- pdfnvar(x,matrix(centrosc1[[k]],ncol = 1),K1,n)
  for (k in (p1+1):(p1+p2)) H[i,k] <- pdfnvar(x,matrix(centrosc2[[k-p1]],ncol = 1),K2,n)
  
}

# pseudoinversa

lambda<-0.1
W <- (solve(t(H) %*% H + lambda*diag(1,(p1+p2),(p1+p2))) %*% t(H)) %*% yall
yhatall <- sign(H %*% W)




