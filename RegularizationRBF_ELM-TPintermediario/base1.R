rm(list=ls())
set.seed(123)
library('corpcor')
library('mlbench')
library('plot3D')
library('glmnet')
library('caret')
library(mlbench)

## RBF com regularização
#Funcao RBF
trainRBF <- function(xin,yin,p,lambda){
    
    # aplica a PDF nos dados -> OBRENÇAO DA MATRIX H 
    pdfnvar<-function(x,m,K,n){
      if(n==1){
        r<-sqrt(as.numeric(K))
        px <- (1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))^2)
      }
      else px <- ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
      return(px)
    } 
    calcLOO <- function(P,y,N) ((t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/N)
    
    # pega as dimensoes de entrada
    N <- dim(xin)[1]
    n <- dim(xin)[2]
    xin <- as.matrix(xin)
    yin <- as.matrix(yin)
    
    # aplica clustering nos dados com k-means nativo do R
    dataclustering <- kmeans(xin,p)
    
    centers <- as.matrix(dataclustering$centers) # armazena os centros encontrados
    covlist <- list()
    
    # estima matriz de covariancia para cada um dos centros
    for(i in 1:p){
      ici <- which(dataclustering$cluster == i)
      xci <- xin[ici,]
      if (n == 1)
        covi <- var(xci)
      else covi<-cov(xci)
      covlist[[i]] <- covi
    }
    
    #determina matriz H
    H <- matrix(nrow = N,ncol = p)
    for (j in 1:N) {
      for(i in 1:p){
        mi<-centers[i,]
        covi <- covlist[i]
        covi<-matrix(unlist(covlist[i]),ncol = n,byrow=T) + 0.001 * diag(n)
        H[j,i] <- pdfnvar(xin[j,],mi,covi,n)
      }  
    }
    
    Haug <- cbind(H,1)
    # calcula W usando a pseudoinversa de Haug e COM regularizacao
    L <- lambda * diag(p)
    A <- t(H) %*% H + L
    P <- (diag(N) - H %*% solve(A) %*% t(H))
    LOO <- calcLOO(P,yin,N)
    # encontrando os pesos com regularização 
    W <- solve(A)%*% t(H) %*% yin
    
    return(list(centers,covlist,W,H,A,P,LOO))
    
  }
YRBF <- function(xin,modelRBF,binary){
  # declara a funcao gaussiana radial 
  pdfnvar<-function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))
  
  # pega as dimensoes de entrada
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  xin <- as.matrix(xin)
  
  centers<-as.matrix(modelRBF[[1]])
  covlist <- modelRBF[[2]]
  p <- length(covlist) # numero de funcoes radiais
  W <- modelRBF[[3]]
  
  #determina matriz H
  H <- matrix(nrow = N,ncol = p)
  for (j in 1:N) {
    for(i in 1:p){
      mi<-centers[i,]
      covi <- covlist[i]
      covi<-matrix(unlist(covlist[i]),ncol = n,byrow=T) + 0.001 * diag(n)
      H[j,i] <- pdfnvar(xin[j,],mi,covi,n)
    }  
  }
  
  #Haug <- cbind(H,1)
  Yhat <- H %*% W
  
  if(binary==TRUE)
    return(sign(Yhat))
  else
    return(Yhat)
}
## ELM COM regularizaçao 

# função ELM 
treinaELM <- function(Xin, Yin, p,lambda){
  #pega as dimensões 
  
  N <- dim(Xin)[1]
  n <- dim(Xin)[2]
  
  # coloca 1 para termo de polarizacao
  
  Xaug <- cbind(Xin,1)
  #cria Z aleatoriamente 
  Z <- replicate(p,runif((n+1),-0.5,0.5))
  
  # calcula H
  H <- tanh(Xaug %*% Z)
  
  L <- lambda * diag(p)
  A <- t(H) %*% H + L
  P <- (diag(N) - H %*% solve(A) %*% t(H))
  
  # encontrando os pesos com regularização 
  W <- solve(A)%*% t(H) %*% Yin
  
  #calcula pseudoinversa
  #Hinv <- pseudoinverse(H)
  
  # calcula w 
  #W <- Hinv %*% Yin
  
  return(list(W,H,Z))
  
}
YELM <- function(Xin,Z,W){
  n <- dim(Xin)[2]
  Xaug <- cbind(Xin,1)
  H <- tanh(Xaug %*% Z)
  Yhat <- sign(H%*%W) # retorna -1,0 ou 1 de acordo com o sinal do numero calculado
  return(Yhat)
  
}

calcA <- function(H,L) (t(H) %*% H + L)
calcP <- function(H,A,N) (diag(N) - H %*% solve(A) %*% t(H))
calcLOO <- function(P,y,N) ((t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/N)
calcGCV <- function(P,y,N) (N * (t(y) %*% (P %*% P) %*% y)/(sum(diag(P)))^2)

#Funçao SINC RBF
x <- as.matrix(runif(500,-15,15))
#x2 <- seq(-15,15,0.05) + 0.001
#y2 <- data.matrix(sin(x2)/x2) 
y <- as.matrix(sin(x)/x + rnorm(500,0,0.05))
p <- 40
model <- trainRBF(as.matrix(x),as.matrix(y),p,lambda = 0)
H <- model[[4]]
N<-length(x)

####### Varia lambda e gera soluções ########
lseq<-seq(0,2,0.1)
nl<-length(lseq)
mobjs<-matrix(nrow=nl,ncol=2)
mw<-matrix(nrow=nl,ncol=p)
cl<-0
Alist<-list()
Plist<-list()
LOOLambda<-matrix(nrow = nl,ncol = 1)
GCVLambda<-matrix(nrow = nl,ncol = 1)


for (i in lseq)
{
  cl<-cl+1
  w<-solve((t(H) %*% H) + i*diag(p)) %*% t(H) %*% y
  Alist[[cl]] <- calcA(H,i*diag(p))
  Plist[[cl]] <- calcP(H,Alist[[cl]],N)
  LOOLambda[cl] <- calcLOO(Plist[[cl]],y,N)  
  GCVLambda[cl] <- calcGCV(Plist[[cl]],y,N)
  
  #mobjs[cl,2]<-sum(H %*% w - y)^2
  #mobjs[cl,1]<-t(w) %*% w
  mw[cl,]<-w
}

emw<-matrix(nrow=nl,ncol=1)
cl<-0
#model2 <- trainRBF(as.matrix(x2),y2,p,0)
#H2 <- model2[[4]] 
for (i in (1:nl))
{
  cl<-cl+1
  w<-as.matrix(mw[i,])
  yhatr<-H %*% w  
  emw[cl]<-sum((yhatr-y)^2)
  #plot(x,y,xlim=c(-15,15),ylim=c(-0.5,1.5),xlab="x",ylab="f(x),h1(x),h2(x)")
  #par(new=T)
  #plot(x,yhatr,col='red',xlim=c(-15,15),ylim=c(-0.5,1.5),xlab="x",ylab="f(x),h1(x),h2(x)")
  #par(new=T)
  
}

bestsolemw<-which.min(emw)
bestLambdaemw <- lseq[bestsolemw]
w<-as.matrix(mw[bestsolemw,])
yhatr<-H %*% w  
par(new=T)
plot(x,yhatr,col='blue',xlim=c(-15,15),ylim=c(-0.5,1.5),xlab="x",ylab="f(x),h1(x),h2(x)")

xrange <- x
Hrange <- H
xr <- 15
xl <- -15
#par(new=T)
#for (i in 1:p)
#{
#  plot(xrange,Hrange[,i],col='blue',xlim=c(xl,xr),ylim=c(-0.5,1.5),xlab='',ylab='')
#  par(new=T)
#}
#par(new=F)  
plot(lseq,emw,type = 'b')

##############
bestsol<-which.min(LOOLambda)
w<-as.matrix(mw[bestsol,])
yhatrbest<-Hrange %*% w  
plot(xrange,yhatrbest,col='yellow',xlim=c(xl,xr),ylim=c(-0.5,1.5),xlab="x",ylab="f(x),h1(x),h2(x)")
par(new=T)
plot(xrange,y,col='green',xlim=c(xl,xr),ylim=c(-0.5,1.5),xlab="x",ylab="f(x),h1(x),h2(x)")

bestLambda <- lseq[bestsol]

plot(lseq,LOOLambda,type='b')

print(bestsol)
print(bestLambda)


