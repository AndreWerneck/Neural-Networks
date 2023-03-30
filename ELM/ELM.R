# EX1 ----- ---------------------------------------------------------------

# gerando os dados de entrada

library('mlbench')
library('corpcor')
library('plot3D')

#cd0 <- mlbench.2dnormals(200)

#cd1 <- mlbench.xor(100)

#cd2 <- mlbench.circle(100)

#cd3 <- mlbench.spirals(100,sd = 0.05)


amostras <- 50
x1 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(2,2)),ncol = amostras/2,nrow = 2))
x2 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(4,4)),ncol = amostras/2,nrow = 2))
x3 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(2,4)),ncol = amostras/2,nrow = 2))
x4 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(4,2)),ncol = amostras/2,nrow = 2))

Xred <- rbind(x1,x2)
Xblue <- rbind(x3,x4)

X <- rbind(Xred,Xblue)

Yred <- matrix(1,nrow = amostras)
Yblue <- matrix(-1,nrow = amostras)

Y <- rbind(Yred,Yblue)


# Plotando os dados 

plot(x1[,1],x1[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='red')
par(new=TRUE)
plot(x2[,1],x2[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='red')
par(new=TRUE)
plot(x3[,1],x3[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='blue')
par(new=TRUE)
plot(x4[,1],x4[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='blue')


# proximo passo é gerar a matriz Z

p <- 20 

Z <- replicate(p,runif(3,-0.5,0.5)) 

# add termo de polarização 

Xaug <- cbind(X,1)

# calculando H 

H <- tanh(Xaug %*% Z) # tanh -> rotulos em 1 e -1!!!

# calculando a pseudo inversa 

Hinv <- pseudoinverse(H)

# calculando w 

W <- Hinv %*% Y

# calculando Yhat de treino 

Yhat_train <- sign(H%*%W)

e_train <- sum((Y-Yhat_train)^2)/4
#print(e_train)

# função ELM 
treinaELM <- function(Xin, Yin, p){
  #pega as dimensões 
  
  N <- dim(Xin)[1]
  n <- dim(Xin)[2]
  
  # coloca 1 para termo de polarizacao
  
  Xaug <- cbind(Xin,1)
  #cria Z aleatoriamente 
  Z <- replicate(p,runif((n+1),-0.5,0.5))
  
  # calcula H
  H <- tanh(Xaug %*% Z)
  
  #calcula pseudoinversa
  Hinv <- pseudoinverse(H)
  
  # calcula w 
  W <- Hinv %*% Yin
  
  return(list(W,H,Z))
  
}
# funcao para calcular saida da ELM
YELM <- function(Xin,Z,W){
  n <- dim(Xin)[2]
  Xaug <- cbind(Xin,1)
  H <- tanh(Xaug %*% Z)
  Yhat <- sign(H%*%W) # retorna -1,0 ou 1 de acordo com o sinal do numero calculado
  return(Yhat)
  
}

Yhat_train2 <- YELM(X,Z,W) 

accTrain <- 1 - (t(Y-Yhat_train2)%*%(Y-Yhat_train2))/dim(Y)[1]

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)

M <- matrix(0,nrow = length(seqi),ncol = length(seqj))

ci <- 0
for (i in seqi) {
  ci<-ci+1
  cj<-0
  for (j in seqj) {
    cj<-cj+1
    x<-as.matrix(cbind(i,j))
    M[ci,cj]<- YELM(x,Z,W)
    
  }
  
}

plot(Xred[,1],Xred[,2],col = 'red', xlim = c(0,6), ylim = c(0,6),xlab = 'x_1',ylab = 'x_2')
par(new=T)
plot(Xblue[,1],Xblue[,2],col = 'blue', xlim = c(0,6), ylim = c(0,6),xlab = '',ylab = '')
par(new=T)
contour(seqi,seqj,M,xlim = c(0,6),ylim = c(0,6),xlab= '', ylab='')

#persp3D(seqi,seqj,M,counter = T,theta=55,phi=30,r=40,d=0.1,expand=0.5,ltheta=90,lphi=180,shade=0.4,ticktype='detailed',nticks=5)



