rm(list = ls())
library('plot3D')

treinap <- function(xin,yin,eta,tol_parada,maxepocas){
  
  # pegando as dimensoes dos dados de entrada
  N<-dim(xin)[1]
  n<-dim(xin)[2]
  
  # adicionando coluna de 1s ao vetor de entrada
  xin<-cbind(xin,-1)
  # inicializando o vetor de pesos aleatoriamente
  w<-as.matrix(runif(n+1)-0.5)
  erroepoca<-tol_parada+10
  epocaatual<-0
  vetorEpocas <- matrix(nrow = 1,ncol = maxepocas)
  
  while((erroepoca>tol_parada)&&(epocaatual<maxepocas)) {
    indexvec = sample(N)
    erroQuad<-0
    for (i in 1:N) {
      #add estocasticidade ao treinamento
      index<-indexvec[i]
      #acha yhat
      yhat<-1*((xin[index,]%*%w)>=0)
      #calcula o erro
      erroatual<-yin[index,]-yhat
      w = w + eta*erroatual*xin[index,]
      erroQuad <- erroQuad + erroatual*erroatual
    }
    epocaatual <- epocaatual + 1 # atualiza as epocas
    vetorEpocas[epocaatual]<-erroQuad/N # erro medio de cada época
    erroepoca <- vetorEpocas[epocaatual] 
  }
  return(list(w,vetorEpocas[1:epocaatual]))
}

# EX1 ---- 
# gerando os dados 
x1 <- matrix(0.4*rnorm(200*2),ncol=2) + t(matrix((c(2,2)),ncol = 200,nrow = 2))
x2 <- matrix(0.4*rnorm(200*2),ncol=2) + t(matrix((c(4,4)),ncol = 200,nrow = 2))

# rotulando os dados 
y1 <- matrix(0,nrow = dim(x1)[1])
y2 <- matrix(1,nrow = dim(x2)[1])

# agrupando os dados para chamar a funcao de treino do perceptron

xin <- rbind(x1,x2)
yin <- rbind(y1,y2)

# Plotando os dados 

plot(x1[,1],x1[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='blue')
par(new=TRUE)
plot(x2[,1],x2[,2],xlim=c(0,6),ylim = c(0,6),xlab ='x1',ylab = 'x2',col='red')

x1_reta <- seq(6/100,6,6/100)
x2_reta <- -x1_reta+6
par(new=TRUE)
plot(x1_reta,x2_reta,type = 'l',col='orange',xlim = c(0,6),xlab = '',ylab = '')

ret <- treinap(xin,yin,0.1,0.01,100)

w <- ret[[1]]
evec <- ret[[2]]

#print(w)

yperceptron <-function(xvec,w){
  u<-xvec %*% w
  y<-1.0*(u>=0)
  
  return(as.matrix(y))
}

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)

M <- matrix(0,nrow = length(seqi),ncol = length(seqj))

ci <- 0
for (i in seqi) {
  ci<-ci+1
  cj<-0
  for (j in seqj) {
    cj<-cj+1
    x<-as.matrix(cbind(i,j,-1))
    M[ci,cj]<- yperceptron(x,w)
    
  }
  
}

plot(x1[,1],x1[,2],col = 'red', xlim = c(0,6), ylim = c(0,6),xlab = 'x_1',ylab = 'x_2')
par(new=T)
plot(x2[,1],x2[,2],col = 'blue', xlim = c(0,6), ylim = c(0,6),xlab = '',ylab = '')
par(new=T)
contour(seqi,seqj,M,xlim = c(0,6),ylim = c(0,6),xlab= '', ylab='')

persp3D(seqi,seqj,M,counter = T,theta=55,phi=30,r=40,d=0.1,expand=0.5,ltheta=90,lphi=180,shade=0.4,ticktype='detailed',nticks=5)

# EX2 -----
rm(list = ls())
library('caret')

# gerando os dados
amostras <- 200
xc1 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(2,2)),ncol = 100,nrow = 2))
xc2 <- matrix(0.4*rnorm(amostras),ncol=2) + t(matrix((c(4,4)),ncol = 100,nrow = 2))

xc1treino <- xc1[1:(0.7*(amostras/ncol(xc1))),]
xc2treino <- xc2[1:(0.7*(amostras/ncol(xc2))),]

xc1teste <- xc1[((0.7*(amostras/ncol(xc1)))+1):(amostras/ncol(xc1)),]
xc2teste <- xc2[((0.7*(amostras/ncol(xc2)))+1):(amostras/ncol(xc2)),]

y1treino <- matrix(0,nrow = dim(xc1treino)[1])
y2treino <- matrix(1,nrow = dim(xc2treino)[1])

y1teste <- matrix(0,nrow = dim(xc1teste)[1])
y2teste <- matrix(1,nrow = dim(xc2teste)[1])

xinTreino <- rbind(xc1treino,xc2treino)
yinTreino <- rbind(y1treino,y2treino)

treinap <- function(xin,yin,eta,tol_parada,maxepocas){
  
  # pegando as dimensoes dos dados de entrada
  N<-dim(xin)[1]
  n<-dim(xin)[2]
  
  # adicionando coluna de 1s ao vetor de entrada
  xin<-cbind(xin,-1)
  # inicializando o vetor de pesos aleatoriamente
  w<-as.matrix(runif(n+1)-0.5)
  erroepoca<-tol_parada+10
  epocaatual<-0
  vetorEpocas <- matrix(nrow = 1,ncol = maxepocas)
  
  while((erroepoca>tol_parada)&&(epocaatual<maxepocas)) {
    indexvec = sample(N)
    erroQuad<-0
    for (i in 1:N) {
      #add estocasticidade ao treinamento
      index<-indexvec[i]
      #acha yhat
      yhat<-1*((xin[index,]%*%w)>=0)
      #calcula o erro
      erroatual<-yin[index,]-yhat
      w = w + eta*erroatual*xin[index,]
      erroQuad <- erroQuad + erroatual*erroatual
    }
    epocaatual <- epocaatual + 1 # atualiza as epocas
    vetorEpocas[epocaatual]<-erroQuad/N # erro medio de cada época
    erroepoca <- vetorEpocas[epocaatual] 
  }
  return(list(w,vetorEpocas[1:epocaatual]))
}

r <- treinap(xinTreino,yinTreino,0.01,0.01,100)
w <- r[[1]]
evec <- r[[2]]

# testando os pesos 


yperceptron <-function(xvec,w){
  u<-xvec %*% w
  y<-1.0*(u>=0)
  
  return(as.matrix(y))
}

xinTeste <- rbind(xc1teste,xc2teste)
yTeste <- rbind(y1teste,y2teste)

xinTeste <- cbind(xinTeste,-1) 
yhat <- yperceptron(xinTeste,w)

# calculando a acuracia de teste
acuraciaTeste <- 1- (t(yTeste-yhat)%*%(yTeste-yhat))/60

# calculando a acuracia de treino
xinTreino <- cbind(xinTreino,-1)
yhatTreino <- yperceptron(xinTreino,w)

acuraciaTreino <- 1- (t(yinTreino-yhatTreino)%*%(yinTreino-yhatTreino))/140

# matrizes de confusao 

confmTreino <- confusionMatrix(factor(yinTreino),factor(yhatTreino))

confmTeste <- confusionMatrix(factor(yTeste),factor(yhat))

print(confmTeste)


# EX3 ---------------------------------------------------------------------
rm(list = ls())
 
# carregando os dados
library('caret')

iris <- data("iris")
data <- iris3
planta1 <- data[1:50,1:4,1]
planta2 <- data[1:50,1:4,2]
planta3 <- data[1:50,1:4,3]

xin <- rbind(planta1,planta2,planta3)

yclass1 <- matrix(0,nrow = 50)
yclass2 <- matrix(1,nrow = dim(xin)[1]- dim(yclass1)[1])

yin <- rbind(yclass1,yclass2)

# selecionando aleatoriamente 70% das amostras de treino

indexTreino <- sample(dim(xin)[1])

Xtrain <- xin[indexTreino[1:(dim(xin)[1]*0.7)],]
Ytrain <- as.matrix(yin[indexTreino[1:(dim(xin)[1]*0.7)],])

Xtest <- xin[indexTreino[((dim(xin)[1]*0.7)+1):dim(xin)[1]],]
Ytest <- as.matrix(yin[indexTreino[((dim(xin)[1]*0.7)+1):dim(xin)[1]],])

# treinando o modelo 
yperceptron <-function(xvec,w){
  xvec <- cbind(xvec,-1)
  u<-xvec %*% w
  y<-1.0*(u>=0)
  
  return(as.matrix(y))
}

Rtrain <- treinap(Xtrain,Ytrain,0.01,0.001,100)
Wtrain <- Rtrain[[1]]
EvecTrain <- Rtrain[[2]]

YhatTrain <- yperceptron(Xtrain,Wtrain)

accTrain <- 1- (t(Ytrain-YhatTrain)%*%(Ytrain-YhatTrain))/105

# testando o modelo 

YhatTest <- yperceptron(Xtest,Wtrain)

accTest <- 1- (t(Ytest-YhatTest)%*%(Ytest-YhatTest))/45

# matrizes de confusao 

confmTrain <- confusionMatrix(factor(Ytrain),factor(YhatTrain))

confmTest <- confusionMatrix(factor(Ytest),factor(YhatTest))

print(confmTest)
print(confmTrain)

# criando loop de treinamento 

rep <- 100

wv <- matrix(0,nrow = 5,ncol = 1) 

for (i in 1:100) {
  
  r <- treinap(Xtrain,Ytrain,0.1,0.01,100)
  wv <- wv + r[[1]]
}

wv <- wv/rep

YhatTest2 <- yperceptron(Xtest,wv)

accTest2 <- 1- (t(Ytest-YhatTest2)%*%(Ytest-YhatTest2))/45

confmTest2 <- confusionMatrix(factor(Ytest),factor(YhatTest2))

print(confmTest2)


# EX4 ---------------------------------------------------------------------
rm(list = ls())
# carregando os dados
library('caret')
library(mlbench)

data(BreastCancer)

bcdata <- as.matrix(BreastCancer)

# tratando os dados com NA 

bcdata[is.na(bcdata)] <- 0
bcdata <- bcdata[,2:11]

# rotulando as classes

classes <- bcdata[,10]
originalClasses <- bcdata[,10]

for (i in 1:length(classes)) {
  if (classes[i]=="benign") {
    classes[i]<-as.numeric(0)
  }
  else{
    classes[i] <-as.numeric(1)
  }
}

bcdata <- as.matrix(bcdata[,1:9])
class(bcdata) <- "numeric"
classes <- as.matrix(classes)
class(classes) <- "numeric"


# selecionando aleatoriamente 70% das amostras de treino

xin <- bcdata
yin <- classes

indexTreino <- sample(dim(xin)[1])

Xtrain <- xin[indexTreino[1:(dim(xin)[1]*0.7)],]
Ytrain <- as.matrix(yin[indexTreino[1:(dim(xin)[1]*0.7)],])

Xtest <- xin[indexTreino[((dim(xin)[1]*0.7)+1):dim(xin)[1]],]
Ytest <- as.matrix(yin[indexTreino[((dim(xin)[1]*0.7)+1):dim(xin)[1]],])

# treinando o modelo 
yperceptron <-function(xvec,w){
  xvec <- cbind(xvec,-1)
  u<-xvec %*% w
  y<-1.0*(u>=0)
  
  return(as.matrix(y))
}

treinap <- function(xin,yin,eta,tol_parada,maxepocas){
  
  # pegando as dimensoes dos dados de entrada
  N<-dim(xin)[1]
  n<-dim(xin)[2]
  
  # adicionando coluna de 1s ao vetor de entrada
  xin<-cbind(xin,-1)
  # inicializando o vetor de pesos aleatoriamente
  w<-as.matrix(runif(n+1)-0.5)
  erroepoca<-tol_parada+10
  epocaatual<-0
  vetorEpocas <- matrix(nrow = 1,ncol = maxepocas)
  
  while((erroepoca>tol_parada)&&(epocaatual<maxepocas)) {
    indexvec = sample(N)
    erroQuad<-0
    for (i in 1:N) {
      #add estocasticidade ao treinamento
      index<-indexvec[i]
      #acha yhat
      yhat<-1*((xin[index,]%*%w)>=0)
      #calcula o erro
      erroatual<-yin[index,]-yhat
      w = w + eta*erroatual*xin[index,]
      erroQuad <- erroQuad + erroatual*erroatual
    }
    epocaatual <- epocaatual + 1 # atualiza as epocas
    vetorEpocas[epocaatual]<-erroQuad/N # erro medio de cada época
    erroepoca <- vetorEpocas[epocaatual] 
  }
  return(list(w,vetorEpocas[1:epocaatual]))
}


#Rtrain <- treinap(Xtrain,Ytrain,0.1,0.001,100)
#Wtrain <- Rtrain[[1]]
#EvecTrain <- Rtrain[[2]]

#YhatTrain <- yperceptron(Xtrain,Wtrain)

#accTrain <- 1- (t(Ytrain-YhatTrain)%*%(Ytrain-YhatTrain))/489

# testando o modelo 

#YhatTest <- yperceptron(Xtest,Wtrain)

#accTest <- 1- (t(Ytest-YhatTest)%*%(Ytest-YhatTest))/dim(Xtest)[1]

# matrizes de confusao 

#confmTrain <- confusionMatrix(factor(Ytrain),factor(YhatTrain))

#confmTest <- confusionMatrix(factor(Ytest),factor(YhatTest))

#print(confmTest)
#print(confmTrain)

# criando loop de treinamento 

rep <- 20

wv <- matrix(0,nrow = 10,ncol = 1) 

for (i in 1:rep) {
  
  print(i)
  r <- treinap(Xtrain,Ytrain,1,0.001,150)
  wv <- wv + r[[1]]
  
}

wv <- wv/rep

YhatTest2 <- yperceptron(Xtest,wv)

accTest2 <- 1- (t(Ytest-YhatTest2)%*%(Ytest-YhatTest2))/209

confmTest2 <- confusionMatrix(factor(Ytest),factor(YhatTest2))

print(confmTest2)
