#EX1 -----

## Parsing the data
rm(list=ls())
t0 <- read.delim('Ex1_t')
x0 <- read.delim('Ex1_x')
y0 <- read.delim('Ex1_y')

t0<-strsplit(as.matrix(t0),split = ' ')
x0<-strsplit(as.matrix(x0),split = ' ')
y0<-strsplit(as.matrix(y0),split = ' ')

t<-list()
x<-list()
y<-list()

for (i in 1:20) {
  t<-append(t,as.numeric(t0[[i]][2]))
  x<-append(x,as.numeric(x0[[i]][2]))
  y<-append(y,as.numeric(y0[[i]][2]))
}

t<-matrix(unlist(t))
x<-matrix(unlist(x))
y<-matrix(unlist(y))

plot(t,x,type='l',col='red',xlim=c(-1,10),ylim=c(-1,1),xlab = "t",ylab = "x")
par(new=T)
plot(t,y,type='l',col='blue',xlim=c(-1,10),ylim=c(-1,1),xlab = "t",ylab = "x")

trainAdaline<- function(X,Y,eta,tolParada,maxEpocas){
  # pega as dimensoes de entrada
  N<-dim(X)[1]
  n<-dim(X)[2]
  
  # add coluna de 1s
  X<-cbind(X,1)
  #inicializa estocasticamente w
  w<-as.matrix(runif(n+1)-0.5)
  erroEpoca<-tolParada+1
  epocaAtual<-0
  vetorEpocas <- matrix(nrow = 1,ncol = maxEpocas)
  while ((erroEpoca>tolParada) && (epocaAtual<maxEpocas)){
    indexVec <- sample(N) # pega aleatoriamente os dados de treinamento
    erroQuad <- 0
    for (i in 1:N) {
      yhat <- 1.0 * (X[indexVec[i],] %*% w) #1x2 * 2x1 = 1x1
      erro <- Y[indexVec[i],] - yhat
      w <- w +eta*erro*X[indexVec[i],]
      erroQuad <- erroQuad + erro*erro
    }
    
    epocaAtual<-epocaAtual+1
    vetorEpocas[epocaAtual]<-erroQuad/N # erro medio
    erroEpoca<-vetorEpocas[epocaAtual] # atualiza o erro de cada epoca
  }
  result<-list(w,vetorEpocas[1:epocaAtual])
  return(result)
}

 
r<- trainAdaline(x,y,0.01,0.001,60)
pesos<- as.matrix(r[1][[1]])
evec<-as.matrix(r[2][[1]])

plot(1:length(evec),evec,type = 'l')

# gerando dados novos

tnew <- seq(0.1,6,0.1)
xnew <- as.matrix(sin(tnew))
# y = w1*x + w0
xnew <- cbind(xnew,1)
ynew <- xnew %*% pesos # 60x2 * 2x1 = 60x1

plot(t,y,type='b',col='black',xlim=c(-1,6),ylim=c(-1,1),xlab = "tempo",ylab = "y")
par(new=T)
plot(tnew,ynew,type='b',col='green',xlim=c(-1,6),ylim=c(-1,1),xlab = "tempo",ylab = "y")
legend('bottomleft',c('Original','Previsto'),fill = c('black','green'))

### Ex2 -------
rm(list=ls())
t2 <- as.matrix(read.table('t'))
x2 <- as.matrix(read.table('x'))
y2 <- as.matrix(read.table('y'))

#x2teste <- x2[11:20,]
#y2teste <- as.matrix(y2[11:20,])
#x2 <- x2[1:10,]
#y2 <- as.matrix(y2[1:10,])

trainAdaline<- function(X,Y,eta,tolParada,maxEpocas){
  # pega as dimensoes de entrada
  N<-dim(X)[1]
  n<-dim(X)[2]
  
  # add coluna de 1s
  X<-cbind(X,1)
  #inicializa estocasticamente w
  w<-as.matrix(runif(n+1)-0.5)
  erroEpoca<-tolParada+1
  epocaAtual<-0
  vetorEpocas <- matrix(nrow = 1,ncol = maxEpocas)
  while ((erroEpoca>tolParada) && (epocaAtual<maxEpocas)){
    indexVec <- sample(N) # pega aleatoriamente os dados de treinamento
    erroQuad <- 0
    for (i in 1:N) {
      yhat <- 1.0 * (X[indexVec[i],] %*% w) #1x2 * 2x1 = 1x1
      erro <- Y[indexVec[i],] - yhat
      w <- w +eta*erro*X[indexVec[i],]
      erroQuad <- erroQuad + erro*erro
    }
    
    epocaAtual<-epocaAtual+1
    vetorEpocas[epocaAtual]<-erroQuad/N # erro medio
    erroEpoca<-vetorEpocas[epocaAtual] # atualiza o erro de cada epoca
  }
  result<-list(w,vetorEpocas[1:epocaAtual])
  return(result)
}

r2 <- trainAdaline(x2,y2,0.01,0.01,200)
pesos2 <-r2[[1]]
evec2 <- r2[[2]]

plot(1:length(evec2),evec2,type = 'b',xlab = 'Epocas',ylab = 'Erro')

# y = w1*x1 + w2*x2 + w3*x3 + w0*1(x0)

#t22 <- seq(0.1,6,0.1)

#x21 <- as.matrix(sin(t22))
#x22 <- as.matrix(2*cos(t22))
#x23 <- as.matrix(t22 - 2.5)


x2new <- cbind(x2,1)
ynew2 <- x2new %*% pesos2

plot(t2,y2,type='b',col='black',xlim=c(-1,7),ylim=c(-3,10),xlab = "tempo",ylab = "y")
par(new=T)
plot(t2,ynew2,type='b',col='green',xlim=c(-1,7),ylim=c(-3,10),xlab = "tempo",ylab = "y")
legend('bottomleft',c('Original','Previsto'),fill = c('black','green'))






  
  