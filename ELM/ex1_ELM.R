# EX1 ----- ---------------------------------------------------------------

# gerando os dados de entrada

library('mlbench')
library('corpcor')

cd0 <- mlbench.2dnormals(200)

cd1 <- mlbench.xor(100)

cd2 <- mlbench.circle(100)

cd3 <- mlbench.spirals(100,sd = 0.05)

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


