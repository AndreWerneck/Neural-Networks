rm(list=ls())
library('corpcor')
fgx<-function(xin) 0.5*xin^2+3*xin+10

X<-runif(100,-15,10)
Y<-fgx(X) + 10*rnorm(length(X),mean = 0,sd=4)
H <-cbind(X,1)
w<-pseudoinverse(H) %*% Y
print(cbind(w,c(0.5,3,10)))

xgrid<-seq(-15,10,0.1)
ygrid<-(0.5*xgrid^2+3*xgrid+10)
Hgrid<-cbind(xgrid,1)
yhatgrid<-Hgrid %*% w

plot(X,Y,type='p',col='red',xlim=c(-15,10),ylim=c(-10,120),xlab = "x",ylab = "y")
par(new=T)
plot(xgrid,ygrid,type='l',col='blue',xlim=c(-15,10),ylim=c(-10,120),xlab = "x",ylab = "y")
par(new=T)
plot(xgrid,yhatgrid,type='l',col='green',xlim=c(-15,10),ylim=c(-10,120),xlab = "x",ylab = "y")
legend("topleft",legend = c("data","Fgx","Polinomio Aproximado"),col=c("red","blue","green"),lty=1,cex=0.8)
