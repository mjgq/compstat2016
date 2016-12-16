
nsim=10000
alfa =0.05

f1 <- function(x){
  2*sqrt(2-x**2)
}
f2 <- function(x){
  4/(1+x**2)
  
}
f3<- function(x){
  6/(sqrt((4-x**2)))
}

montecarlo<- function(nsim,alfa=0.05){
  x<-runif(nsim,0,1)
  phi<-sapply(x,f3)
  theta<-mean(phi)
  z<-qnorm(alfa/2,lower.tail=FALSE)
  s<-var(phi)
  limsup <-theta + z*sqrt((s**2)/nsim)
  liminf <-theta - z*sqrt((s**2)/nsim)
  return(list(est=theta,limup=limsup,liminf=liminf))
}
n<-seq(100,1000,by=10)
res=t(sapply(n,montecarlo))
plot(as.numeric(res[,1]),type="l")
lines(as.numeric(res[,2]),col="blue")
lines(as.numeric(res[,3]),col="red")
abline(h=pi)
