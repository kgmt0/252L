p0 <- 1
p1 <- function(theta,a,b,d1){
  exp(N*a*(theta-b+d1))
}
p2 <- function(theta,a,b,d1,d2){
  exp(N*a*(theta-b+d1)+1.7*a*(theta-b+d2))
}
p3 <- function(theta,a,b,d1,d2,d3){
  exp(N*a*(theta-b+d1)+1.7*a*(theta-b+d2)+1.7*a*(theta-b+d3))
}
p4 <- function(theta,a,b,d1,d2,d3,d4){
  exp(N*a*(theta-b+d1)+1.7*a*(theta-b+d2)+1.7*a*(theta-b+d3)+1.7*a*(theta-b+d4))
}

P0.2 <-function(theta,a,b,d1,d2){
  p0/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2))
}
P1.2 <- function(theta,a,b,d1,d2){
  p1(theta,a,b,d1)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2))
}
P2.2 <- function(theta,a,b,d1,d2){
  p2(theta,a,b,d1,d2)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2))
}

P0.4 <-function(theta,a,b,d1,d2,d3,d4){
  p0/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2)+p3(theta,a,b,d1,d2,d3)+p4(theta,a,b,d1,d2,d3,d4))
}
P1.4 <- function(theta,a,b,d1,d2,d3,d4){
  p1(theta,a,b,d1)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2)+p3(theta,a,b,d1,d2,d3)+p4(theta,a,b,d1,d2,d3,d4))
}
P2.4 <- function(theta,a,b,d1,d2,d3,d4){
  p2(theta,a,b,d1,d2)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2)+p3(theta,a,b,d1,d2,d3)+p4(theta,a,b,d1,d2,d3,d4))
}
P3.4 <- function(theta,a,b,d1,d2,d3,d4){
  p3(theta,a,b,d1,d2,d3)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2)+p3(theta,a,b,d1,d2,d3)+p4(theta,a,b,d1,d2,d3,d4))
}
P4.4 <- function(theta,a,b,d1,d2,d3,d4){
  p4(theta,a,b,d1,d2,d3,d4)/(p0+p1(theta,a,b,d1)+p2(theta,a,b,d1,d2)+p3(theta,a,b,d1,d2,d3)+p4(theta,a,b,d1,d2,d3,d4))
}

N <- 1.7
a <- .63
b <- .37
d1 <- .72
d2 <- -.72

curve(P0.2(x,a,b,d1,d2),-6,6,main='VH295342',xlab='theta',ylab='p',col=1)
curve(P1.2(x,a,b,d1,d2),-6,6,add=TRUE,col=2)
curve(P2.2(x,a,b,d1,d2),-6,6,add=TRUE,col=3)
legend(-5.5,0.7,title='Score Points',lwd=c(1,1,1),legend=c('0','1','2'),col=c(1,2,3))
abline(v=(b-d1),lty=2)
abline(v=(b-d2),lty=2)

N <- 1.7
a <- 0.6
b <- 0.0
d1 <- .39
d2 <- 1.47
d3 <- -.13
d4 <- -1.73

curve(P0.4(x,a,b,d1,d2,d3,d4),-6,6,main='VH295562',xlab='theta',ylab='p',col=1)
curve(P1.4(x,a,b,d1,d2,d3,d4),-6,6,add=TRUE,col=2)
curve(P2.4(x,a,b,d1,d2,d3,d4),-6,6,add=TRUE,col=3)
curve(P3.4(x,a,b,d1,d2,d3,d4),-6,6,add=TRUE,col=4)
curve(P4.4(x,a,b,d1,d2,d3,d4),-6,6,add=TRUE,col=5)
legend(-5.5,0.7,title='Score Points',lwd=c(1,1,1,1,1),legend=c('0','1','2','3','4'),col=c(1,2,3,4,5))
abline(v=(b-d1),lty=2)
abline(v=(b-d2),lty=2)
abline(v=(b-d3),lty=2)
abline(v=(b-d4),lty=2)
