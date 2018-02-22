library(mirt)


resp<-read.table("emp-rasch.txt",header=FALSE)

#generate models
mod1<-mirt(resp,1,itemtype="Rasch")
mod2<-mirt(resp,1,itemtype="2PL")
mod3<-mirt(resp,1,itemtype="3PL")

#estimate and sort abilities
fscores(mod1,full.scores.SE=TRUE)->th1
th1[order(th1[,1]),]->th1
fscores(mod2,full.scores.SE=TRUE)->th2
th2[order(th2[,1]),]->th2
fscores(mod3,full.scores.SE=TRUE)->th3
th3[order(th3[,1]),]->th3

#generate plots
par(mfrow=c(3,1))
plot(jitter(th1),xlab="theta",ylab="se", main='Rasch',xlim=c(-4,4), ylim=c(0.15,0.6))
plot(jitter(th2),xlab="theta",ylab="se",main='2PL',xlim=c(-4,4), ylim=c(0.15,0.6))
plot(jitter(th3),xlab="theta",ylab="se",main='3PL',xlim=c(-4,4), ylim=c(0.15,0.6))

#test information plots
par(mfrow=c(3,1))
plot(th1[,1],testinfo(mod1,th1[,1]),xlab='theta',ylab='information')
plot(th2[,1],testinfo(mod2,th2[,1]),xlab='theta',ylab='information')
plot(th3[,1],testinfo(mod3,th3[,1]),xlab='theta',ylab='information')
