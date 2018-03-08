resp <-read.table("emp-3pl-math-reading.txt",header=TRUE)
resp
readsums <- rowSums(resp[,47:102])
readsums
resp[order(rowSums(resp[,47:102])),]->resp2
resp2
mathsums <- rowSums(resp2[,2:46])
mathsums
member <- c(rep(0,731),rep(1,731))
member

item <- matrix(NA,45,2)
for (i in 2:46){
  m <- glm(resp2[,i] ~ mathsums + member, family='binomial')
  obj<-summary(m)
  item[i-1,1] <- obj$coefficients[3,1]
  item[i-1,2] <- obj$coefficients[3,4]
}
item

plot(2:46,item[,1],col=ifelse(item[,2]<=0.05,'red','blue'),main='DIF: Math Items', xlab='Item',ylab=expression(beta[1]))


