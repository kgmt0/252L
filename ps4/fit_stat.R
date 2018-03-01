##You have N draws from some distribution f()
##let's call the vector of draws x
##You want to test a null hypothesis something like: f() has "spread" (loosely defined here) similar to that of the normal distribution.

##let's consider two test statistics
##a. the SD of the data (so, h0: sd(x)=1)
##b. the IQR of the data (so, h0: iqr(x)= qnorm(.75)-qnorm(.25)=1.35)
##obviously these are both related to the variation in the data.

##now, different draws of x will results in different test statistics. we can utilize simulation to generate the null distribution of these test statistics

set.seed(13141)
ts1<-ts2<-numeric() #these will be test statistics for different draws of x
N<-250
for (i in 1:1000) {
    x<-rnorm(N) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
    ts1[i]<-sd(x)
    ts2[i]<-quantile(x,.75)-quantile(x,.25)
}

##Now let's generate test statistics from a scenario in which the null hypothesis is false (e.g., f() is the student's T distribution with df=5)
obs<-rt(N,df=5)
obs.ts1<-sd(obs) #1.4
obs.ts2<-quantile(obs,.75)-quantile(obs,.25) #1.6
##so we can see that ts1 falls well outside of the distribution geneated when null hypothesis is true (compare to lefthand panel of figure). ts2, however, is a less sensitive test here (compare to righthand panel of figure). to the extent that we were interested in asking whether the unknown data-generating function f() had variation comparable to the normal, we'd perhaps be better off looking at the SD rather than the IQR.

par(mfrow=c(1,2))
plot(density(ts1),xlim=range(c(ts1,obs.ts1)))
abline(v=obs.ts1,col="red")
plot(density(ts2),xlim=range(c(ts2,obs.ts2)))
abline(v=obs.ts2,col="red")

quantile(ts1,c(.025,.975)) #so, we expect variation in our first test statistic under the null between ~(0.91,1.09)
quantile(ts2,c(.025,.975)) #here we expect variation between (1.2,1.5)
get.pseudop<-function(val,ec) {
    fun<-ecdf(ec)
    fun(val)->xx
    xx
}
get.pseudop(obs.ts1,ts1)
get.pseudop(obs.ts2,ts2)

##it looks to me like ts1 is a way better test statistic for examining the hypothesis that observed variation is consistent with what we get from a normal distribution. to be authoritative we'd want to conduct this experiment many many times.

##1. does ts2 become a more discerning test statistic (in an absolute sense, not relative to ts1) as we increase N?

pp <- numeric()
svec <- numeric()
snvec <- numeric()
Ns <-seq(10, 2000, length.out=100)

for (s in 1:100) {
  ts<-tns<-numeric() #these will be test statistics for different draws of x
  for (i in 1:1000) {
    s.x<-rt(Ns[s],df=5) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
    ts[i]<-quantile(s.x,.75)-quantile(s.x,.25)
    sn.x<-rnorm(Ns[s]) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
    tns[i]<-quantile(sn.x,.75)-quantile(sn.x,.25)
  }
  svec[s] <- mean(ts)
  snvec[s] <- mean(tns)
  pp[s] <- get.pseudop(mean(ts),tns)
}
par(mfrow=c(1,1))
plot(Ns,pp, xlab='N', ylab='Pseudo p-Value')
abline(h=0.975,col='red')

#Yes, it starts to become useful around N=1000.

##2. does the performance of ts2 improve if you go from the iqr to a more/less extreme spread? (i have no clue what the answer is!)
pp <- numeric()
svec <- numeric()
snvec <- numeric()
vals<-seq(0.5, 1, length.out=100)
N<-250
for (s in 1:100) {
  ts<-tns<-numeric() #these will be test statistics for different draws of x
  for (i in 1:1000) {
    s.x<-rt(N,df=5) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
    ts[i]<-quantile(s.x,vals[s])-quantile(s.x,(1-vals[s]))
    sn.x<-rnorm(N) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
    tns[i]<-quantile(sn.x,vals[s])-quantile(sn.x,(1-vals[s]))
  }
  svec[s] <- mean(ts)
  snvec[s] <- mean(tns)
  pp[s] <- get.pseudop(mean(ts),tns)
}
par(mfrow=c(1,1))
plot(vals,pp, xlab='Upper Percentile', ylab='Pseudo p-Value')
abline(h=0.975,col='red')

#for a fixed N, the ability of some type of range statistic to discern between the z and t ditsributions increases as the width of that range increases.

##3. (harder) can you come up with a test statistic for the null hypothesis that f() IS the normal distribution?
# Yes - 1-2*sd/(range of the middle 68.2% of the data) - centered around zero for normal distributions.  

N<-250
uts<-tons<-ts<-tns<-numeric() #these will be test statistics for different draws of x
for (i in 1:1000) {
  s.x<-rt(N,df=5) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
  ts[i]<- 1-2*sd(s.x)/(quantile(s.x,.841)-quantile(s.x,.159))
  sn.x<-rnorm(N) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
  tns[i]<-1-2*sd(sn.x)/(quantile(sn.x,.841)-quantile(sn.x,.159))
  on.x<-rnorm(N, mean=3, sd=1.5) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
  tons[i]<-1-2*sd(on.x)/(quantile(on.x,.841)-quantile(on.x,.159))
  u.x<-runif(N) #note that i've in fact added something to the null hypothesis here: that the data in fact comes from the standard normal
  uts[i]<-1-2*sd(u.x)/(quantile(u.x,.841)-quantile(u.x,.159))
}
get.pseudop(mean(tns),tns)
get.pseudop(mean(uts),tns)
get.pseudop(mean(ts),tns)
get.pseudop(mean(tons),tns)

quantile(tns,c(.025,.975))
mean(ts)
mean(tns)
mean(tons)
mean(uts)
plot(density(tns),xlim=c(-.2,.2),main='Distribution of Test Statistics')
abline(v=mean(ts),col='red')
abline(v=mean(tons),col='green')
abline(v=mean(uts),col='blue')

