library(mirt)
resp<-read.table("nde_math_white_space.txt",header=FALSE)

#CTT Model
get.coors<-function(resp) {
  r.xt<-numeric() #initializing a numeric vector
  ss<-rowSums(resp) #these are the sum scores/observed scores of CTT
  for (i in 1:ncol(resp)) {
    r.xt[i]<-cor(ss,resp[,i]) #for any i, what is this?
  }
  r.xt
}

r <- get.coors(resp) #item total correlations
p <- colMeans(resp) #p values

plot.fun<-function(resp) { #don't worry too much about the details here in the first pass.
  pv<-colMeans(resp,na.rm=TRUE)
  r.xt<-get.coors(resp)
  plot(pv,r.xt,pch=19)
  ##everything below here just goes into the red line
  loess(r.xt~pv)->m
  cbind(pv,fitted(m))->tmp
  lines(tmp[order(tmp[,1]),],col="red")
  NULL
}

plot.fun(resp) #plots item total correlations vs p-values

#Rasch Model
mod1<-mirt(resp,1,itemtype="Rasch")
theta1<-fscores(mod1,method="ML",full.scores=TRUE)
plot(theta1)



rasch_fit <- itemfit(mod1,fit_stats='infit')
summary(rasch_fit)


plot(rasch_fit[,2], main='Outfit')
abline(1-1.96*.05,0,col='red')
abline(1+1.96*.05,0,col='red')

plot(extract.mirt(mod1,'parvec')) #plots estimated item easiness

g1 <- extract.mirt(mod1, 'G2') #extracts "goodness of fit" statistic
g1

mod1a<-mirt(resp[,-22],1,itemtype="Rasch")
theta1a<-fscores(mod1a,method="ML",full.scores=TRUE)
plot(theta1a)

rasch_fit_a <- itemfit(mod1a,fit_stats='infit')
summary(rasch_fit_a)


plot(rasch_fit_a[,2], main='Outfit')
abline(1-1.96*.05,0,col='red')
abline(1+1.96*.05,0,col='red')

plot(extract.mirt(mod1a,'parvec')) #plots estimated item easiness


#2PL
mod2<-mirt(resp,1,itemtype="2PL")
theta2<-fscores(mod2,method="ML",full.scores=TRUE)
plot(theta2)




#plot estimated item parameters
matrix(extract.mirt(mod2,'parvec'),ncol=2,byrow=TRUE) -> pars
plot(pars[,1], main = 'Estimated Item Discrimination')  #this might be easiness
plot(pars[,2], main = 'Estimated Item Easiness')        #this might be discrimination
summary(pars)


#3PL
mod3<-mirt(resp,1,itemtype="3PL")
theta3<-fscores(mod3,method="ML",full.scores=TRUE)
plot(theta3)


#plot estimated item parameters
matrix(extract.mirt(mod3,'parvec'),ncol=3,byrow=TRUE) -> pars
plot(pars[,1], main = 'Estimated Item Discrimination')  #this might be easiness
plot(pars[,2], main = 'Estimated Item Easiness')        #this might be discrimination
plot(pars[,3], main = 'Estimated Guessing Parameters')  #these are 'g' values - check out the mirt documentation to see how they write the model.  more negative means more impact from guessing.
summary(pars)



# test information curves

# rasch model - outlying item included
plot(theta1,testinfo(mod1, theta1))

# rasch model - outlying item removed
plot(theta1a,testinfo(mod1a, theta1a))

# 2PL 
plot(theta2,testinfo(mod2, theta2))

# 3PL
plot(theta3,testinfo(mod3, theta3))


#information curves
  
graphs <- seq(1,40)
for (n in graphs){
  print(itemplot(mod1,n,type='info',ylim=c(0,1)))
}
for (n in graphs){
  print(itemplot(mod2,n,type='info',ylim=c(0,1)))
}
for (n in graphs){
  print(itemplot(mod3,n,type='info',ylim=c(0,1)))
}

