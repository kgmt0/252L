EM <- function(PA,PB,sets,N,pA,pB){
  
  #construct flip matrix
  flips <- matrix(NA,sets,N)
  for (i in 1:sets){
    prob <- ifelse(runif(1)<0.5,PA,PB)
    for (j in 1:N){
      flips[i,j] <- ifelse(runif(1)<prob,1,0)
    }
  }
  
  dpA <- dpB <- 1
  thresh <- 0.0001
  
  while (abs((dpA+dpB)/2)>= thresh){
      
    coins <- c(0,0,0,0)
    for (i in 1:sets){
      wA <- pA^rowSums(flips)[i]*(1-pA)^(N-rowSums(flips)[i])
      wB <- pB^rowSums(flips)[i]*(1-pB)^(N-rowSums(flips)[i])
      wA <- wA/(wA+wB)
      wB <- wB/(wA+wB)
      coins[1] <- coins[1] + wA*rowSums(flips)[i]
      coins[2] <- coins[2] + wA*(N-rowSums(flips)[i])
      coins[3] <- coins[3] + wB*rowSums(flips)[i]
      coins[4] <- coins[4] + wB*(N-rowSums(flips)[i])
    }
    
    pAold <- pA
    pBold <- pB
  
    pA <- coins[1]/(coins[1]+coins[2])
    pB <- coins[3]/(coins[3]+coins[4])
    
    dpA <- pA - pAold
    dpB <- pB - pBold
  }
  return(c(pA,pB))
}

##using the EM function you can start to ask questions.
##a. how sensitive is estimation (e.g., in terms of bias or mean squared error) to initial values (e.g., c(.52,.5) in above)?

p <-seq(.1, .8, length.out=100)
aresults <- matrix(NA,100,3)
for (n in 1:100){
  a <- p[n]
  for (i in 1:100){
    t <- EM(.3,.7,5,10,a,.8)
    temp[i,1] <- (t[1]-.3)^2
    temp[i,2] <- (t[2]-.7)^2
  }
  aresults[n,1] <- 0.8-p[n]
  aresults[n,2] <- mean(temp[,1])
  aresults[n,3] <- mean(temp[,2])
}

plot(aresults[,1],aresults[,2],main='First Probability Estimation', xlab='Separation Between Starting Estimates', ylab='Mean Square Error')
plot(aresults[,1],aresults[,3],main='Second Probability Estimation', xlab='Separation Between Starting Estimates', ylab='Mean Square Error')

##b. how sensitive is estimation to the location of true parameters (e.g., the "p" argument below)?

P <-seq(0, .8, length.out=100)
bresults <- matrix(NA,100,3)
for (n in 1:100){
  a <- P[n]
  for (i in 1:100){
    t <- EM(a,.8,5,10,.45,.55)
    temp[i,1] <- (t[1]-a)^2
    temp[i,2] <- (t[2]-.8)^2
  }
  bresults[n,1] <- 0.8-a
  bresults[n,2] <- mean(temp[,1])
  bresults[n,3] <- mean(temp[,2])
}

plot(bresults[,1],bresults[,2],main='First Probability Estimation', xlab='Separation Between True Parameters', ylab='Mean Square Error')
plot(bresults[,1],bresults[,3],main='Second Probability Estimation', xlab='Separation Between True Parameters', ylab='Mean Square Error')

##c. to the values of n and N?
#c. effect of N on error:  mean squared error decreases as number of flips increases - past 50 flips or so, error stabilizes
cresults <- matrix(NA,100,3)
for (N in 1:100){
  temp <- matrix(NA,100,2)
  for (i in 1:100){
    t <- EM(.3,.7,5,N,.45,.55)
    temp[i,1] <- (t[1]-.3)^2
    temp[i,2] <- (t[2]-.7)^2
  }
  cresults[N,1] <- N
  cresults[N,2] <- mean(temp[,1])
  cresults[N,3] <- mean(temp[,2])
}

plot(cresults[,1],cresults[,2],main='First Probability Estimation', xlab='Number of Coin Flips', ylab='Mean Square Error')
plot(cresults[,1],cresults[,3],main='Second Probability Estimation', xlab='Number of Coin Flips', ylab='Mean Square Error')

##c. to the values of n and N?
#d. effect of number of sets on error:  mean squared error decreases as number of flips increases - past 50 flips or so, error stabilizes
dresults <- matrix(NA,100,3)
for (N in 1:100){
  temp <- matrix(NA,100,2)
  for (i in 1:100){
    t <- EM(.3,.7,N,10,.45,.55)
    temp[i,1] <- (t[1]-.3)^2
    temp[i,2] <- (t[2]-.7)^2
  }
  dresults[N,1] <- N
  dresults[N,2] <- mean(temp[,1])
  dresults[N,3] <- mean(temp[,2])
}

plot(dresults[,1],dresults[,2],main='First Probability Estimation', xlab='Number of Sets of Flips', ylab='Mean Square Error')
plot(dresults[,1],dresults[,3],main='Second Probability Estimation', xlab='Number of Sets of Flips', ylab='Mean Square Error')
