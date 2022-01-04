# generate flip data
PA = 0.75 #probability of flipping heads on coin A
PB = 0.45 #probability of flipping heads on coin B
sets = 50 #number of sets of flips
N = 100 #number of flips per set

#construct flip matrix
flips <- matrix(NA,sets,N)
for (i in 1:sets){
  prob <- ifelse(runif(1)<0.5,PA,PB)
  for (j in 1:N){
    flips[i,j] <- ifelse(runif(1)<prob,1,0)
  }
}


#set starting probability guesses
pA <- .5
pB <- .525

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
pA
pB
