a = .04
b = .16
K = .1


pincrease = function(n){
  return(K*exp(a*n))
}

pdecrease = function(n){
  return(K*exp(b*(n-1)))
}


pstay = function(n){
  return(1-pincrease(n)-pdecrease(n))
}
r1 = c(1-pincrease(1),pincrease(1),0,0,0)
r2 = c(pdecrease(2),pstay(2),pincrease(2),0,0)
r3 = c(0,pdecrease(3),pstay(3),pincrease(3),0)
r4 = c(0,0,pdecrease(4),pstay(4),pincrease(4))
r5 = c(0,0,0,pdecrease(5),1-pdecrease(5))

p = matrix(c(r1,r2,r3,r4,r5),nrow= 5,byrow = TRUE)
p

e = eigen(t(p))

# note since R organizes eigenvalues in descending order, and we know the maximum is equal to 1

eigenVector = e$vectors[,1]/sum(e$vectors[,1])

eigenVector

currentState = 1
stateCounter = c(0,0,0)

for (i in 1:10^6){
   u = runif(0,1)
   transition = p[currentState,]
   nextPosition = 1
   while (){
     nextPosition = nextPosition + 1
   }
}