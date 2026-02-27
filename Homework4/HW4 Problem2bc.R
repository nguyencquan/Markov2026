p = matrix(nrow = 6, ncol = 6)

for(i in 1:5){
  p[i+1,] = dbinom(0:5,i,9/10)
}

p[1,] = c(0,0,0,0,0,1)

pPrime=p[2:6,2:6]
I = diag(5)


solve(I-pPrime,rep(1,5))

d = solve(I-t(pPrime),c(0,0,0,0,1))

c(1,d) * 1/(1+sum(d))


