p = 0

for (b in 0:100){
  pALessEqB = 0
  for (a in 0:(b)){
    pALessEqB = pALessEqB + (2^a * exp(-2)/factorial(a))
  }
  pAGreaterB = 1 - pALessEqB
  p = p + pAGreaterB * (1.5^b * exp(-1.5)/factorial(b))
}

p

#However if we abuse r code for better efficiency
#This will be the equation I will use from now on
pOp = 0
for (b in 0:100){
  pAGreaterB = 1 - ppois(b,2)
  pOp = pOp + pAGreaterB *  dpois(b, lambda = 1.5)
}
pOp