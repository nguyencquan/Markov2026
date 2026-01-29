set.seed(0328)

newtonMethod = function(x_0,u){
  x_1 = x_0 - (1-exp(-x_0)*(x_0+1)-u)/(x_0*exp(-x_0))
  return(x_1)
}

getRoot = function(u){
  x = 1
  for(i in 1:5){x = newtonMethod(x,u)}
  k = 0
  while (x < 0 & k < 20){x = newtonMethod(x,u); k = k+1}
  if (x<0){
    print('ERROR')
    print(u)
  }
  return(x)
}


start = Sys.time()
U = runif(10^6)

X = sapply(U,FUN = getRoot)

finish = Sys.time()
finish - start

hist(X,breaks = 100)
