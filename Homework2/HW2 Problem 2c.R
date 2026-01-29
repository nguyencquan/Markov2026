set.seed(0328)
c = 4/exp(1)
checkValue = function(){
  x = 0
  while(TRUE){
    x = rexp(1,.5)
    u = runif(1)
    if (u <= x*exp(-x)/(c*.5*exp(-.5*x))){
      break
    }
  }
  return(x)
}


start = Sys.time()

X = replicate(10^6,checkValue())

end = Sys.time()

end-start

hist(X,breaks = 100)


