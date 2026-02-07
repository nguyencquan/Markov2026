p = t(matrix(
  c(
    1/2, 1/2, 0, 0, 0, 0,
    0, 1/2, 1/2, 0, 0, 0,
    1/3, 0, 1/3, 1/3, 0, 0,
    0,0,0,1/2,1/2,
    0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 1, 0
  ),
  nrow = 6,
))

p5 = p
for (i in 2:5){
  p5 = p5%*%p
}


run5 = function(){
  currentState = 1
  for (i in 1:5){
    u = runif(1)
    margin = 0
    
    newState = 1
    while (u >= margin + p[currentState,newState]){
      margin = margin  + p[currentState,newState]
      newState = newState + 1
    }
    currentState = newState
  }
  return(currentState==4)
}

equal4 = replicate(10000,run5())
p5[1,4]
sum(equal4)/10000

