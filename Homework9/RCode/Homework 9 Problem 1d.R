alpha = 1
beta = 1
L = 20

endTimes = c()

for (i in 1:1000){
  time = 0
  i = 0
  while (i != 20){
    waitTime = 0
    if (i == 0){
      waitTime = rexp(1,rate = alpha)
      direction = 1
    }else{
      waitTime = rexp(1,rate = alpha+beta)
      direction = ifelse(runif(1,0,1) <= .5,-1,1)
    }
    i = i + direction
    time = time + waitTime
  }
  endTimes = c(endTimes,time)
}

theoretical = (L* (L+1))/(2* alpha)
mean(endTimes)
var(endTimes)
