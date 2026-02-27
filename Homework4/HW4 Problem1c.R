runGame = function(){
  i = 10
  while (i>0){
    k = runif(1,0,1)
    if (k <=.4){
      i = i-1
    }else if(k <= .75){
      i = i+1
    }else{
      break
    }
  }
  return(i)
}

d = replicate(100000,runGame())

mean(d)