library(ggplot2)

winAtt = function(t,aGoals){
  p = 0
  for (x in 0:50){
    p = p + dpois(x,lambda = 2/90*(90-t)) *  dpois(x+aGoals, lambda = 1.5/90*(90-t))
  }
  return(p)
}

ggplot(data.frame(x = 0:90), aes(x)) + 
  stat_function(fun = function(t) winAtt(t, aGoals = ifelse(t < 60,0,1)),
                color = "red",size = 1.5) +
  theme_bw() +
  labs(
    title = "Probability of tie with respect to time, goal at 60",
    x = "Time (minutes)",
    y = "Probability of Tie"
  )
