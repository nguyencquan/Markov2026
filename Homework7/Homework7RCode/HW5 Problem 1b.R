library(ggplot2)

winAtt = function(t){
  p = 0
  for (x in 0:50){
    p = p + dpois(x,lambda = 2/90*(90-t)) *  dpois(x, lambda = 1.5/90*(90-t))
  }
  return(p)
}

ggplot(data.frame(x = 0:90), aes(x)) + 
  geom_function(fun = winAtt, color = "blue",
                size = 1.5) +
  theme_bw() +
  labs(
    title = "Probability of tie with respect to time, no goals at time t",
    x = "Time (minutes)",
    y = "Probability of Tie"
  )
