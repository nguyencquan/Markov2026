library(ggplot2)

pmf = function(x){
  return((x^4)/(x^6+1))
}

sample = function(i){
  N = floor(10^i)
  x = runif(N,0,1)
  y = runif(N,0,1)
  underIntegral = y <= pmf(x)
  return(sum(underIntegral)/N)
}

xVal = seq(from = 1, to = 5, by = .1)
estimates = sapply(xVal, sample)

df = data.frame(Log_n = xVal, Estimate = estimates)

ggplot(data = df,aes(x=Log_n,y = Estimate)) + geom_point(color = "blue") + geom_line(color = "red") +
  ggtitle("Monte Carlo Simulation of integral") + geom_hline(yintercept = .1434, color = "yellow", linetype = "dashed", size = 1) + theme_bw()