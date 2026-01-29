start = Sys.time()

X = rexp(10^6,1) + rexp(10^6,1)


finish = Sys.time()

finish-start

hist(X,breaks = 100)

library(ggplot2)
ggplot(data.frame(x = X),aes(x = x)) + 
  geom_histogram(bins = 100,aes(y = ..density..),fill = "blue",alpha = .5) +
  geom_function(fun = function(x){x*exp(-x)},color = "red",size = 1) +
  labs(title = "Probability distribution using sum of two exponential distributions")