library(ggplot2)

N = 10^5

arrivalTimes = replicate(N,max(runif(3,min = 0, max = 1)))

df = data.frame(arrivalTimes)

ggplot(df,aes(x = arrivalTimes)) + geom_histogram(bins = 50,aes(y = after_stat(density)),fill = "blue", alpha = .5)+ geom_function(size = 1,color = "red",fun = function(x) 3*x^2) + 
  labs(x = "Arrival Times", title = "Theoretical and simulated density curve") + theme_bw()