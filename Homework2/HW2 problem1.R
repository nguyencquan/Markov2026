library(ggplot2)
x_0 = 10
gamma = 4

inverseCDF = function(p){
  return((x_0)/(p^(1/(gamma-1))))
}

pdf = function(x){
  val = x_0^(gamma-1)*(gamma-1)*x^(-gamma)
  val[x<x_0] = 0
  return(val)
}

sample100 = data.frame(x = inverseCDF(runif(100,0,1)))
sample1000 = data.frame(x = inverseCDF(runif(1000,0,1)))
sample10000 = data.frame(x = inverseCDF(runif(10000,0,1)))



ggplot(data.frame(sample100),aes(x = x)) + geom_histogram(aes(y = after_stat(density)),bins = 30) + geom_function(fun = pdf,linewidth = 1 ,color = "red") + xlim(0,60) + labs(title = "Distribution with 100 samples")
ggplot(data.frame(sample1000),aes(x = x)) + geom_histogram(aes(y = after_stat(density)),bins = 40) + geom_function(fun = pdf,linewidth = 1 ,color = "red") + xlim(0,60) + labs(title = "Distribution with 1000 samples")
ggplot(data.frame(sample10000),aes(x = x)) + geom_histogram(aes(y = after_stat(density)),bins = 70) + geom_function(fun = pdf,linewidth = 1 ,color = "red") + xlim(0,60) + labs(title = "Distribution with 10000 samples")
