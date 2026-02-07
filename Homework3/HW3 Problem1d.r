library(ggplot2)

a = sqrt(3) - 1

3*(2-4*a-3*a^2)*exp(a)

ggplot() + xlim(c(0,10)) + geom_function(fun = function(x) return(x/3*(1+x)*exp(-x)),aes(color = "f(x)")) + 
  geom_function(fun = function(x) return(1/(3*a^2*exp(a)*(1-a)) * a^2*x*exp(-a*x)),aes(color = "c(a*)g(x)")) +
  theme_bw() +
  labs(title = "pdf of f(x) and c(a*)g(x)") +
  scale_color_manual(
    name = "Density functions",
    values = c("f(x)" = "blue", "c(a*)g(x)" = "red")
  )
