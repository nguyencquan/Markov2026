#Simulate one 48-minute game with λ = 3/minute by simulating separate exponential interarrival
#times for team A and B, and plotting red and blue vertical bars representing the time baskets
#are scored for teams A and B for t ∈ [0, 48].

library(ggplot2)

lambda = 3
interA = c()
interB = c()

A = c()
B = c()

while (sum(interA)<=48){
  interA = c(interA,rexp(1,lambda))
  A = c(A, sum(interA))
}

while (sum(interB)<=48){
  interB = c(interB,rexp(1,lambda))
  B = c(B, sum(interB))
}

ggplot() +
  geom_histogram(aes(x = A),alpha = .5, fill = "blue",linewidth = .7,binwidth = 2)+
  geom_histogram(aes(x = B),alpha = .5, fill = "red",linewidth = .7,binwidth = 2) +
  scale_x_continuous(limits = c(0, 48))+
  labs(
    title = "Distribution of scoring, seperate sampling",
    x = "Time (minutes)",
    y= "Basket Count"
  ) + theme_bw()
