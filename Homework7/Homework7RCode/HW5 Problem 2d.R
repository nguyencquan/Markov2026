#Simulate another game by simulating a single process representing baskets scored by either
#team, and choosing each basket to be for A or B with probability 1/2. Repeat the plot done in
#part (b).

library(ggplot2)

lambda = 3
allScores = c()
allScoreSum = c()

A2 = c()
B2 = c()

while (sum(allScores)<=48){
  allScores = c(allScores,rexp(1,lambda*2))
  allScoreSum = c(allScoreSum, sum(allScores))
}

select = runif(length(allScoreSum))

A2 = allScoreSum[select >.5]
B2 = allScoreSum[select <=.5]


ggplot() +
  geom_histogram(aes(x = A2),alpha = .5, fill = "blue",linewidth = .7,binwidth = 2)+
  geom_histogram(aes(x = B2),alpha = .5, fill = "red",linewidth = .7,binwidth = 2) +
  scale_x_continuous(limits = c(0, 48))+
  labs(
    title = "Distribution of scoring, grouped sampling",
    x = "Time (minutes)",
    y= "Basket Count"
  ) + theme_bw()
