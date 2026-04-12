maxLambda = .5*(1+(120/30)^2)

sampleTimesSum = c()
sampleTimes = c()

Tfin = c()

while (sum(sampleTimes) <= 120){
  sampleTimes = c(sampleTimes,rexp(1,maxLambda))
  sampleTimesSum = c(sampleTimesSum, sum(sampleTimes))
}

for (t in sampleTimesSum){
  if (runif(1,0,1)<= (.5*(1+(t/30)^2))/maxLambda){
    Tfin = c(Tfin,t)
  }
}

Tfin = Tfin[-length(Tfin)]
length(Tfin)

ggplot() +
  geom_histogram(aes(x = Tfin),alpha = .5, fill = "blue",linewidth = .7,breaks = seq(0, 120, by = 10))+
  labs(
    title = "Distribution of incidence of illness (n = 389)",
    x = "Time (days)",
    y= "Incidence Count"
  ) + theme_bw()

length(Tfin[110 < Tfin & Tfin<=120])
length(Tfin[100 < Tfin & Tfin<=110])
