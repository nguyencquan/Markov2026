# I don't know R that well, so I will make it on intervals of .01
#anyways least efficient algorithm known to man
library(ggplot2)
dt = .05

stateAtT = function(t,position,waitTime){
  i = max(which(waitTime <= t))
  return(position[i])
}

simulation = function(n){#1/3 start 1, 2/3 start in 2
  totalAt1 = rep(0,5/dt + 1)
  
  for(i in 1:n){
    startPosition = ifelse(runif(1,min = 0, max = 1) < 1/3,1,2)
    position = c(startPosition)
    waitTime = c(0)
    
    currentPosition = startPosition
    while (waitTime[length(waitTime)] <= 5){
      wait = rexp(1,rate = 1)
      waitTime = c(waitTime, waitTime[length(waitTime)] + wait)
      currentPosition = ifelse(currentPosition + 1 <= 4, currentPosition + 1, 1)
      position = c(position,currentPosition)
    }
    
    discrete = seq(0, 5, by = dt)
    discretePosition = sapply(discrete, function(t) stateAtT(t, position, waitTime))
    
    isAt1 = discretePosition
    isAt1[isAt1 != 1] = 0
    totalAt1 = totalAt1 + isAt1
  }
  return(totalAt1/n)
}

df = data.frame(
  t = seq(0, 5, by = dt),
  n100 = simulation(100),
  n1000 = simulation(1000),
  n10000 = simulation(10000),
  n100000 = simulation(100000)
)

ggplot(data = df) +
  geom_line(aes(x = t,y = n100, color = "n100"),linewidth = 1,alpha = .5)+
  geom_line(aes(x = t,y = n1000, color = "n1000"),linewidth = 1,alpha = .5)+
  geom_line(aes(x = t,y = n10000, color = "n10000"),linewidth = 1,alpha = .5)+
  geom_line(aes(x = t,y = n100000, color = "n100000"),linewidth = 1,alpha = .5)+
  geom_function(fun = function(x) (-exp(-2*x) + exp(-x)*(2*cos(x) - 4* sin(x)))/12 + .25,linewidth = 1,
                linetype = "dashed",
                aes(color = "theoretical"))+
  scale_color_manual(
    name = "Simulation Size",
    values = c(
      "n100" = "blue",
      "n1000" = "red",
      "n10000" = "green",
      "n100000" = "purple",
      "theoretical" = "black"
    ),
    labels = c(
      "n100" = "n = 100",
      "n1000" = "n = 1,000",
      "n10000" = "n = 10,000",
      "n100000" = "n = 100,000",
      "theoretical" = "Theoretical Line"
    )
  ) + theme_bw() +
  labs(
    title = "Simulation of Markov Chain",
    x = "Time (no units)",
    y = "Proportion in state 1"
  ) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 1/2))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

