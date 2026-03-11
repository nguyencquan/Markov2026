library(ggplot2)

a = .99

A = matrix(c(
  1-a,a,0,
  a,0,1-a,
  0,1-a,a
),nrow = 3,byrow = TRUE)

eigenStuff = eigen(A)

vector1 = eigenStuff$vectors[,1]/eigenStuff$vectors[3,1]
vector2 = eigenStuff$vectors[,2]/eigenStuff$vectors[3,2]
vector3 = eigenStuff$vectors[,3]/eigenStuff$vectors[3,3]

X = matrix(c(vector1,vector2,vector3),nrow = 3)

q0 = c(1,0,0)

decomp = solve(X)%*% q0
decomp


#N is how many times we are going to run it
runMarkov = function(N,graph){
  fractionIn1 = c()
  currentStates = rep(1,N)
  for (n in 1:300){
    for (i in 1:N){
      u = runif(1)
      newState = 1
      cdf = A[currentStates[i],1]
      while (u>= cdf){
        newState = newState + 1
        cdf = cdf + A[currentStates[i],newState]
      }
      currentStates[i] = newState
    }
    fractionIn1 = c(fractionIn1,sum(currentStates == 1)/N )
  }
  if(graph){
    ggplot(data.frame(
      "count" = fractionIn1,
      "x" = 1:300
    ),aes(x=x,y = count)) + geom_point(aes()) +
      geom_ribbon(aes(ymin = 1/3-(1/3 * eigenStuff$values[2]^x),ymax = 1/3 + (2/3 *eigenStuff$values[2]^x)),
                  fill = "lightpink",alpha = .5) + labs(title = paste("Markov Chain simulation N = ",N), y = "proportion of chain in 1", x = "n time")

  }
}

runMarkov(10000,TRUE)