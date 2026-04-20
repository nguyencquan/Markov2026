# calculating the Eigenvalues
Q = matrix(c(-1,1,0,0,0,-1,1,0,0,0,-1,1,1,0,0,-1),nrow = 4, byrow = TRUE)
eigenVector = eigen(Q)$vectors
eigenValues = eigen(Q)$values

round(eigenValues,15)

#Solving the linear system

Qt = t(Q)
Qt[1,] = c(1,1,1,1)

stationaryDistribution = solve(Qt,c(1,0,0,0))
stationaryDistribution

# Solving the Master Equation for constants
round(eigen(t(Q))$vectors,10)

A = matrix(c(.5 , .5 , 0,.25,
             -.5 , 0 , .5,.25,
             .5 , -.5 , 0,.25,
             -.5, 0, -.5,.25),nrow = 4, byrow = TRUE)
solve(A, c(1/3,2/3,0,0))
