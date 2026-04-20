Q = matrix(c(-1,1,0,
             1,-2,1,
             0,1,-1),nrow = 3,byrow = TRUE)

round(eigen(t(Q))$values,10)
round(eigen(t(Q))$vector,10)


E = matrix(c(1/3 , -1 , -1,
             1/3 , 0 , 2,
             1/3 , 1, -1),nrow = 3, byrow = TRUE)
solve(E,c(1,0,0))
