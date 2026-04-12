#Finally, simulate 105 games efficiently by using
#N = NA + NB ∼ Poisson(2λt), NA|N ∼ Binomial(N, 1/2)

#to estimate E[D(t)], VAR[D(t)], and P (D(t) = 0) for t = 48. Compare with the theoretical
#values.

n = 10^5
N = rpois(n,2*3*48)

Na = rbinom(n,N,.5)
Nb = N - Na


mean(2*(Na-Nb))
var(2*(Na-Nb))

3*8*48

sum(Na==Nb)/n
1/sqrt(2*pi * 6* 48)
