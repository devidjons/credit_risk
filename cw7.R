library(dplyr)
library(animation)
library(rootSolve)#multiroot function
dane = read.csv(
    "http://www2.im.uj.edu.pl/DariuszZawisza/enron.csv",
    sep = ";",
    header = T,
    dec = ","
)
colnames(dane) = c("E", "L", "r", "sp")
d1 = function(A, L, r, sigma)
{
    return((log(A / L) + (r + (sigma ^ 2) / 2)) / sigma)
}

d2 = function(A, L, r, sigma)
{
    return(d1(A, L, r, sigma) - sigma)
}

sigma_E=sd(diff(log(dane$equities)))*sqrt(262)
f1=function(x,n, data=dane)
{
    E = data$E[n]
    L = data$L[n]
    r = data$r[n]
    return(c(E - x[1] * pnorm(d1(x[1], L, r, x[2])) + L * exp(-r) * pnorm(d2(x[1], L, r, x[2])),
              x[1] * pnorm(d1(x[1], L, r, x[2])) * x[2] - sigma_E * E))
    
    
}

multiroot(function(x) f1(x,1,dane), start=c(1, 0.1))
 newton.method(f1, init=c(rep(1, 262), rep(0.1,262)))
