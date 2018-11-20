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
d1 = function(, L, r, sigma)
{
    return((log(A / L) + (r + (sigma ^ 2) / 2)) / sigma)
}

d2 = function(A, L, r, sigma)
{
    return(d1(A, L, r, sigma) - sigma)
}

sigma_E=sd(diff(log(dane$equities)))*sqrt(262)
f1=function(x, data=dane)
{
    E = data$E
    L = data$L
    r = data$r
    return(c((E - x[1:262] * pnorm(d1(x[1:262], L, r, x[263:524])) + L * exp(-r) * pnorm(d2(x[1:262], L, r, x[263:524]))) ^ 2,
              (x[1:262] * pnorm(d1(x[1:262], L, r, x[263:524])) * x[263:524] - sigma_E * E) ^ 2))
    
    
}

multiroot(f1, start=c(rep(1, 262), rep(0.1,262)), data=dane)
newton.method(f1, init=c(rep(1, 262), rep(0.1,262)))
