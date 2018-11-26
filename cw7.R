library(dplyr)
library(animation)
library(rootSolve)#multiroot
library(GA)
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

sigma_E=sd(diff(log(dane$E)))*sqrt(262)
f1=function(x,n, data=dane)
{
    E = data$E[n]
    L = data$L[n]
    r = data$r[n]
    return(c(E - x[1] * pnorm(d1(x[1], L, r, x[2])) + L * exp(-r) * pnorm(d2(x[1], L, r, x[2])),
              x[1] * pnorm(d1(x[1], L, r, x[2])) * x[2] - sigma_E * E))
}

solution = sapply(1:dim(dane)[1], function(x)
    multiroot(function(y)
        f1(y, n = x), start = c(60000, 0.1))$root) %>% t

