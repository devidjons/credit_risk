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
f1=function(a1,a2,n, data=dane)
{
    x=c(a1,a2)
    E = data$E[n]
    L = data$L[n]
    r = data$r[n]
    return(c(E - x[1] * pnorm(d1(x[1], L, r, x[2])) + L * exp(-r) * pnorm(d2(x[1], L, r, x[2])),
              x[1] * pnorm(d1(x[1], L, r, x[2])) * x[2] - sigma_E * E))
}




ga_fun = function(n)
{
    r1 = ga(
        type = "real-valued",
        fitness = function(x)
            - sum(f1(x[1], x[2], n) ^ 2),
        lower = c(dane$E[n], 0.1),
        upper = c(dane$E[n] + dane$L[n], 0.6),
        popSize = 100,
        maxiter = 500,
        maxFitness = -2e-2,
        suggestions = matrix(rep(c((dane$E[n] + dane$L[n]) * 0.95, 0.3), 4), ncol = 2, byrow = T),
        keepBest = F
    )
    sol=r1@solution
    return(c(sol, sum(f1(sol[1], sol[2], n) ^ 2)))
}
r2=ga_fun(5)
solution=sapply(1:262, function(x) ga_fun(x))%>%t