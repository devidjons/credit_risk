library(dplyr)
library(rootSolve)#multiroot function
dane = read.csv(
    "http://www2.im.uj.edu.pl/DariuszZawisza/enron.csv",
    sep = ";",
    header = T,
    dec = ","
)

d1 = function(A, L, r, sigma)
{
    return((log(A / L) + (r + (sigma ^ 2) / 2)) / sigma)
}

d2 = function(A, L, r, sigma)
{
    return(d1(A, L, r, sigma) - sigma)
}

sigma_E=sd(diff(log(dane$equities)))*sqrt(262)
f=function(a)
{
    x=a[,1]
    y=a[,2]
    sum(x)+sum(y^2)
}
g=function(a)
{
    x=a[,1]
    y=a[,2]
    sum(x-y)
}
model=function(a)
{
    return(c(f(a), g(a)))
}
multiroot(model, start=matrix(rnorm(4),2,2))
