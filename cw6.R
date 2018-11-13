library(dplyr)
dane = read.csv(
    "http://www2.im.uj.edu.pl/DariuszZawisza/enron.csv",
    sep = ";",
    header = T,
    dec = ","
)
head(dane)
epsilon = 0.01
colnames(dane) = c("E", "L", "r", "sp")
d1 = function(A, L, r, sigma)
{
    return((log(A / L) + (r + (sigma ^ 2) / 2)) / sigma)
}

d2 = function(A, L, r, sigma)
{
    return(d1(A, L, r, sigma) - sigma)
}

iteration = function(A, sigma, data = dane)
{
    E = data$E
    L = data$L
    r = data$r
    A1 = (E + L * exp(-r) * pnorm(d2(A, L, r, sigma)))/pnorm(d1(A,L,r,sigma))
    n = length(A1)
    sigma = sd(log(A1[-1] / A1[-n])) * sqrt(262)
    error = sum(abs(A1 - A))
    return(list(
        sigma = sigma,
        A = A1,
        comparison = error > epsilon
    ))
}
A = dane$E
sigma = sd(log(lead(A) / A), na.rm = T) * sqrt(262)
iteration(A, sigma)
iterstop = T
while (iterstop)
{
    result = iteration(A, sigma)
    A = result$A
    sigma = result$sigma
    iterstop = result$comparison
    print(sigma)
}
data_output = data.frame(A = A, sp = dane$sp, r = dane$r, L=dane$L)
data_output %>% mutate(m_reg = log(lead(.$A) / .$A) - r,
                       sp_reg = log(lead(.$sp) / .$sp) - r) -> data_output
beta = lm(m_reg ~ sp_reg - 1, data = data_output)$coef
data_output$m=data_output$r+beta*data_output$sp_reg
data_output%>%mutate(DD=(log(A/L)+(m-1/2*sigma^2))/sigma)%>%select(DD)%>%as.matrix->DD
plot(DD)
