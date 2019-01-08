library(dplyr)
dane=read.csv("dane11.csv")
dane[4,2]=10
N=100000
Fun=function(n)
{
    return(ifelse(n<=20, dane[n+1,4],100)/100)
}
z=matrix(rnorm(5*N), nrow = N)
lambda_fun=function(x)
{
    m=mean(x)
    sigma=sd(x)
    K1=log(prod(dnorm(x,m,sigma)))
    K2=log(prod(dnorm(x)))
    return(2*(K1-K2))
}
lambda_sample = apply(z, 1, lambda_fun)

emp_sample=dane[1:5,2]%>%Fun%>%qnorm
stat_val=lambda_fun(emp_sample)
mean(lambda_sample<stat_val)
