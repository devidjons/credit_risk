library(dplyr)
m=0.04
r=0.05
sigma=0.16
t=1
N=1000
make_winer=function(t=1, n=100)
{
    dt=t/n
    return(cumsum(rnorm(n,0,sqrt(dt))))
}

def_sim=function(k=0.6, n=100, years=1)
{
    A0=1
    L=k*A0
    t=(1:100)/100*years
    A=A0*exp((m-sigma^2/2)*t+sigma*make_winer(1,n))
    L=exp(-r*(years-t))
    return(c(moment_stopu=any(A<L),merton=last(A<L)))
}
simulation=(sapply(1:N, function(x) def_sim()))%>%t
colMeans(simulation)

