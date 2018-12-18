library(dplyr)
#parameters
u=0.4
Time=7
dt=1/2
n=T/dt
nom=100
kupon=function(t)
{
    dt*nom*0.07125+ifelse(t<Time, 0,100)
}

B=100.18
D=78.38
r = data.frame(
    t = c(0.0833, 0.25, 1, 2, 3, 5, 10),
    R = 0.01 * c(5.56, 5.64, 5.68, 5.64, 5.62, 5.66, 5.76)
)
#method 1
B_fun=function(r, n)
 {
#     t1=r$t[r$t<=t]%>%max
#     r1=r$R[r$t==t1]
    t=n*dt
    t2=r$t[r$t>=t]%>%min
    r2=r$R[r$t==t2]
    return((1+r2*dt)^(n))
#     
}

p = (B - D) / ((1 - u) * sum((1:n) * kupon((1:n)*dt) * sapply(1:n, function(x)
    B_fun(r, dt * x))))

