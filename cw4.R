betta=0.65
N=1000
x=matrix(rnorm(2*N), ncol=2)
z=rnorm(N)
w=rchisq(n = N, df = 3)
# s1=cbind(sqrt(1-betta^2))
s1=sqrt(1-betta^2)*x+betta*z

s2=s1/sqrt(w)*sqrt(3)
s3=qnorm(pt(s2,3))
s4=x
par(mfrow=c(2,2))

plot(s1)
plot(s2)
plot(s3)
plot(s4)

