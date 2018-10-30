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

betta_test=function(betta, N=1000)
{
#   x=matrix(rnorm(2*N), ncol=2)
#   z=rnorm(N)
#   w=rchisq(n = N, df = 3)
  # s1=cbind(sqrt(1-betta^2))
  s1=sqrt(1-betta^2)*x+betta*z
  s2=s1/sqrt(w)*sqrt(3)
  s3=qnorm(pt(s2,3))
  return(c(normal=mean(apply(((s1) < qnorm(0.05)), 1, function(x) all(x) * 1)),
              student=mean(apply(((s3) < qnorm(0.05)),1,function(x) all(x)*1)),
              betta=betta
              ))
}
res=sapply(seq(0,1,by=0.01), betta_test)
par(mfrow=c(1,1))
plot(res[3,],res[1,], type="l", ylim=c(0,0.1))
lines(res[3,],res[2,], col="blue")
