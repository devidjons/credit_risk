library(dplyr)
dane=read.csv("http://www2.im.uj.edu.pl/DariuszZawisza/01logit.csv", sep=";", header=T, dec=",")
B=sample(1:length(dane[,1]), length(dane[,1]))
dane=dane[B,]
train=dane[1:2400,]
test=dane[2400:4000,]
head(train)
model=glm(Default ~ EBIT.TA,data=train, family=binomial(link="logit"))
summary(model)
pd=predict(model, test, type="response")
boxplot(pd)
library(pROC)
roc_point=function(real, theoretical, level=0.5)
{
    theoretical[theoretical<level]=0
    theoretical[theoretical>=level]=1
    r1=sum(theoretical==0 & real==0)/sum(real==0)
    r2=sum(theoretical==0 & real==1)/sum(real==1)
    
    return(1-c(r1,r2))
}

points1=sapply(seq(0,1,by=0.0001), function(x) roc_point(test$Default, pd, x))
par(mfrow=(c(1,2)))
plot(0,0,xlim=c(0,1), ylim=c(0,1))
lines(points1[1,], points1[2,])
roc(test$Default,pd)%>%plot

