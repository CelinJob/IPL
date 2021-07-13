# IPL
https://github.com/CelinJob/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv
IPL Ball-by-Ball 2008-2020.csv
#A1b
setwd("C:https://github.com/CelinJob/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv")
df= read.csv('IPL Ball-by-Ball 2008-2020.csv', na.strings = "" )
names(df)
head(df)
tail(df)
class(df$date)
View(df)

library(dplyr)
runs  = df%>%
  group_by(batsman, id)%>%
  summarize(scores= sum(batsman_runs))
View(runs)
unique(runs$batsman)
dim(runs)

runsbatsman=aggregate(cbind(runs$scores),by=list(runs$batsman),FUN=sum)
colnames(runsbatsman)=c("batsman","total_runs")
View(runsbatsman)
hist(runsbatsman$total_runs,freq=F,main="Total Runs")

smith= runs[runs$batsman == 'GC Smith',]
View(smith)
dim(smith)
hist(smith$scores,6)
descdist(smith$scores)
#continuous distribution
#H0:Weibull distribution fits the data
#H1:Weibull distribution does not fit the data

#fitting distribution
library(MASS)
library(fitdistrplus)
#H0:distribution is weibull
#H1:distribution is not weibull
res<-fitdistr(smith$scores+0.00001,"weibull",lower=0.001)
res$estimate
smithwei = rweibull(1000, shape=0.4796084, scale = 17.7071996 )
plot(density(smithwei))
ks.test(smith$scores,"pweibull", scale=17.7071996, shape=0.4796084 )
#p value is more than 0.05.Therfore we accept null hypothesis.The distribution follows weibull distribution.

#H0:distribution is gamma
#H1:distribution is not gamma
res1<-fitdistr(smith$scores+.001,"gamma",lower=0.001)
res1$estimate
smithgam = rgamma(1000, shape= 0.44049901, rate = 0.01728549)
plot(density(smithgam))
lines(density(smithwei))
ks.test(smith$scores+0.001,"gamma") 
#p value is less than 0.05.Therfore we reject null hypothesis.The distribution does not  follows gamma distribution.

#H0:distribution is normal
#H1:distribution is not normal
res2<-fitdistr(smith$scores+.001,"normal",lower=0.001)
res2$estimate
smithnorm = rnorm(1000, mean= 25.48376, sd =25.37120 )
plot(density(smithnorm))
lines(density(smithnorm))
ks.test(smith$scores,"pnorm",mean= 25.48376, sd =25.37120)
#p value is more than 0.05.Therfore we accaept null hypothesis.The distribution   follows normal distribution.

#H0:distribution is lognormal
#H1:distribution is not lognormal
res3<-fitdistr(smith$scores+.001,"lognormal",lower=0.001)
res3$estimate
smithlnorm = rlnorm(1000, mean=1.767086 , sd =3.186289 )
plot(density(smithlnorm))
lines(density(smithlnorm))
ks.test(smith$scores,"plnorm",meanlog= 1.767086, sd =3.186289) 
#p value is more than 0.05.Therfore we accept null hypothesis.The distribution   follows normal distribution.





hist(smith$scores, prob=TRUE)
curve(dweibull(x,0.4796084,17.7071996),col="red",lwd=2,add=T)
curve(dexp(x,rate= 0.03924068 ),col="green",lwd=2,add=T)
curve(dgamma(x,0.44049901, 0.01728549),col="blue",lwd=2,add=T)
lines(density(smith$scores),col="pink")






wickets=df%>%
  group_by(bowler,id)%>%
  summarize(wicket=sum(is_wicket))
View(wickets)
unique(wickets$bowler)
dim(wickets)
View(wickets)
wickets1=as.numeric(wickets$wicket)
hist(wickets1,freq=F,main="wickets")
wicketbowler=aggregate(cbind(wickets$wicket),by=list(wickets$bowler),FUN=sum)
colnames(wicketbowler)=c("bowler","total_wickets")
View(wicketbowler)
hist(wicketbowler$total_wickets,freq=F,main="Total wickets")



har= wickets[wickets$bowler == 'Harbhajan Singh',]
View(har)
dim(har)
hist(har$wicket,freq=F,breaks=c(0,1,2,3,4,5),main="Take a look to data Harbhajan Singh")
m=mean(har$wicket);std=sd(har$wicket);m;std
lines(density(har$wicket),col="blue")

#H0:distribution is poisson
#H1:distribution is not  poisson

library(vcd) # For discrete data
gf.har<- goodfit(har$wicket, type = "poisson", par = NULL)
summary(gf.har)
plot(gf.har, main="har wickets distribution")
#p value is more than 0.05.Therfore we accept null hypothesis.The distribution follows  poisson distribution.
