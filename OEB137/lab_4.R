rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()
### lab 4 Simulating data and metadata
library(ggplot2)
library(ggpubr)

set.seed(137)

treatment<-rep(c("control","fertilizer"),each=20)
treatment

response<-ifelse(treatment=="control",rnorm(20,50,10),rnorm(20,80,11))
mydata<-data.frame(treat=treatment,resp=response)

ggplot(mydata,aes(treat,response))+geom_boxplot()

#### two different categforical treatments
treatment1<-rep(c("level1","level2"),each=2,20)
treatment2<-rep(c("level1","level2"),each=1,40)

mydata2<-data.frame(treat1=treatment1,treat2=treatment2)

resp<-rnorm(80,c(1000,200,900,400),sd=50)
AllData<-data.frame(resp,treatment1,treatment2)

ggplot(AllData,aes(treatment1,resp))+geom_boxplot(aes(color=treatment2))

resp<-rnorm(80,c(100,300,200,900),sd=50)
AllData<-data.frame(resp, treatment1,treatment2)

###
treatment<-rep(c(0:3),10)##dummy variable
resp<-rnorm(40,c(50,100,150,250),sd=20)
linear<-data.frame(treatment,resp)

plot(linear$treatment~linear$resp)
ggscatter(linear,'treatment','resp',
add= "reg.line",
cor.coef="TRUE")

## y=a+bx+e
rm(list=ls())
set.seed(137)
alpha<-100
beta<-0.3
sigma<-1

x<-runif(n = 100,min = 1,max = 100)

y<-rnorm(length(x),alpha+beta*x,sigma)
df<-data.frame(y,x)
plot(df$y~df$x)
summary(lm(df$y~df$x))

treatment<-c(0,1)
treat.coef<--2
##a +bx+b2z+e

y<-rnorm(length(x)*length(treatment),alpha+beta*x+treat.coef*treatment,sigma)
df<-data.frame(y=y,x=x,treatment=rep(treatment,100))

ggplot(df, aes(x,y))+geom_point(aes(color=as.factor(treatment)))
