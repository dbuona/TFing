##housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

#Lab 3 
install.packages("ggpubr")
library(ggpubr)
#The 'ggplot2' package is excellent and flexible for elegant data visualization in R. 
#However the default generated plots requires some formatting before we can send them for publication. 
#Furthermore, to customize a 'ggplot', the syntax is opaque and this raises the level of difficulty for researchers with no advanced R programming skills. 
#'ggpubr' provides some easy-to-use functions for creating and customizing 'ggplot2'- based publication ready plots.


rnorm(n = 1000,mean = 50,sd = 10)

hist(rnorm(n = 1000,mean = 50,sd = 10))

rnorm(10,50,10)

#load libraries before you set seed:https://livefreeordichotomize.com/2018/01/22/a-set-seed-ggplot2-adventure/
set.seed(137)
rnorm(10,50,10)
rnorm(10,50,10)

lazysample<-rnorm(10,50,10)
jOfbiostuff<-rnorm(100,50,10)
sciencesample<-rnorm(1000,50,10)

gghistogram(lazysample,bins=5)
hist(lazysample, breaks=5)
gghistogram(sciencesample)

rbinom(n = 10,size = 1,prob = .5)
rbinom(10,10,.5)

berny<-rbinom(1000,1,.8)
gghistogram(berny)
hist(berny)

poisy<-rpois(n = 10,lambda = 2.4)
gghistogram(poisy,bins=5)
hist(poisy,breaks=10)

logy<-rlnorm(n = 100,meanlog = 100,sdlog = 2)
hist(logy,breaks=20)

ggdensity(sciencesample)

ggqqplot(lazysample)

library(car) 
qqPlot(lazysample)
qqnorm(lazysample)

shapiro.test(jOfbiostuff)# if p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.

set.seed(42)
rnorm(10,50,10)
rnorm(10,30,20)

##
ggarrange(gghistogram(sciencesample),ggdensity(sciencesample))

par(mfrow=c(2,2))
hist(sciencesample)
qqnorm(sciencesample)
dev.off()

location<-rep(c("Harvard","Yale"),each=20)

dbh<-rep(c(rnorm(20,86,12),rnorm(20,44,19)))

data.frame(location=location,diamter=dbh)

ifelse()
df<-data.frame(location=location)

df$dbh<-NA
df$dbh<-ifelse(test=df$location=="Harvard",yes = rnorm(20,86,12),no=rnorm(20,44,19))
