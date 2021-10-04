#lab 5 power analyses
# 07 October 2020

rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(pwr)
## install.packages("") if you've never loaded this before
library(effsize)
## install.packages("") if you've never loaded this before

###############################
##what is  statisitcal power?####
#################################
pwr.t.test(d = 0.5,sig.level = 0.05,power=0.8)

pwr.t.test(d=0.7,n=40, sig.level = 0.05)

#https://www.statmethods.net/stats/power.html
mean_cont <-67
mean_treat <-75
sd_cont <- 10
sd_treat <- 12
n_cont <- 50
n_treat <- 50

df<-data.frame(treatment=rep(c("control","manipulated"),each=50))
df$y<-ifelse(df$treatment=="control",rnorm(n_cont,mean_cont,sd_cont),rnorm(n_treat,mean_treat,sd_treat))
library(ggplot2)
ggplot(df,aes(y))+geom_density(aes(fill=treatment))

###hard coded cogend
mean_diff <- mean_cont - mean_treat

pooled_sd_n <- ((n_treat-1)*(sd_treat^2)) + ((n_cont-1)*(sd_cont^2))
pooled_sd_d <- ((n_treat + n_cont) - 2)
pooled_sd <- sqrt(pooled_sd_n/pooled_sd_d)

cohens_d <- mean_diff / pooled_sd

cohen.d(df$y,df$treatment)

pwr.t.test(sig.level = 0.05, n=50,power=0.8)

###Anova
pwr.anova.test(k=5,f=.25,sig.level = 0.05, power=0.9)

#correlation
pwr.r.test(n=10,r=0.6, sig.level=0.05)

# regression
pwr.f2.test(u=2,v=97,f2 = 0.15, sig.level = 0.05)

#modified from https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/
print(paste("The year is", 2018))
print(paste("The year is", 2019))
print(paste("The year is", 2020))

year<-2018:2038
for (i in c(year)){
  print(paste("The year is",i))
}

t<-seq(0.2,0.8,by=0.1)
p<-seq(0.5,0.9,by=0.1)

results<-data.frame(effect_size=numeric(),power=numeric(),N=numeric())

for (j in 1:length(p)){
  for (i in 1:length(t)){
  result<-pwr.t.test(d=t[i],sig.level = 0.05,power=p[j])
  sample<-ceiling(result$n)
  resultshere<-data.frame(effect_size=t[i],power=p[j],N=sample)
  results<-rbind(results,resultshere)
  }
}

ggplot(results,aes(effect_size,N))+geom_point(aes(color=as.factor(power)))+
  stat_smooth(aes(color=as.factor(power)),se = FALSE,size=0.2)
