# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("Documents/git/TFing/OEB137/")
beet<-read.csv("beetles.csv")
colnames(beet)
cor(beet$hornSize,beet$wingMass)
#-0.5

#2
horn.mod<-lm(beet$wingMass~beet$hornSize)
plot(resid(horn.mod))
summary(horn.mod)

#3
plot(beet$wingMass, resid(horn.mod), 
            ylab="Residuals", xlab="wing mass") 
abline(0, 0)

#4
hum<-read.csv("NeanderthalBrains.csv")
brain<-lm(hum$lnBrain~hum$lnMass*hum$species)
summary(brain)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    4.2535     0.9769   4.354 0.000111 ***
#  hum$lnMass                     0.7135     0.2270   3.143 0.003395 ** 
#  hum$speciesrecent              1.1809     1.0623   1.112 0.273862    
#hum$lnMass:hum$speciesrecent  -0.2595     0.2481  -1.046 0.302790   

hum$predy<-predict(brain)
ggplot(hum,aes(lnMass,predy))+stat_smooth(aes(color=species))


fruit<-read.csv(("treefruit.csv"))
fruity<-glm(fruit$fruit~fruit$height,family=binomial(link="logit"))
summary(fruity)
exp(coef(fruity))
