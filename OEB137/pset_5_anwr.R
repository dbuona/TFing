rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
library(tidyverse)
library(pwr)
setwd("~/Documents/git/TFing/OEB137/")
d<-read.csv("stomcon.csv")
d.spar<-dplyr::filter(d,spp=="Spartina")
#n=100
cor(d.spar$stom.con,d.spar$temp)
pwr.r.test(n=100,r=0.7928911,sig.level = 0.05)

d %>% dplyr::group_by(spp) %>% dplyr::summarise(mean=mean(stom.con),sd=sd(stom.con),n=n())

mean_cont <-14.2
mean_treat <-36.2
sd_cont <- 1.84
sd_treat <- 1.53
n_cont <- 100
n_treat <- 100

mean_diff <- mean_cont - mean_treat

pooled_sd_n <- ((n_treat-1)*(sd_treat^2)) + ((n_cont-1)*(sd_cont^2))
pooled_sd_d <- ((n_treat + n_cont) - 2)
pooled_sd <- sqrt(pooled_sd_n/pooled_sd_d)

cohens_d <- mean_diff / pooled_sd

pwr.t.test(n = 100,sig.level = 0.05,d=cohens_d)

summary(lm(d$stom.con~d$temp))
summary(lm(d$stom.con~d$temp+d$soil.mois))

modelhere <- lm(d$stom.con~d$temp+d$soil.mois) # m2l.nistudy 
observed.here <-d$stom.con

mod.sum <- summary(modelhere)
mod.sum[grep("Estimate", rownames(mod.sum)),] 

# getting predicted values if needed
preds.mod.sum <- mod.sum[grep("yhat", rownames(mod.sum)),]

# Here's our method to calculate R sq
mod.R2 <- 1- sum((observed.here-preds.mod.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

# Which seems correct! See  https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
rsq <- function (x, y) cor(x, y) ^ 2
rsq(observed.here, preds.mod.sum[,1])
summary(lm(preds.mod.sum[,1]~observed.here))



(0.0196-0.01959)/(1-0.0196)

pwr.f2.test(2,298,f2 =(0.0196-0.01959)/(1-0.0196))


##