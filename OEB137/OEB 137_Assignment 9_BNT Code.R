#### OEB 137 Assignment 9 ####
#### BNT Code ####

set.seed(137)
bats<-data.frame(individual=seq(1:480),
                 cave=as.factor(rep(c("Cumberland Caverns","Bristol Caverns","Lurray Caverns","Tuckaleechee Caverns"),each=120)),
                 colony=as.factor(c(rep(c("Cumb1","Cumb2","Cumb3"),each=40),
                          rep(c("Bris1","Bris2","Bris3"),each=40),
                          rep(c("Lur1","Lur2","Lur3"),each=40),
                          rep(c("Tuck1","Tuck","Tuck3"),each=40))),
                 sex=rbinom(480,1,.48),
                 BMI=as.numeric(rnorm(480,11,4)))
bats$lice<-as.numeric(22+(bats$sex*.5)*(bats$BMI*2.2)+rnorm(480))
bats[bats$cave=="Cumberland Caverns",]$lice<-bats[bats$cave=="Cumberland Caverns",]$lice+18
bats[bats$cave=="Bristol Caverns",]$lice<-bats[bats$cave=="Bristol Caverns",]$lice+35
bats[bats$cave=="Tuckaleechee Caverns",]$lice<-bats[bats$cave=="Tuckaleechee Caverns",]$lice-18
bats$sex<-as.factor(ifelse(bats$sex==1, yes="male", no="female"))
write.csv(bats, file="bat_lice.csv", row.names=F)


### Mixed-Effects Models ###
bats<-read.csv("bat_lice.csv")
library(lme4)
library(jtools)
#1. 
mod1<-lmer(lice~sex+BMI+(1|cave)+(1|colony), data=bats, REML=T)
summary(mod1)

#2. 
mod2<-lmer(lice~sex+BMI+(1|cave/colony), data=bats, REML=T)
jtools::summ(mod2, digits=4)

#3. 
mod3<-lmer(lice~sex+BMI+(1+BMI|cave/colony), data=bats, REML=T)
summ(mod3, digits=4)

#The intercept indicates that a female bat with a BMI of 0 should carry ~25 lice, 
#Male bats have, on average, 12.4 more lice than females, and an increase in one 
#unit of BMI increases the bat's lice load by ~0.47 lice. The majority of the random 
#variation in the data occurs between different caves

#### Time Series ####

#5
library(forecast)
co2<-read.csv("maunaloa.csv")
co2ts<-ts(co2$month.avg, frequency=12, start(1958,3))
co2decomp<-decompose(co2ts)
plot(co2decomp)
co2$deseas.avg<-co2ts-co2decomp$seasonal

with(co2, plot(dec.date, month.avg, type="l", col="red"))
with(co2, points(dec.date, deseas.avg, type="l", col="black"))

#6
randwalk<-rwf(co2ts, 960, drift=TRUE) #960 months until October 2100 (could also use 950 to get to Jan 2100)
plot(randwalk)
randwalk$mean[360] #october 2050 is 360 months from the last data in CO2 object









##### Other Junk Code I was messing around with ################################

co2forecasts <- HoltWinters(co2ts, gamma=T) ## delete beta and gamma and see what cahnges
co2forecasts$fitted
plot(co2forecasts)

co2forcasts2 <-forecast:::forecast.HoltWinters(co2forecasts, h=600) ## need 3 colons since it is unimported object
plot(co2forcasts2)

#7


plot(meanf(co2ts,h=600))
plot(snaive(co2ts, h=600)) ## based on moth of previous year








install.packages(c("devtools", "astsa", "mgcv"))
#devtools::install_github("SMAC-Group/simts")

library(astsa)
library(mgcv)
library(simts)

data(globtemp, package = "astsa")
globtemp = gts(globtemp, start = 1958, freq = 1, unit_ts = "C", name_ts = "Global Temperature Deviations", data_name = "Evolution of Global Temperatures")
plot(globtemp)


library(lubridate)
co2<-read.csv("maunaloa.csv")

with(co2, plot(dec.date, month.avg, type="l"))
with(co2, points(dec.date, deseas.avg, type="l", col="red"))
with(co2, points(dec.date, deseas.avg2, type="l", col="blue"))

co2ts<-ts(co2$month.avg, frequency=12, start(1958,3))
plot(co2ts)
co2decomp<-decompose(co2ts)
plot(co2decomp)
co2$deseas.avg2<-co2ts-co2decomp$seasonal

with(co2, plot(dec.date, month.avg, type="l", col="red"))
with(co2, points(dec.date, deseas.avg2, type="l", col="black"))

co2$ymd<-as.character(with(co2, paste(year,"/",month,"/",15)))
co2$ymd<-strptime(co2$ymd, format="%y/%m/$d")
co2$ymd<-as.Date(co2$ymd, format="%Y-%m-%d")
co2$jdate<-yday(co2$ymd)
