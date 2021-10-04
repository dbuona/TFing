rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(lme4)
#library(arm)
library(ggplot2)
library(dplyr)
library(forecast)
setwd("~/Documents/git/TFing/OEB137")
source("12.2_Partial pooling with no predictors.R") # where data was cleaned


## Complete pooling regression
lm.pooled <- lm (y ~ x)
summary (lm.pooled)

## No pooling regression
lm.unpooled <- lm (y ~ x + factor(county) -1)
summary(lm.unpooled)

## Comparing-complete pooling & no-pooling (Figure 12.2)
x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)  # counties to be displayed
y.range <- range (y[!is.na(match(county,display8))])

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="soil depth", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1,
        main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, add=TRUE)
}

## parital pooling model (random effect)
## Including x as a predictor
M1 <- lmer (y ~ x + (1 | county))
display (M1)

# estimated regression coefficicents
coef (M1)

# fixed and random effects
fixef (M1)
ranef (M1)

a.hat.M1 <- coef(M1)$county[,1]                # 1st column is the intercept
b.hat.M1 <- coef(M1)$county[,2]                # 2nd element is the slope


for (j in display8) {
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="soil deptg", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1,
        main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, add=TRUE)
  curve (a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, col="black", add=TRUE)
}

## varying slope and intercept
M3 <- lmer (y ~ x + ( x | county))
display (M3)

# plots on Figure 13.1
a.hat.M3 <- fixef(M3)[1] + ranef(M3)$county[,1] 
b.hat.M3 <- fixef(M3)[2] + ranef(M3)$county[,2]

b.hat.unpooled.varying <- array (NA, c(J,2))
for (j in 1:J){
  lm.unpooled.varying <- lm (y ~ x, subset=(county==j))
  b.hat.unpooled.varying[j,] <- coef(lm.unpooled.varying)
}


par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="soil depth", ylab="log radon level", cex.lab=1.2, cex.axis=1.1,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.1, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.1)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, lty=2, col="gray10", add=TRUE)
  curve (b.hat.unpooled.varying[j,1] + b.hat.unpooled.varying[j,2]*x, lwd=.5, col="gray10", add=TRUE)
  curve (a.hat.M3[j] + b.hat.M3[j]*x, lwd=1, col="black", add=TRUE)
}


###time series
# from https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
precip <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")


prec.ts <- ts(precip, frequency=12, start=c(1946,1)) ## time series of births (lets pretend its actualyl cliamte data)
par (mfrow=c(1,1), mar=c(2,2,3,1))
plot(prec.ts)

##decompose seasonality
prec.decom<-decompose(prec.ts)


plot(prec.decom)
#### remove seasonality
seas.adj <- prec.ts - prec.decom$seasonal
plot(seas.adj)


##forecast
?HoltWinters()
precforecasts <- HoltWinters(prec.ts,beta=FALSE,gamma=FALSE) ## delete beta and gamma and see what cahnges
precforecasts$fitted
plot(precforecasts)


precforcasts2 <-forecast:::forecast.HoltWinters(precforecasts, h=40) ## need 3 colons since it is unimported object
plot(precforcasts2)




plot(meanf(prec.ts,h=35))
plot(snaive(prec.ts, h=35)) ## based on moth of previous year
plot(rwf(prec.ts, 35, drift=TRUE))
