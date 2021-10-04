rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
library(dplyr)
library(ggplot2)
setwd("..//danielbuonaiuto/Documents/git/TFing/OEB137/")
d<-read.csv("big_data.csv")

# We recorded climatic data,day of flowering (y)
#for a bunch of species at many site with many sensor over many year
## "Ball drop" sensors-- turn on Dec 31, 
#and have never been left censored (nothing happens before day 1)

##########################################################################
##1) how many years, species, sites and sensors are included in my study?##
######## Fix any mistakes you find here ##############################
#########################################################################
unique(d$sp_num)
unique(d$species)
#######################################################################
#2) mean T is the mean spring temperature (March & April) ########
#do these values seem reasonable? #####################################
##if not where is the error? Do you think is biological? If so why? ####
######################################################################
quantile(d$meanT)
ggplot(d,aes(year,meanT))+stat_summary()
d$year[which(d$meanT<5)]

d.86<-filter(d,year==1986)

ggplot(d.86,aes(site,meanT))+geom_point(aes(color=as.factor(sensor)))
#########################################################################
# 3) mean  P is mean precipitation Jan-May
#do these values seem reasonable?
##if not where is the error. Do you think is biological? If so why?
quantile(d$meanP)
d$year[which(d$meanP>100)]
d$site[which(d$meanP>100)]
d$sensor[which(d$meanP>100)]
########################################################################

##################################################################################
# 4) If you wanted to analyze differnces between genera, what would you need to do? 
###################################################################################
options(scipen=999) ## removes scientific notation
flo.mod<-lm(d$y~d$chillunits+d$meanT+d$meanP)
summary(flo.mod)

(max(d$chillunits)-min(d$chillunits))*-0.03
unname(quantile(d$chillunits)[4]-quantile(d$chillunits)[2])*-0.03

(max(d$meanP)-min(d$meanP))*0.03
unname(quantile(d$meanP)[4]-quantile(d$meanP)[2])*0.03

(max(d$meanT)-min(d$meanT))*-2
unname(quantile(d$meanT)[4]-quantile(d$meanT)[2])*-2
