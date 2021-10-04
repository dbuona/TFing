rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
library(dplyr)
set.seed(137)



year<-1970:2020



df<-data.frame(year=numeric(),chillunits=numeric(),meanT=numeric(),meanP=numeric(),site=numeric(),sensor=numeric())

site<-1:20
sensor<-1:15
for (i in c(1:length(site))){
for (j in c(1:length(sensor))){

dfhere<-data.frame(year=year,chillunits=rnorm(length(year),500,100),meanT=rnorm(length(year),22,5),meanP=rnorm(length(year),39,5),site=site[i],sensor=sensor[j])
df<-rbind(df,dfhere)
}}
sp<-data.frame(sp_num=1:20)

d<-merge(df,sp)

alphas<-data.frame(sp_num=numeric(),alpha=numeric())

for (i in unique(d$sp_num)){
alpha<-rnorm(1,150,20)
  alphashere<-data.frame(sp_num=unique(d$sp_num)[i],alpha=alpha)
  alphas<-rbind(alphas, alphashere)
  }

d<-left_join(d,alphas)

d$y<-d$alpha-0.03*d$chillunits-2*d$meanT+0.03*d$meanP+rnorm(306000,0,10)

d<-dplyr::select(d,-alpha)

species<- c("Q.rubra", "Q.robur", "Q.macrocarpa","Q.alba","Q.velutina",
            "Q.imbricaria","Q.coccinea", "Q. mulenbergii", "Q. douglasii", "Q. agrifola",
            "P.serotina","P.pensylvanica","P.virginiana", "P.mexicana","P.avium",
            "P.maritima","P.caroliniana","P.persica","P.domestica","P.spinosa")
spnam<-data.frame(sp_num=1:20,species=species)
d<-left_join(d,spnam)

## Ball drop sensors turn on Dec 31, and have never been left censorted
##1 how many years, species, sites and sensors are included in my study?##

range(d$meanT)
d$meanT[which(d$year==1986)]<-d$meanT-20
d$meanP[which(d$year==1991 & d$sensor==3 & d$site==11)]<-d$meanP+100
d$species[which(d$species=="Q.rubra" & site==13)]<-"q.rubra"
unique(d$species)
range(d$meanP)
quantile(d$meanT)


write.csv(d,"big_data.csv")
# find the range of values for y (day of year flowering)

#########################################################
#### Part II: Statistical vs. Ecological significance ###
#########################################################

options(scipen=999) ## removes scientific notation
flo.mod<-lm(d$y~d$chillunits+d$meanT+d$meanP)
summary(flo.mod)

###which predictors are statistically significant?###

#### Talk through how to interpret the coefficents

###Which predictors are biologically significant####

(max(d$chillunits)-min(d$chillunits))*-0.03
unname(quantile(d$chillunits)[4]-quantile(d$chillunits)[2])*-0.03

(max(d$meanT)-min(d$meanT))*-2
unname(quantile(d$meanT)[4]-quantile(d$meanT)[2])*-2

(max(d$meanP)-min(d$meanP))*0.03
unname(quantile(d$meanP)[4]-quantile(d$meanP)[2])*0.03

ggplot(d,(aes(meanT,y)))+geom_point()+geom_smooth(method="lm")

d$temp_bin<-NA
quantile(d$meanT)
d$temp_bin[which(d$meanT<18.36955)] <- 0
d$temp_bin[which(d$meanT>=18.36955 &d$meanT<21.778)] <- 1
d$temp_bin[which(d$meanT>=21.778 &d$meanT<25.20026)] <- 2
d$temp_bin[which(d$meanT>=25.20026 &d$meanT<40.80332)] <- 3
d$temp_bin[which(d$meanT>=40.80332)] <- 4
?stat_summary()

ggplot(d,(aes(temp_bin,y)))+geom_point(color="gray",size=0.5)+stat_summary()+geom_smooth(method="lm")






#is this realistic


negys<-d[which(d$y<0),] 

## assume they got sign flipped)


d$y[which(d$y<0)] <- abs(d$y)




library(ggplot2)
ggplot(d,aes(year,meanT))+geom_smooth()

range(goo$y)
#### part 3 statistical significance vs. ecological significance
options(scipen=999) ## rem
summary(lm(d$y~d$chillunits+d$meanT+d$meanP))


(max(d$chillunits)-min(d$chillunits))*-0.03
unname(quantile(d$chillunits)[4]-quantile(d$chillunits)[2])*-0.03

(max(d$meanT)-min(d$meanT))*-2
unname(quantile(d$meanT)[4]-quantile(d$meanT)[2])*-2

(max(d$meanP)-min(d$meanP))*0.03
unname(quantile(d$meanP)[4]-quantile(d$meanP)[2])*0.03
