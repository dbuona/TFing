rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
setwd("~/Documents/git/TFing/OEB137/")
library(dplyr)
dater<-read.csv(file = "Narragansett Barnacles.csv")
head(dater) # completed Q1

table(dater$site)

aggregate(Diam~site,data=dater,NROW)

aggregate(Diam~site,data=dater,mean)

aggregate(dater$Diam,by=list(dater$site),FUN=sd, na.rm=TRUE) ## teach aggregate
dater %>% dplyr::group_by(site) %>% dplyr::summarise(count=n())


Gmean<-function(x){prod(x)^(1/length(x))
} 

?prod()
aggregate(dater$Diam,by=list(dater$site),FUN=Gmean)


dater$ID<-paste(dater$site,dater$plot, sep="_") ## paste

table(dater$ID)##patience

hist(dater$Diam) ## histogram 
dater$logDiam<-log(dater$Diam)

hist(dater$logDiam)

density<-data.frame(table(dater$ID))
predictor<-data.frame(dater$ID,dater$salinity,dater$DO)
predictor<-predictor[!duplicated(predictor), ]

colnames(predictor)<-c("ID","sal","do")


new.df<-merge(predictor,density)
plot(new.df$Dens~new.df$sal)
plot(new.df$Dens~new.df$do)
plot(dater$Diam~dater$salinity)
