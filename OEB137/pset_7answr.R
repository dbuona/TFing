rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

setwd("/Users/danielbuonaiuto/Documents/git/TFing/OEB137/")

d<-read.csv("nastyfish.csv")

#1)
table(d$species)
nova1<-aov(d$perc.inf~d$species)
summary(nova1)
#2)
nova.block<-aov(d$perc.inf~d$species+d$site)
summary(nova.block)

#3)
dat<-read.csv("grzinglwns.csv")
table(dat$herb)
table(dat$dung)
colnames(dat)
nova3<-aov(plnt.ht~herb*dung,data=dat)
summary(nova3)

TukeyHSD(nova3)


TUKEY<-TukeyHSD(x=nova3, c("herb:dung"), conf.level=0.95)


#library("agricolae")
#H#SD.test(nova3, c("herb:dung"), console=TRUE)

#library(ggplot2)
library("multcompView")
ggplot(dat,aes(dung,plnt.ht))+geom_boxplot(aes(fill=herb))

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "dat$dung")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )
