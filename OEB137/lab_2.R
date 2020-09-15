##functions     We talked a bit about vectors, object, and in doing so we encountered functions.
#today we are going to break down what a function is in the context of distrubtions and practice writing out own
# housekeeping
rm(list=ls())
library(dplyr)
options(stringsAsFactors = FALSE)

##part 1 functions and decsriptive statistics
##go throught the parts of a function, arguements and show ? feature
mean(iris$Petal.Length) #na.rm=TRUE or FALSE (logical statements) see that = sign is withing the function
mydata<-c(5:10,NA,12)
mean(mydata,na.rm=TRUE) ### explain defaults abd why they sometimes mess you up

sd(iris$Petal.Length) ## more descriptive statistics
var(iris$Petal.Length) ##""
se(iris$Petal.Length)# there is no standard error function

##but was it stanmdar error ( sqrt(varience/n)) and we can make a function

sqrt(4)
sqrt(iris$Petal.Length) ## note this applies the function over the whole vector

###why parenthesizes matter
sqrt(var(iris$Petal.Length)/length(iris$Petal.Length)) ### this is the equation to calculate standard error
sqrt(var(iris$Petal.Length))/length(iris$Petal.Length) #just like in real math parenthesize matter, and will change your order of opporations
sqrt(var(iris$Petal.Length))/length(iris$Petal.Length)) #but here they also matter to make you code run


## part2 write your own function
std.err <- function(x) { #but we can make a function of it
  sqrt(var(x)/length(x))
}

std.err(iris$Petal.Length) ##see?

mult.sd <- function(x,y) { ##functions can have multiple areguements
  sd(x)*y
}
mult.sd(iris$Petal.Length,1)#68% of data
mult.sd(iris$Petal.Length,2) #95% of data



div3plusy<- function (x,y) { ## functions can be simple mathmatical opperations 
  x/3+y
}

div3plusy(12,10) 

## now everyone write their own 3 argument function (x,y,z)


### so far we've only looked at the full data but what if we are interested in differnces amoun species
?aggregate()
aggregate(Petal.Length~Species,data=iris,mean)
## alt specification
aggregate(x=iris$Petal.Length,by=list(iris$Species),FUN=mean)
aggregate(Petal.Length~Species,data=iris,std.err) ## you can add your own functiona s long as you've stored it in the global environment

###unique identifiers
iris$plot<-rep(1:5,each=5)
aggregate(Petal.Length~plot,data=iris,mean)#### this mixes species in the plots
iris$ID<-paste(iris$Species,iris$plot) ## makea  unique sp plot label

aggregate(Petal.Length~ID,data=iris,mean)

##visualize distrubtions
hist(iris$Sepal.Width)


## if there is time, comparison of  tidyverse to aggreegate
aggregate(Petal.Length~Species,data=iris,mean)


iris.goo<-iris %>% group_by(Species) %>% summarise(mean.petal.length=mean(Petal.Length))
###ggplot to hist



