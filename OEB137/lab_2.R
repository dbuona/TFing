# Hi everyone,
# welcome to lab

# housekeeping
rm(list=ls()) ##clears your global env
options(stringsAsFactors = FALSE)## weird R formatting

mean(iris$Sepal.Length)
?mean()
mydata<-c(5,10,30,4,NA,12)
mean(mydata)
mean(mydata,na.rm = TRUE)

sd(iris$Sepal.Length)
var(iris$Sepal.Length)
se(iris$Sepal.Length)

sqrt(4)
sqrt(var(iris$Sepal.Length)/length(iris$Sepal.Length))
sqrt(var(iris$Sepal.Length))/length(iris$Sepal.Length)

##writing your own standard error function

std.err<- function(x){
  sqrt(var(x)/length(x))
}

std.err(iris$Sepal.Length)
std.err(iris$Petal.Length)     

mult.sd<- function(x,y) {
  sd(x)*y
}

mult.sd(iris$Sepal.Length,1)
mult.sd(iris$Sepal.Length,2)

div3plusy<-function(x,y){
  (x/3)+y
}

div3plusy(12,10)

##assignment make a function with 3 arguements
makeafunction<- function(x,y,z){
}

mean(iris$Sepal.Length)
unique(iris$Species)

aggregate(Petal.Length~Species,data=iris,mean)
aggregate(x=iris$Petal.Length,by=list(iris$Species),mean)

aggregate(Petal.Length~Species,data=iris,std.err)

install.packages("dplyr")
library(dplyr)

iris %>% 
  group_by(Species) %>%  
  summarise(mean.petal.length=mean(Petal.Length))

head(iris)
iris$plot<-rep(1:5,each=5)
?rep()
head(iris,10)
aggregate(Petal.Length~Species+plot,data=iris,mean)
iris$ID<-paste(iris$Species,iris$plot)
head(iris)

plot(iris$Sepal.Length~iris$Sepal.Width)
hist(iris$Sepal.Width)

install.packages("ggplot2")
library(ggplot2)

ggplot(iris,aes(Sepal.Width,Sepal.Length))+
  geom_point(aes(color=Species))+
  geom_smooth(method="lm",aes(color=Species))

