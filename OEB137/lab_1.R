#OEB 137 Lab 1: Introduction to R.

#what we'll cover
#0.1) What is R
#1) assignment
#2) Components Console, Environment, Script
#3) packages
#4)  working directory, read in data
#4.1) What is a data frame? how to make one (in brief)-- we'll come back to this when we start simulating our own
#5) Exploring your data functions
#5.1 data classes
#6) Exploratory plots in ggplot and plot r Why do theme
#7) Indexing data.frame
#8) making a new column
#9) cleaning colnames and individual cells (which commands or ifelse, overwriting)
#10)combining data frames

##R is a community
##R is open source
##R is an interface
##R is a language with many regional dialects (eg plot vs ggplot2)
  ##HELP resources outside of R google rcheats



#Part 1: R is a fancy calculator (Jim Regetz)

#R remembers (assignment statements)
617-312
x<- 617
x
x <- 312 ##overwrite
x
#rm(x)

jenny <- 867.5309
jennyUnlisted <- jenny- x ### teach
jennyUnlisted


##getting started
#Console, Environment, Script 
rm(list=ls()) ##this clears your global environement
options(stringsAsFactors = FALSE) ##if you care about this https://simplystatistics.org/2015/07/24/stringsasfactors-an-unauthorized-biography/#:~:text=The%20argument%20'stringsAsFactors'%20is%20an,argument%20also%20appears%20in%20'read.
graphics.off() ## shuts down graphic devices
##you can make a script dictionary

#### load packages
#include functions, data, vignettes
#install.packages("dplyr") ## first time you do it or after you update R
library(dplyr) ## or require()

#have students make a folder on their desktop OEB137
setwd("~/Desktop/OEB137")

#iris2020$Sepal.Length<-iris2019$Sepal.Length+1 ## this is what I did to make the broken data
#iris2020$Sepal.width<-iris2019$Sepal.width*10
#write.csv(iris2020,"iris2020.csv",row.names = FALSE)

###reading in data
iris
iris2019<-iris
##read in your own data
iris2020<-read.csv(file = "iris2020.csv")


## some querying functions for iris2019
head(iris2019)
tail(iris2019)
#View(iris)##rstudo
ncol(iris2019)
nrow(iris2019)
dim(iris2019)
table(iris$Species)
unique(iris2019)
colnames(iris2019)

###brief interlude about class. We'll talk more about this when we model
#also talk here about vectors,list, lists. matrices dataframe is a list of equal length vectors
##A vector is a collection of one or more values, all of the same type
##Lists can hold many many types of objects in one container
# a dataframe is a list of vectors of equal length

class(iris2019$Species)
class(iris2019$Sepal.Length)

plot(iris$Species)
plot(as.numeric(iris$Species))

plot(iris$Sepal.Length)
plot(as.integer(iris$Sepal.Length))
plot(as.factor(iris$Sepal.Length))



##attach and why i dont do it https://www.r-bloggers.com/to-attach-or-not-attach-that-is-the-question/
attach(iris2019)
table(Species)
detach(iris2019)
table(iris2019$Species)



##sidebar build your own df 
observation<-c(1,2,3,4,5,6) #equivlent to
observation2<-1:6
observation3<-c("one","two","three","four","five","six")
my.df<-data.frame(observation=observation,observation2=observation2,observation3=observation3)
#query

##back to iris2019 indexing
colnames(iris2019)
colnames(iris2019)[c(2,3)]
iris[2,3]
which(iris$Petal.Width == 1)

##explore iris2019 data

plot(iris2019$Sepal.Length~iris2019$Species)
plot(iris2019$Sepal.Length~iris2019$Sepal.Width)
library(ggplot2)
ggplot(iris2019,aes(Sepal.Width,Sepal.Length))+geom_point(aes(color=Species))
ggplot(iris2019,aes(Species,Sepal.Length))+geom_point(aes(color=Species))
ggplot(iris2019,aes(Species,Sepal.Length))+geom_boxplot(aes(color=Species))

##many tutorials tell you how to explore your data, but this will show you why you should


##Why this matters do the same with iris2020
ggplot(iris2020,aes(Sepal.Width,Sepal.Length))+geom_point(aes(color=Species))

ggplot(iris2020,aes(Sepal.Width,Sepal.Length))+geom_point(aes(color=Species))

table(iris2020$Species)
range(iris2019$Sepal.Width)
range(iris2020$Sepal.Width)

unique(iris2020$Species)

##fix the data
iris2020$Species[which(iris2020$Species=="setoza" | iris2020$Species=="setsa")] #creak this function foen and add <-"setosa"

iris2020$Sepal.Width<-iris2020$Sepal.Width/10

###add a new column
iris2020$year<-2020
iris2019$year<-2019

iris.multi<-rbind(iris2019,iris2020) ## try to combind them
### errors are hard to understand

colnames(iris2020)
colnames(iris2019)
colnames(iris2020)<-colnames(iris2019)
iris.multi<-rbind(iris2019,iris2020)

##next week; continue whatever we dont get through, more on building your own data
#seq(), rep(), rnorm(). and dplyr, tidy r functions. a bit more on plotting

