
617-312 #Basic Math

x<-617 ##assign an object
x<-312 #overwrite that

jenny<-867.5309  #assign another object
jenny

jennyUnlisted<-jenny-x ##assign an object based on two exisiting objects
jennyUnlisted

rm(x)
rm(list=ls()) #removes everything from the environment
options(stringsAsFactors = FALSE)
#https://simplystatistics.org/2015/07/24/stringsasfactors-an-unauthorized-biography/#:~:text=The%20argument%20'stringsAsFactors'%20is%20an,argument%20also%20appears%20in%20'read.

install.packages("ggplot2") ##install a brand new packages
library(ggplot2) # load the package
require() #alternative way to load the package

setwd("~/Documents/git/TFing/OEB137") ## set your working director
iris #print a pre-loaded data set
iris2019<-iris #assign that data set a new name

iris2020<-read.csv("iris2020.csv") ##read in your own data from your working directory

iris2019$Species #print a signle column (vector) from the data frame

unique(iris2019$Species) ## this function shows you all the unique values in  the "Species" column
unique(iris2020$Species) # it can be useful for spotting typos and mistakes

table(iris2020$Species) #this shows you how many cells of each unique values

plot(iris$Sepal.Length~iris$Species) ### basic exploratory plot

class(iris$Sepal.Length)## this function tells you the underlying structure of a vector
class(iris$Species) ##note the differences

##side bar
attach(iris2019) # you could attach data (x) 
#which mean you dont have to referece the x$ part of the x$y statement
Species #see?
detach(iris2019) # I don't do this-- for further reading:
#https://www.r-bloggers.com/to-attach-or-not-attach-that-is-the-question/

colnames(iris2019) ##this function prints the column names of a data frame

colnames(iris2020)[3] # The bracket index a specific column

colnames(iris2020)[3]<-"Petal.Length" # here we reassign column 3 the proper name
colnames(iris2020) # look its fix

View(iris2019) ## This allows you to see the whole data frame,
#or you can click on the data frame in the Global Environment and the same thing will happen


plot(iris2019$Sepal.Width~iris2020$Sepal.Width)## We can plot columnes from two different data frames against each other
#plots are a good way to check you data and prepare 

range(iris2020$Sepal.Width)# This function give me the range of values in the vector (column)
iris2020$Sepal.Width<-iris2020$Sepal.Width/10 ## here i overwrite the original column with values
#that are an order of magnitude smaller
