##functions     We talked a bit about vectors, object, and in doing so we encountered functions.
#today we are going to break down what a function is in the context of distrubtions and practice writing out own
# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
iris

mean(iris$Petal.Length) #na.rm=TRUE or FALSE (logical statements) see that = sign is withing the function
sd(iris$Petal.Length)
var(iris$Petal.Length)
se(iris$Petal.Length)#rror 

sqrt(4)
sqrt(iris$Petal.Length) ## note this applies the function over the whole vector

sqrt(var(iris$Petal.Length)/length(iris$Petal.Length)) ### this is the equation to calculate standard error
sqrt(var(iris$Petal.Length))/length(iris$Petal.Length) #just like in real math parenthesize matter
sqrt(var(iris$Petal.Length))/length(iris$Petal.Length)) #but here they also matter to make you code run

std.err <- function(x) { #but we can make a function of it
  sqrt(var(x)/length(x))
}

std.err(iris$Petal.Length) ##see?

mult.sd <- function(x,y) { ##functions can have multiple areguements
  sd(x)*y
}
mult.sd(iris$Petal.Length,1)#68% of data
mult.sd(iris$Petal.Length,2) #95% of data

goo<-rnorm(1000,100,20)

mean(goo)
sd(goo)

?rnorm()
rpois()
rbinom()
rlnorm()


sd()


response<-rnorm(5,20,2)
year<-rep(2019,5)
mydata<-data.frame(response,year)

response<-rnorm(5,40,5) ##note: this over writes response but not my data
year<-rep(2020,5)

mydata2<-data.frame(response,year)

fulldata<-rbind(mydata,mydata2)
?rbind()
#(show cbind)
plot(response~as.factor(year),data=fulldata)

