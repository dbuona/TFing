rm(list=ls()) #clears global env
options(stringsAsFactors = FALSE)
graphics.off()

### Bayesian analysis: with much code and commentary courtesy of Max Farrell 

#librariesd
library(gdata)
library(palmerpenguins)
library(rstan)
library(shinystan)
library(brms)
library(loo)
library(lme4)
library(tidybayes)
setwd("Documents/git/TFing/OEB137")

load("bayesclass.Rda")
#############################
###1) write model       ####
####   see stan code   ###
###########################

# We're going to start by writing a linear model in the 
# language Stan. This can be written in your R script, 
# or saved seprately as a .stan file and called into R.

# A stan program has three required "blocks":

# 1. "data" block: where you declare the data types, 
# their dimensions, any restrictions 
# (i.e. upper= or lower- , which act as checks for Stan),
# and their names.

# Any names will be what you give to your Stan 
# program and also be the names used in other blocks.

# 2. "parameters" block: This is where you indicate the 
# parameters you want to model, their dimensions, restricitons, and name. 
# For a linear regression, we will want to model the intercept, 
# any slopes, and the standard deviation of the errors 
# around the regression line.

# 3. "model" block: This is where you include any sampling statements,
# including the "likelihood" (model) you are using. 
# The model block is where you indicate any prior distributions 
# you want to include for your parameters. If no prior is defined, 
# Stan uses default priors of uniform(-infinity, +infinity). 
# You can restrict priors using restrictions when 
# declaring the parameters (i.e. <lower=0> to make sure a parameter is + )

# Sampling is indicated by the "~" symbol, and Stan already includes
# many common distributions as vectorized functions. 
# See the manual for a comprehensive list.

# There are also optional blocks:
# functions {}
# transformed data {}
# transformed parameters {}
# generated quantitied {}

# Comments are indicated by "//" and are ignored by Stan.




###############################################################
###### 2) generate fake data and test model on fake data ######
################################################################
set.seed(137)

N <- 1000
alpha <- 2.5
beta <- 0.2
sigma <- 6
x <- rnorm(N, 100, 10)
y <- rnorm(N, alpha + beta*x, sigma)

mydat<-data.frame(x=x,y=y)


# Have a look
plot(y~x, pch=20)

# Plot the "true" relationship
abline(alpha, beta, col=4, lty=2, lw=2)

#run as a regular lm
lm1 <- lm(y ~ x)
summary(lm1) 

abline(lm1, col=2, lty=2, lw=3)

### prepare data for stan
datalist.simpy<- with(mydat, 
                    list(y=y, 
                         x=x,
                         N = nrow(mydat)))

m_simplestan<- stan('stan_simplelm.stan', data = datalist.simpy,
              iter = 2000, warmup=500)

##if i didnt provide priors, stan defults to uniform(-Inf,Inf)
posterior <- extract(m_simplestan)

# extract() pulls puts the posteriors for each parameter into a list

# Let's compare to our previous estimate with "lm" and true
abline( mean(posterior$alpha), mean(posterior$beta), col=6, lw=2)

# One way to visualize the variability in our estimation of the regression line
# is to plot multiple estimates from the posterior:

# Plotting the posterior distribution of regression lines
for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col="gray", lty=1)
}
# Let's plot the "true" regression line on top:
abline(alpha, beta, col=4, lty=2, lw=3)

data("penguins")
penguins<- penguins[complete.cases(penguins),]

###############################################
############### 3) Run model on real data #####
###############################################  

datalist.peng<- with(penguins, 
                      list(y=bill_length_mm, 
                           x=flipper_length_mm,
                           N = nrow(penguins)))

m_peng= stan('stan_simplelm.stan', data = datalist.peng,
                   iter = 2000, warmup=500)

m_peng_bad= stan('stan_simplelm.stan', data = datalist.peng,
             iter = 500, warmup=20)


y<-penguins$bill_length_mm

### check diagnostics with shinystan
launch_shinystan(m_peng_bad)
launch_shinystan(m_peng)
m_peng_bad
######################################################
### 3a quick trip to BRMS for model comparision and priors
## brms is a package that links stan code to R code.
######################################################
get_prior(flipper_length_mm~bill_length_mm,data=penguins) ### this tells us the default brms provies

##### here are my reasonable attempts at weakly informative priors########
default<-c(prior(normal(0,16), class = b, coef = bill_length_mm),
  prior(normal(0,16), class = sigma), 
  prior(normal(197,16),class = Intercept))

##### straght up bad priors
priorz <- c(prior(normal(1,2), class = b, coef = bill_length_mm),
            prior(cauchy(0,2), class = sigma), 
                   prior(normal(-10,3),class = Intercept))


### prior predictively check, predict y values based just on priors 
modpeng1a.prior<-brm(flipper_length_mm~bill_length_mm,data=penguins,prior=priorz,sample_prior = "only")
modpeng1.prior<-brm(flipper_length_mm~bill_length_mm,data=penguins,prior=default,sample_prior = "only")

pp_check(modpeng1a.prior,nsamples = 100)
pp_check(modpeng1.prior,nsamples = 100)


######## now lets do some model comparision using leave-one-out cross validation
modpeng1<-brm(flipper_length_mm~bill_length_mm,data=penguins)
modpeng2<-brm(flipper_length_mm~bill_length_mm+sex,data=penguins)
modpeng3<-brm(flipper_length_mm~bill_length_mm+sex+(1|species),data=penguins)



pp_check(modpeng1)
pp_check(modpeng2)
pp_check(modpeng3)

?loo()

fit1 <- add_criterion(modpeng1, "loo")
fit2 <- add_criterion(modpeng2, "loo")
fit3 <- add_criterion(modpeng3, "loo")


brms::loo_compare(fit1,fit2,fit3,criterion="loo")
summary(fit3)
########################################
#### 4) Evalulate models ####################
#### Model diagonostics################
#### Rhat: ratio of the average variance of draws within each chain
#to the variance of the pooled draws across chains: close to 1 signals convergence
#### ESS 
### divergent transitions



posterior <- extract(m_peng)
plot(datalist.peng$y~datalist.peng$x, pch=20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col="gray", lty=1)
}

lm2 <- lm(y ~ x,data=datalist.peng)

abline(lm2, col=2, lty=2, lw=3)



#example where you are not getting the same thing as lm
#budburst
#rm(list=ls()) #clears global env
#options(stringsAsFactors = FALSE)
#graphics.off()

bb.stan<-read.csv("bbstan_mainmodel.csv")
nrow(bb.stan)

datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )           )

MaXL.bb<-lmer(resp~chill.z+force.z+photo.z+(1+chill.z+force.z+photo.z|complex),data=bb.stan,REML=TRUE) 

m2l.ni = stan('nointer_2level.stan', data = datalist.bb,
              iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

launch_shinystan(m2l.ni)




#save.image("bayesclass.Rda")
