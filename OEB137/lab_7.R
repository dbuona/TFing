rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(palmerpenguins)
library(ggeffects)
library(dplyr)
library(ggplot2)
install.packages("")
########################################
### Question: How does flipper length ###
###covary by body mass or bill depth? ###
#######################################
head(penguins)
mod.add<-lm(penguins$flipper_length_mm~penguins$body_mass_g+penguins$bill_depth_mm)
options(scipen=999)
summary(mod.add)

###centering 
penguins$bm_cent<-penguins$body_mass_g-mean(penguins$body_mass_g,na.rm=TRUE)
penguins$bd_cent<-penguins$bill_depth_mm-mean(penguins$bill_depth_mm,na.rm=TRUE)
penguins$bl_cent<-penguins$bill_length_mm-mean(penguins$bill_length_mm,na.rm=TRUE)
###################################
mod.add.cent<-lm(penguins$flipper_length_mm~penguins$bm_cent+penguins$bd_cent)
summary(mod.add.cent)

penguins$bm_z<-penguins$bm_cent/sd(penguins$body_mass_g,na.rm=TRUE)
penguins$bd_z<-penguins$bd_cent/sd(penguins$bill_depth_mm,na.rm=TRUE)

mod.add.z<-lm(penguins$flipper_length_mm~penguins$bm_z+penguins$bd_z)
summary(mod.add.z)

### question 2 ####
### bill length and bill depth####### 
##realtionship with flipper length####
##################################
cor(penguins$bill_length_mm,penguins$bill_depth_mm,use="complete.obs")

mod.nointer.beak<-lm(flipper_length_mm~bd_cent+bl_cent,data=penguins)
summary(mod.nointer.beak)

mod.inter.beak<-lm(flipper_length_mm~bd_cent*bl_cent,data=penguins)

summary(mod.inter.beak)

quantile(penguins$bd_cent,na.rm=TRUE)

pen.pred<-ggpredict(model = mod.inter.beak,terms=c("bl_cent",
"bd_cent[-1.5511696 , 0.1488304 , 1.5488304]"),ci.lvl = 0.50)

ggplot(pen.pred,aes(x=x, y= predicted, color=group))+ stat_smooth(method="lm")
#################################
##########question 3#####################
## We are worried about fucundity in penguins##
##Is likelihood of nesting related to body size?##
##################################################
penguins$eggs<-ifelse(penguins$sex=="female" & penguins$bill_length_mm>= 46,1,0)

penguins.xx<-subset(penguins,sex=="female")

eggs.mod<-glm(eggs~body_mass_g,data=penguins.xx, family=binomial(link="logit"))
summary(eggs.mod)
exp(coef(eggs.mod))
range(penguins$body_mass_g,na.rm=TRUE)

eggs.pred<-ggeffects::ggpredict(eggs.mod,terms=c("body_mass_g[2700 ,3550, 4050, 4750, 6300]"), ci.lvl=0.50)

ggplot(eggs.pred,aes(x=x,y=predicted))+geom_point()+stat_smooth(method="glm")

##############################
### nonlinear model ##########
#https://rpubs.com/mengxu/exponential-model
##how does penguin density in hunting ground##

#typical exponetial decay: y = alpha * exp(beta * x) + theta + error
alpha <- 30
beta <- -0.08
theta <- 4

# Sample some points along x axis
n <- 50
x <- seq(n)
###############################
###question 4 ########
## x penguin density ###############
##y is fish density ##############
##how does penguin densityt impact fish density###########
###################################################
#weill be simulating a typical exponetial decay pattern with 3 parameters:
#y = alpha * exp(beta * x) + theta + error
#setvalues of parements
alpha <- 30
beta <- -0.08
theta <- 4

# Sample some points along x axis
n <- 50 ## sample size
x <- seq(1,n, by=5) # sensity of peguins

# Make  y = f(x) + error
df <- data.frame(x = x,
                 y = alpha * exp(beta * x) + theta + rnorm(n))



# run a nolinear model in nls
# to do so you need to specify start paramenters which can be tricky
## you in nls you need to specify start parameters
# 1) Select an approximate $\theta$,
#since theta must be lower than min(y), and greater than zero
theta.0 <- min(df$y) * 0.5  

# Estimate the rest of the start parameters using a the log() response linear model
model.0 <- lm(log(y - theta.0) ~ x, data=df)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters

start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
##heres the model
model.nl <- nls(y ~ alpha * exp(beta * x) + theta , data = df, start = start)

## its hard to make curved lines in r but you can use predict() to generate
# a single point estimate (x,y) for each x value and then fit a line to them

df$predy<-predict(model.nl)


##plot it
ggplot(df,aes())+geom_point(aes(x,y))+geom_line(aes(x,predy))
## for above, not we specify x and for each geom_object not in the first ggplot() arguement
