rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(brms)
library(lme4)
library(ggplot2)
library(dplyr)
library(mice)
library(ape)


#### Tool 1 imputations
data("nhanes", package = "mice")
head(nhanes)

ggplot(nhanes,aes(bmi,chl))+
  geom_point()+
  stat_smooth(method="lm")

prior <- c(set_prior("normal(0,100)", class = "b"),
           set_prior("normal(100,100)", class = "Intercept"),
           set_prior("normal(0,100)", class="sigma"))

mod1.prior<-brm(bmi~age*chl,data=nhanes,prior=prior,family = "gaussian", iter=3000,warmup=2500,sample_prior="only")
pp_check
mod1<-brm(bmi~age*chl,data=nhanes,prior=prior,family = "gaussian", iter=3000,warmup=2500)
stancode(mod1)




summary(mod1)
##diagnosticss
pp_check(mod1,nsamples = 100)
plot(mod1, pars = "^b")
launch_shinystan(mod1)

a<-conditional_effects(mod1,"age:chl",probs = c(.25,.75))c
bform <- bf(bmi | mi() ~ age * mi(chl)) +
  bf(chl | mi() ~ age) + set_rescor(FALSE)
fit_imp2 <- brm(bform, data = nhanes)


fixef(fit_imp2)
b<-conditional_effects(fit_imp2,"age:chl",resp="bmi",probs = c(.25,.75))
launch_shinystan(fit_imp2)


### phylogenetic mixed model compare to phylolm
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)
head(data_simple)

data

inv.phylo <- MCMCglmm::inverseA(phylo, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)


model_simple.phylo <- brm(
  phen ~ cofactor + (1|phylo), 
  data = data_simple, 
  family = gaussian(), 
  cov_ranef = list(phylo= A))

fixef(model_simple)
fixef(model_simple.phylo)


hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(model_simple.phylo, hyp, class = NULL))


plot(hyp)
summary(fit2) 
plot(fit2)  


##survival analysis AFT
head(kidney)

fit.surv <- brm(time | cens(censored) ~ age + sex + disease, 
                data = kidney, family = weibull(), inits = "0")

new.data.3<- data.frame(age=c(20,60,20,60), sex=c("male","male","female","female"),disease= rep("GN",4))

fity<-fitted(fit.surv,newdata=new.data.3)
fity<-cbind(fity,new.data.3)


