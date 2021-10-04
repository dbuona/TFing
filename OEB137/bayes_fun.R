##Fun with bayesian
rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(brms)
library(lme4)
library(ggplot2)
library(dplyr)
library(mice)
library(ape)


data("nhanes", package = "mice")
head(nhanes)

prior <- c(set_prior("normal(0,100)", class = "b"),
           set_prior("normal(100,100)", class = "Intercept"),
           set_prior("normal(0,100)", class="sigma"))

mod1.prior<-brm(bmi~age*chl,data=nhanes,prior=prior,family = "gaussian", iter=3000,warmup=2500,sample_prior="only")

pp_check(mod1.prior)

mod1<-brm(bmi~age*chl,data=nhanes,prior=prior,family = "gaussian", iter=3000,warmup=2500)

pp_check(mod1,nsamples = 50)
summary(mod1)

plot(mod1, pars="^b")
stancode(mod1)

conditional_effects(mod1,"age:chl",probs=c(.25,.75))
?mice()

bform<- bf(bmi|mi()~age*mi(chl))+
             bf(chl| mi()~age)+set_rescor(FALSE)
fit_imp2<-brm(bform,data=nhanes)

conditional_effects(fit_imp2,"age:chl",resp= "bmi",probs = c(.25,.75))


##phylom lm
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)
head(data_simple)
plot(phylo)

model_simple<-brm(phen~cofactor, data=data_simple,family=gaussian(),chains=2)
summary(model_simple)

inv.phylo<-MCMCglmm::inverseA(phylo, nodes="TIPS",scale=TRUE)
A<-solve(inv.phylo$Ainv)
rownames(A)<-rownames(inv.phylo$Ainv)

model_simple_phylo<-brm(phen~cofactor+(1|phylo),
                        data=data_simple, family=gaussian(),cov_ranef=list(phylo=A))

fixef(model_simple_phylo)
fixef(model_simple)

hyp<- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 +sigma^2)=0"
(hyp<-hypothesis(model_simple_phylo,hyp, class=NULL))
