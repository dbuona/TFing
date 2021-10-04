rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

library(ggplot2)
library(car)
library(agricolae)

setwd("Documents/git/TFing/OEB137/")

pengy<-read.csv("Dans_penguins.csv")

set.seed(147)

##Assumptions of Anova
#1 normally distrubuted
#2 indepedent
#3 roughly homogenious varience

#4 assigned properly
#5 additive

head(pengy)
bartlett.test(body~species,data=pengy)
leveneTest(body~species,pengy)

anov.simple<-aov(pengy$body~pengy$species)
anov.simple<-with(pengy,aov(body~species))
anov.simple<-aov(body~species,data=pengy)

summary(anov.simple)

plot(anov.simple)

TukeyHSD(anov.simple)

ggplot(pengy,aes(species,body))+
  geom_jitter(height=0,width=0.1,color="skyblue1")+
  geom_boxplot(alpha=0.6)

HSD.test(anov.simple,"species",group=TRUE,console=TRUE)  

ggplot(pengy,aes(species,body))+
  geom_jitter(height=0,width=0.1,color="skyblue1")+
  geom_boxplot(alpha=0.6)+
  annotate("text",x=c(1,2,3),y=c(55,30,55),label=c("a","b","a"))

#2way anova
head(pengy)

leveneTest(body~species*island,data=pengy)
multi.anov<-aov(body~species*island,data=pengy)
summary(multi.anov)
TukeyHSD(multi.anov)
ggplot(pengy,aes(species,body))+
  geom_jitter(height=0,width=0.1,aes(color=island))+
  geom_boxplot(alpha=0.6,aes(fill=island))

int<-with(pengy,interaction(species,island))
inty<-aov(pengy$body~int)

HSD.test(inty,"int",group=TRUE,console=TRUE)

ggplot(pengy,aes(species,body))+
  geom_jitter(height = 0,width=0.1,aes(color=island))+
  geom_boxplot(alpha=0.6,aes(fill=interaction(species,island)))+
  annotate("text", x=c(.75,1.25,1.75,2.25,2.75,3.25), y=c(52,52,25,25,52,52), label=c("a","a","b","c","a","a"),size=6, colour="black")


###ANCOVA
head(pengy)
ancov<-aov(pengy$body~pengy$diet+pengy$species)
ancov2<-aov(pengy$body~pengy$species+pengy$diet)
summary(ancov)
summary(ancov2)

table(penguins$species)
table(penguins$island)

plot(TukeyHSD(ancov,which="pengy$species"))


library(palmerpenguins)
twoway<-aov(body_mass_g~species+island,data=penguins)
summary(twoway)

twowayb<-aov(body_mass_g~island+species,data=penguins)
summary(twowayb)

Anova(twoway,type="III")
Anova(twowayb,type="III")

ggplot(penguins,aes(species,body_mass_g))+geom_boxplot(aes(fill=island))

Anova(lm(body_mass_g~species+island,data=penguins),tpye="III")

##MANOVA
manova.one<-manova(cbind(bill_length_mm,flipper_length_mm)~species,data=penguins)
summary(manova.one)
