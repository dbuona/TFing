rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()


#Lots of libraries
library(vegan)
library(palmerpenguins)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(tidyr)
library(scatterplot3d)
install.packages("")
 #Some recources
##### PCA https://rpubs.com/friendly/penguin-biplots
#####also https://www.davidzeleny.net/anadat-r/doku.php/en:pca
#####NMDS https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/

set.seed(138)

plot(0:10,0:10,type="n",axes=FALSE,xlab="abundance",ylab="")
axis(1)
points(5,0);text(5.5,0.5, labels="A")
points(3,0); text(3.2,0.5,labels="B")
points(0,0); text(0.8,0.5,labels="C")

d<-scatterplot3d(0:10,0:10,0:10, type="n",xlab="Abundance sp 1",
                 ylab="abundance sp 2",zlab="abundance sp 3"); d
d$points3d(5,5,0); text(d$xyz.convert(5,5,0.5),labels="community A")
d$points3d(3,3,3); text(d$xyz.convert(3,3,3.5),labels="community B")
d$points3d(0,5,5); text(d$xyz.convert(0,5,5.5),labels="community C")

#####

community_matix<-matrix(sample(1:50,100,replace=TRUE),nrow=10,
                        dimnames = list(paste("community",(1:10),sep=""),paste("sp",1:10,sep="")))

pca.com<-rda(community_matix,scale=FALSE)
pca.com

screeplot(pca.com,type="line",main="Varience of PCA components")

sum((as.vector(pca.com$CA$eig)/sum(pca.com$CA$eig))[1:3])

biplot(pca.com,type=c("text"))


NMDS.com<-metaMDS(community_matix,k=2)

stressplot(NMDS.com)

ordiplot(NMDS.com,type="n")
orditorp(NMDS.com,display="species",col="red",air=0.01)
orditorp(NMDS.com,display="sites",cex=1,air=0.01)


treat<-c(rep("Treatment1",5),rep("treatment2",5))
colors<-c(rep("firebrick1",5),rep("skyblue1",5))

ordiplot(NMDS.com,type="n")
orditorp(NMDS.com,display = "species",col="black",air=0.01)
orditorp(NMDS.com,display="sites",col=c(rep("firebrick1",5),rep("skyblue1",5)),air=0.01, cex=1.25)
for(i in unique(treat)){
  ordihull(NMDS.com$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=FALSE)
}

ano.treats<-anosim(community_matix,grouping=treat,distance = "bray",permutations =9999)         

##PCA
peng<-na.omit(penguins)

peng.pca<-prcomp(~bill_length_mm+ bill_depth_mm+flipper_length_mm+body_mass_g,
                 data=peng,
                 na.action = na.omit,scale.=TRUE)
?prcomp()
summary(peng.pca)
screeplot(peng.pca,type="line")

autoplot(peng.pca,data=peng,color="species",
         loadings=TRUE,loadings.label=TRUE)
