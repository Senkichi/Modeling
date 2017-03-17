<<<<<<< Updated upstream
setwd("~/Documents/Spring 2017/Modeling") 
ept<-read.csv(file="TBL4 Exercise Data.csv",header=T,na.strings=c("NC","NM"))
library(nlme)
library(lme4)
plot(PerEmbed~MANUII,data=ept,col=fMetro)

#2b
ept$fMetro<-factor(ept$Metropolitan.Area.Short.Code)
model1<-lm(PerEmbed~MANUII+fMetro+MANUII:fMetro,data=ept)
summary(model1)

#2b-a
#the p value is < 2.2e-16 and means that the effect of urbanization on percent embeddedness 
  #significantly varies by city

#2b-b
=======
setwd("~/Documents/Spring 2017/Modeling")
EPT<-read.csv(file="TBL4 Exercise Data.csv",header=T,na.strings=c("NC","NM"))
library(nlme)
library(lme4)
EPT
#b


ept$fMetro<-factor(ept$Metropolitan.Area.Short.Code)
model1<-glm(PerEmbed~MANUII+Metropolitan.Area.Short.Code+MANUII:Metropolitan.Area.Short.Code,data=EPT)
summary(model1)


#c
>>>>>>> Stashed changes
library(multcomp)
K1<-c(1,0,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0) #this vector of the coefficients

#always have to create a matrix even if its one row
K.all<-rbind(K1)

#linfct are the coefficients of your linear function of interest
model.glht<-glht(model1,linfct=K.all)
confint(model.glht) #94.85% in Denver streams that have no urbanization (95% CI 72.6785 to 117.0306)
summary(model.glht)
# this assess the p value and tells you there is a significant difference p<alpha

#2b-c
#least responsive is BOS with -55.573425 (95% CI -75.407778 -35.739073)
#most responsive is ATL with 0.079089  (95% CI -0.268532   0.426711)

K2<-c(1,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #this vector of the coefficients

#always have to create a matrix even if its one row
K.all2<-rbind(K2)

#linfct are the coefficients of your linear function of interest
model.glht2<-glht(model1,linfct=K.all2)
confint(model.glht2) #138.47 (95% CI 116.7549 to 160.1932)
summary(model.glht2)
# this assess the p value and tells you there is a significant difference p<alpha

#2c


#2d
pooled<-lm(PerEmbed~MANUII+EPT+MANUII:EPT, data=ept)
summary(pooled)
mixed<-lmer(PerEmbed~1+MANUII+(1|fMetro),data=ept)
summary(mixed)
mixed2<-lmer(PerEmbed~1+MANUII+(1+MANUII|fMetro),data=ept)
summary(mixed2)



