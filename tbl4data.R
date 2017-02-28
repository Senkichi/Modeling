setwd("~/Documents/Spring 2017/Modeling")
ept<-read.csv(file="TBL4 Exercise Data.csv",header=T,na.strings=c("NC","NM"))
library(nlme)
library(lme4)
#b
ept$fMetro<-factor(ept$Metropolitan.Area.Short.Code)
model1<-glm(PerEmbed~MANUII+fMetro+MANUII:fMetro,data=ept)
summary(model1)

#c
library(multcomp)
model.glht<-glht(model1)
#this is not correct the k values that need to be used are 1 and -1 just like the chick wts problem
confint(model.glht)
#ATL, BIR, BOS, MGB, POR, RAL

#b so denver would be fMetroDEN == 0        -11.953973 -32.823351   8.915406


#c
#most responsive is BOS with -55.573425 (95% CI -75.407778 -35.739073)
#least responsive is ATL with 0.079089  (95% CI -0.268532   0.426711)
