setwd("D:/regis/course/BL654 modeling/week5")
dataall<- read.csv(file = "TBL4 Exercise Data.csv", header = T)

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
#Q2
#(a)
#(b)
model.full<- glm(PerEmbed~MANUII+Metropolitan.Area.Short.Code, 
                 na.action=na.exclude, data=dataall)
summary(model.full)
library(stargazer)
stargazer(model.full, ci=T, type = "text")
library(multcomp)
glht(model.full)

#(c)

#(d)
model.d1<-glm(PerEmbed~MANUII,na.action=na.exclude, data=dataall)
summary(model.d1)
model.d2<-glm(PerEmbed~MANUII+Metropolitan.Area.Short.Code,na.action=na.exclude, data=dataall)
summary(model.d2)
model.d3<-glm(PerEmbed~MANUII*Metropolitan.Area.Short.Code,na.action=na.exclude, data=dataall)
summary(model.d3)

#(e)
mean(dataall$PerEmbed,na.rm=TRUE)
sd(dataall$PerEmbed,na.rm=TRUE)
library(ggplot2)
ggplot(dataall,aes(x=MANUII,y=PerEmbed))+
  geom_point()+
  geom_smooth()+
  labs(title="complete pooling model", x="urbanization",y="embeddedness")
predict(model.d1,dataall,interval="confidence")

ATL<-dataall[dataall$Metropolitan.Area.Short.Code=="ATL",]
BIR<-dataall[dataall$Metropolitan.Area.Short.Code=="BIR",]
BOS<-dataall[dataall$Metropolitan.Area.Short.Code=="BOS",]
DEN<-dataall[dataall$Metropolitan.Area.Short.Code=="DEN",]
DFW<-dataall[dataall$Metropolitan.Area.Short.Code=="DFW",]
MGB<-dataall[dataall$Metropolitan.Area.Short.Code=="MGB",]
POR<-dataall[dataall$Metropolitan.Area.Short.Code=="POR",]
RAL<-dataall[dataall$Metropolitan.Area.Short.Code=="RAL",]
SLC<-dataall[dataall$Metropolitan.Area.Short.Code=="SLC",]
ATL.log<-log(mean(ATL$PerEmbed,na.rm=TRUE))
BIR.log<-log(mean(BIR$PerEmbed,na.rm=TRUE))
BOS.log<-log(mean(BOS$PerEmbed,na.rm=TRUE))
DEN.log<-log(mean(DEN$PerEmbed,na.rm=TRUE))
DFW.log<-log(mean(DFW$PerEmbed,na.rm=TRUE))
MGB.log<-log(mean(MGB$PerEmbed,na.rm=TRUE))
POR.log<-log(mean(POR$PerEmbed,na.rm=TRUE))
RAL.log<-log(mean(RAL$PerEmbed,na.rm=TRUE))
SLC.log<-log(mean(SLC$PerEmbed,na.rm=TRUE))
mean.log<-mean(ATL.log,BIR.log,BOS.log,DEN.log,DFW.log,MGB.log,POR.log,RAL.log,SLC.log)
sd.log<-sd(ATL.log,BIR.log,BOS.log,DEN.log,DFW.log,MGB.log,POR.log,RAL.log,SLC.log)
###(e) have little problem on confidence

#2
#(a)
#(b)
model.full2<- glm(EPT~MANUII+Metropolitan.Area.Short.Code, 
                 na.action=na.exclude, data=dataall)
summary(model.full2)
library(stargazer)
stargazer(model.full2, ci=T, type = "text")
library(multcomp)
glht(model.full2)

#(c)
#(d)
model2.d1<-glm(EPT~MANUII,na.action=na.exclude, data=dataall)
summary(model2.d1)
model2.d2<-glm(EPT~MANUII+Metropolitan.Area.Short.Code,na.action=na.exclude, data=dataall)
summary(model2.d2)
model2.d3<-glm(EPT~MANUII*Metropolitan.Area.Short.Code,na.action=na.exclude, data=dataall)
summary(model2.d3)

#(e)
mean(dataall$EPT,na.rm=TRUE)
sd(dataall$EPT,na.rm=TRUE)
library(ggplot2)
ggplot(dataall,aes(x=MANUII,y=EPT))+
  geom_point()+
  geom_smooth()+
  labs(title="complete pooling model", x="urbanization",y="EPT")
predict(model2.d1,dataall,interval="confidence")

ATL<-dataall[dataall$Metropolitan.Area.Short.Code=="ATL",]
BIR<-dataall[dataall$Metropolitan.Area.Short.Code=="BIR",]
BOS<-dataall[dataall$Metropolitan.Area.Short.Code=="BOS",]
DEN<-dataall[dataall$Metropolitan.Area.Short.Code=="DEN",]
DFW<-dataall[dataall$Metropolitan.Area.Short.Code=="DFW",]
MGB<-dataall[dataall$Metropolitan.Area.Short.Code=="MGB",]
POR<-dataall[dataall$Metropolitan.Area.Short.Code=="POR",]
RAL<-dataall[dataall$Metropolitan.Area.Short.Code=="RAL",]
SLC<-dataall[dataall$Metropolitan.Area.Short.Code=="SLC",]
ATL.log<-log(mean(ATL$EPT,na.rm=TRUE))
BIR.log<-log(mean(BIR$EPT,na.rm=TRUE))
BOS.log<-log(mean(BOS$EPT,na.rm=TRUE))
DEN.log<-log(mean(DEN$EPT,na.rm=TRUE))
DFW.log<-log(mean(DFW$EPT,na.rm=TRUE))
MGB.log<-log(mean(MGB$EPT,na.rm=TRUE))
POR.log<-log(mean(POR$EPT,na.rm=TRUE))
RAL.log<-log(mean(RAL$EPT,na.rm=TRUE))
SLC.log<-log(mean(SLC$EPT,na.rm=TRUE))
mean.log<-mean(ATL.log,BIR.log,BOS.log,DEN.log,DFW.log,MGB.log,POR.log,RAL.log,SLC.log)
sd.log<-sd(ATL.log,BIR.log,BOS.log,DEN.log,DFW.log,MGB.log,POR.log,RAL.log,SLC.log)
###(e) have little problem on confidence
