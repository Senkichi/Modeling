RIKZ<-read.table(file="RIKZ.txt",header=T,dec=".") #read RIKZ data in
head(RIKZ)
RIKZ.model1<- lm(Richness~Exposure+NAP+Exposure:NAP, data=RIKZ)
summary(RIKZ.model1) #assumes each site is independent from each other but they 
#aren't because sites are clustered per beach, this model is incorrect.
#the beach levels are independent but sites are not.

#2 stage analysis Model - see chapter from slide show ch 5, pp 103-104...
#preference, use lmer4 model - linear mixed effects in R
#adding g is glmer - generalize " " - so if we have a response that is not NORMAL, 
#we would use glmer (binomial, etc.).
#can't use Poissons and binomial in lmer but we can do the same thing in glmer.
  #just change the family type in glmer.
#glmer can do all, just set family for normal to Guassian. 

#fit 2 models:
#1:
#vary random effect on intercept by beach but not slope.
#1 fixed effect for NAP and that doesn't vary by beach, it's stays fixed
#we also have a fixed effect for intercept and we will see how it varies by beach.
#will graph as parallel lines with same slope and different intercepts.
library(nlme)
library(lme4)
RIKZ$fBeach<-factor(RIKZ$Beach) #important to factor the beach numbers
#fit richness as a function of Nap (understood that we have an intercept)
#comma, random effect on 1(the intercept) of fBeach. fitting random effect of Beach
#on intercept.
M1.lme<-lme(Richness~NAP, random = ~1 | fBeach, data= RIKZ)
#in lmer, you don't have to add the random effect~1, see diff below.
M1.lmer<-lmer(Richness~1 + NAP + (1|fBeach), data=RIKZ)
summary(M1.lme)#2 fixed effects, the overall intercept across beaches (Intercept)and 
#the fixed effect of the slope (NAP) and we get fewer species as we go higher.
#StdDev of intercept is 2.944 (how much the intercepts vary by beaches) and 3.05977
#is the residual (std deviation of sites) (sigma) - how much the sites 
#vary from each other after accounting
#for the effect of NAP and the random effect of beach.
#so do sites or do beaches differ more? sites, but not by much (2.9 is less than 3.05)
#sites = 3.059 and beach is 2.94 for std deviation (how far from avg intercept) compared
#to 6.581893
#nothing about how the beaches differ from each other in the model above.

#now looking at lmer model...
summary(M1.lmer) 
#same std. dev 2.944 and 3.060 by beach and site respectively.
#p-value is missing because it's difficult to say given the correlation structure
#and the fact that we are compromising between pooling and no pooling
#the d.of.freedoms are hard to calculate and narrow down, so leave it out.
#you can use the std error numbers to do plus/minus 2 std err's as a CI....
#this is the model for paralell lines of beaches with same slope and diff intercepts.
#effect of NAP is fixed

#now change to model 2:
#allow intercepts and slopes to vary by beach
#random effect of beach on slope.
##just added second part of equation by adding beta 2*NAP
M2.lme<-lme(Richness~NAP, random = ~1 + NAP | fBeach, data=RIKZ)
#this tranlates directly down here, like the previous model from lme to lmer
M2.lmer<-lmer(Richness ~1 + NAP + (1+NAP|fBeach), data=RIKZ)
summary(M2.lme)
#differences now is the random effect for slope of NAP by Beach
#1.714 is how much the slope varies (std. dev) by beach.
#we still have 2 fixed effects - 6.5 is now the overal intercept from complete
#pooling and -2.8 is the overall slope, but we know slope can vary from beach
#to beach, so even though it's negative 2.83, that the std dev can be as high as 
#1.71 across the beaches, so between 2 std. dev's above and below is 3.4
#so some beaches can show a slightly positive response but most will show negative.
#3.54 is how much variation in the intercept and notice that 2.7 is the residual
#and that the residual variation here from site to site is smaller than before
#because we've accounted for that extra variation in the slope.
#think of fixed as global (pooled), when NAP is 0, what is the species richness
#globally, and random as beach to beach variation.
summary(M2.lmer)
#no slope given in this model again.

#to compare a model with no random effects to one with random effects, use
#gls()
library(nlme)
model1<-gls(Richness~1+NAP, data=RIKZ) # no random effects
model2<-lme(Richness~1+NAP, random= ~1|fBeach, data=RIKZ)#with random effects
#model 1 i just like a regular lm
#to compare in anova:
anova(model1,model2)
#so, we have 3 df in model 1, slope, intercept and sigma squared (Residual)
#in model 2 we have 4 df (+random effect)
#it's telling you that the AIC is lower in model2, so we should use random effects
#due to the p value being low too.
# to see if we should add a random effect in slope, just add that to the random
#effects model in number 2.
#don't recommend step-wise due to random effects present, it doesn't know how to 
#choose or exclude.

#TBL - 2 responses requested,
#1-how does urbanization affect the particle size and does it vary by city
#2-log partical size...use glmer to do the same type of thing that is either a
#count or proportion (binomial(proportion) or Poisson(count))
#just change family type in glmer to appropriate data type.
