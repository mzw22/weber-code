#setup
library(PerformanceAnalytics) # easy correlation plot
library(tidyverse)      
library(tidyr)          
library(dplyr)          
library(devtools)
library(MASS)
library(ggplot2)
#NB need to install each of these packages first (e.g. using code like install.packages("tidyr") etc)

#set working directory and import data
setwd("~/Homework/webers_law/data/dixit_etal_2022") 
PSexp_comp<-read.csv("Experiments_complexity.csv")

#use expression |a-b|/r - if r is close to zero, then it's a difference model (i.e. absolute difference)
#if r is close to 1 it's a ratio model (i.e. Weber).

#columns
#absdiff = |a-b| = exp complexity ac - host complexity ac
#hostcomp = b = host complexity ac
#minustwo = |a-b|/b^(-2) = hostcomp^(-2) (etc.)

PSexp_comp$absdiff<-abs(PSexp_comp$Exp_COMPLEXITY_ac-PSexp_comp$HostAv_COMPLEXITY_reduced_ac)
PSexp_comp$hostcomp<-PSexp_comp$HostAv_COMPLEXITY_reduced_ac

#each model is |a-b|/r with r from -2 to 2
PSexp_comp$minustwo<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-2))
modminustwo<-glm(Egg_rejected ~ minustwo, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminustwo)
#AIC=107.04

PSexp_comp$minusone<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-1))
print(PSexp_comp$minusone)
modminusone<-glm(Egg_rejected ~ minusone, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminusone)
AICk <- extractAIC(modminusone)
print (AICk)
#AIC=105.77

PSexp_comp$minus0.75<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-0.75))
modminus0.75<-glm(Egg_rejected ~ minus0.75, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminus0.75)
#AIC=105.19

PSexp_comp$minuszeroptfive<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-0.5))
modminuszeroptfive<-glm(Egg_rejected ~ minuszeroptfive, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminuszeroptfive)
#AIC=104.45

PSexp_comp$minus0.25<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-0.25))
modminus0.25<-glm(Egg_rejected ~ minus0.25, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminus0.25)
#AIC=103.53

PSexp_comp$zero<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(0))
modzero<-glm(Egg_rejected ~ zero, data=PSexp_comp, family = binomial(link = "logit"))
summary(modzero)
#AIC=102.41

PSexp_comp$zero.25<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(0.25))
modzero.25<-glm(Egg_rejected ~ zero.25, data=PSexp_comp, family = binomial(link = "logit"))
summary(modzero.25)
#AIC=101.17

PSexp_comp$zeroptfive<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(0.5))
modzeroptfive<-glm(Egg_rejected ~ zeroptfive, data=PSexp_comp, family = binomial(link = "logit"))
summary(modzeroptfive)
#AIC=99.941

PSexp_comp$zero.75<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(0.75))
modzero.75<-glm(Egg_rejected ~ zero.75, data=PSexp_comp, family = binomial(link = "logit"))
summary(modzero.75)
#AIC=98.962

PSexp_comp$one<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(1))
modone<-glm(Egg_rejected ~ one, data=PSexp_comp, family = binomial(link = "logit"))
summary(modone)
#AIC=98.443

PSexp_comp$one.25<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(1.25))
modone.25<-glm(Egg_rejected ~ one.25, data=PSexp_comp, family = binomial(link = "logit"))
summary(modone.25)
#AIC=98.479

PSexp_comp$oneptfive<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(1.5))
modoneptfive<-glm(Egg_rejected ~ oneptfive, data=PSexp_comp, family = binomial(link = "logit"))
summary(modoneptfive)
#AIC=99

PSexp_comp$one.75<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(1.75))
modone.75<-glm(Egg_rejected ~ one.75, data=PSexp_comp, family = binomial(link = "logit"))
summary(modone.75)
#AIC=99.835

PSexp_comp$two<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(2))
modtwo<-glm(Egg_rejected ~ two, data=PSexp_comp, family = binomial(link = "logit"))
summary(modtwo)
#AIC=100.81

#create data frame with each value of r, its AIC, and various powers of r for the polynomial fit
r<-c(-1,-0.75, -0.5,-0.25, 0,0.25, 0.5, 0.75,1,1.25, 1.5,1.75, 2)
modAIC<-c(105.77,105.19,104.45,103.53,102.41,101.17,99.941,98.962,98.443,98.479,99,99.835,100.81) #the AICs for the models with different values of r
rsquare<-r^2
rcube<-r^3
rfour<-r^4
rfive<-r^5
rsix<-r^6

#plot values of AICs against their corresponding values of r
plot(r,modAIC)

#dataframe of values of r, AIC, and the powers of r for the polynomial
dat<-data.frame(r,modAIC,rsquare,rcube,rfour,rfive)

#now fit models to the data quintic, cubic, quartic
quintmodelfit<-lm(modAIC~r+rsquare+rcube+rfour+rfive,data=dat)
summary(quintmodelfit)
stepAIC(quintmodelfit)
#Megan - let me know if you're confused about how stepAIC works - basically it sees whether eliminating variables
#improves the AIC. Here we find that eliminating variables did not improve (i.e. reduce) AIC
#whereas with cubic below, we see that eliminating the r^2 term does improve AIC

cubemodelfit<-lm(modAIC~r+rsquare+rcube,data=dat)
summary(cubemodelfit)
stepAIC(cubemodelfit)

quartmodelfit<-lm(modAIC~r+rsquare+rcube+rfour,data=dat)
summary(quartmodelfit)
stepAIC(quartmodelfit)

#ggplot with best model, here, quintic (based on the initial stepwise elimination)
ggplot(dat,aes(x=r,y=modAIC))+
  geom_point(data=dat)+
  geom_function(fun=function(x) 102.39999+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5)))+
  labs(y="Model AIC",x="r")

#now to find minimum point
fun<-function(x) 102.39999+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5))
f=expression(102.39999+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5)))
derivative<-D(f,"x")
derivative
#now insert output of derivative as function d - do this manually
d<-function(x) 1.61021 * (3 * x^2) - (0.94387 * (2 * x) + 4.9094) + 0.66213 * (4 * x^3) - 0.35879 * (5 * x^4)
x<-seq(-2,2,0.01)
plot(x,d(x),type="l")
x<-seq(0,2,0.001)
y<-sign(d(x))
minus.to.plus <- which(diff(y)>0) #gives index of the first place that y is positive
#here get 1128 - i.e. x value of 1.128
fun(1.128) #get y value - 98.38902

#Alternatively, simply do 
library(rootSolve)
uniroot.all(d,c(0,2))
#get x=1.127478
fun(1.127478) #get same y value 98.38902

#can create function with y value of 2 above the minimum (approx 95% confidence intervals)
#just subtract 2 from the intercept of fun and subtract the y value of the root
g<-function(x) 100.39999-98.38902+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5))
uniroot.all(g,c(0,2))
#get 0.4026419 and 1.8827176 as 95CIs

#plot AIC against r again, and add the minimum of the graph. 
ggplot(dat,aes(x=r,y=modAIC))+
  geom_point(data=dat,size=2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=13),
        axis.title=element_text(size=14))+
  geom_function(fun=function(x) 102.39999+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5)))+
  labs(y="AIC of model",x="k")+
  geom_line(y=100.39999)+ #this should be y= the intercept of the model plus 2
  geom_point(aes(1.127,98.38902),col="red", size=4)

#final model (my addition)
PSexp_comp$ksol <- PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(1.127478))
mod_sol <- glm(Egg_rejected ~ ksol, data=PSexp_comp, family = binomial(link = "logit"))

#plot graphs to check assumptions
par(mfrow=c(2,2)) #make grid for plots
plot(mod_sol)
