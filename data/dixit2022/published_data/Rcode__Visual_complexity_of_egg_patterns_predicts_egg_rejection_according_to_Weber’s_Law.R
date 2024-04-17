#### Stats complexity ####
#setup
library(PerformanceAnalytics) # easy correlation plot
library(tidyverse)      
library(tidyr)          
library(dplyr)          
library(devtools)
library(MASS)
setwd("C:/Users/tdixi/Documents/Complexity_project")
PSalleggs_comp<-read.csv("AllEggs_complexity.csv",header=T,sep=",")
PSalleggs_comp<-PSalleggs_comp%>%mutate(Year_Nest = paste(Year, Nest, sep = "_"))
PSexp_comp<-read.csv("Experiments_complexity.csv",header=T,sep=",")

#subset by nest
set.seed(4)
PSalleggssub_comp<-PSalleggs_comp%>%group_by(Year_Nest)%>%sample_n(1)
#Chart correlation of complexity on different sides
chart.Correlation(PSalleggssub_comp[c("COMPLEXITY_a","COMPLEXITY_b","COMPLEXITY_c","COMPLEXITY_d",
                                      "COMPLEXITY_ac","COMPLEXITY_bd")],method="spearman")
chart.Correlation(PSalleggssub_comp[c("COMPLEXITY_a","COMPLEXITY_b","COMPLEXITY_c","COMPLEXITY_d",
                                      "COMPLEXITY_ac","COMPLEXITY_bd")],method="pearson")
#all highly correlated 

#double check with icc
library(irr)
icc(PSalleggssub_comp[c("COMPLEXITY_a","COMPLEXITY_b","COMPLEXITY_c","COMPLEXITY_d","COMPLEXITY_ac",
                        "COMPLEXITY_bd")],type="agreement")
#highly repeatable
#n=178,ICC=0.868,95CI=0.839-0.893,F(177,890) = 40.3 , p = 0.0 
icc(PSalleggssub_comp[c("COMPLEXITY_a","COMPLEXITY_b","COMPLEXITY_c","COMPLEXITY_d")],type="agreement")
#n=178,ICC=0.809,95CI=0.767-0.846,F(177,534) = 17.9, p = 1.26e-146 

#check ICC with intra-clutch variation
PSalleggssub_comp2<-subset(PSalleggs_comp,Nest==NestIn)
PSalleggssub_comp2<-subset(PSalleggssub_comp2,duplicated(Year_Nest) | duplicated(Year_Nest, fromLast=TRUE))
PSalleggssub_comp22<-subset(PSalleggssub_comp2,Egg=="P1" | Egg=="P2")
PSalleggssub_comp222<-PSalleggssub_comp22%>%dplyr::select(c(Year_Nest,Egg,COMPLEXITY_a))
PSalleggssub_comp2222 <- spread(PSalleggssub_comp222,Year_Nest,COMPLEXITY_a)
PSalleggssub_comp2222<-as.data.frame(t(PSalleggssub_comp2222))
PSalleggssub_comp2222<-PSalleggssub_comp2222[-1,]
colnames(PSalleggssub_comp2222) <- c("X","Y")
PSalleggssub_comp2222$X<-as.numeric(PSalleggssub_comp2222$X)
PSalleggssub_comp2222$Y<-as.numeric(PSalleggssub_comp2222$Y)
icc(PSalleggssub_comp2222[c("X","Y")],type="agreement")



#with experiment data, test whether reduced correlates with full clutches
chart.Correlation(PSexp_comp[c("HostAv_COMPLEXITY_all_a","HostAv_COMPLEXITY_all_b","HostAv_COMPLEXITY_all_c",
                               "HostAv_COMPLEXITY_all_d","HostAv_COMPLEXITY_all_ac","HostAv_COMPLEXITY_all_bd",
                               "HostAv_COMPLEXITY_reduced_a","HostAv_COMPLEXITY_reduced_b","HostAv_COMPLEXITY_reduced_c",
                               "HostAv_COMPLEXITY_reduced_d","HostAv_COMPLEXITY_reduced_ac","HostAv_COMPLEXITY_reduced_bd")]
                  ,method="spearman")
chart.Correlation(PSexp_comp[c("HostAv_COMPLEXITY_all_a","HostAv_COMPLEXITY_all_b","HostAv_COMPLEXITY_all_c",
                               "HostAv_COMPLEXITY_all_d","HostAv_COMPLEXITY_all_ac","HostAv_COMPLEXITY_all_bd",
                               "HostAv_COMPLEXITY_reduced_a","HostAv_COMPLEXITY_reduced_b","HostAv_COMPLEXITY_reduced_c",
                               "HostAv_COMPLEXITY_reduced_d","HostAv_COMPLEXITY_reduced_ac","HostAv_COMPLEXITY_reduced_bd")]
                  ,method="pearson")
#again, everything highly correlated!

#----------------------------------------------------------------------------------------------------#

#### Rejection data - testing if and how complexity predicts rejection ####
setwd("C:/Users/tdixi/Documents/Complexity_project") 
PSexp_comp<-read.csv("Experiments_complexity.csv")
# test whether rejection predicted by complexity 
PSexp_comp$comp<-scale(abs(PSexp_comp$Exp_COMPLEXITY_ac-PSexp_comp$HostAv_COMPLEXITY_reduced_ac))
PSexp_comp$unscaled_comp<-abs(PSexp_comp$Exp_COMPLEXITY_ac-PSexp_comp$HostAv_COMPLEXITY_reduced_ac)
#omit rows with NAs
PSexp_comp<-PSexp_comp[complete.cases(PSexp_comp[,"HostAv_COMPLEXITY_reduced_ac"]),]
#first show that FemaleID doesnt matter
#t11<-glm(Egg_rejected ~ FemaleID, data=PSexp_comp, family = binomial(link = "logit"))
#summary(t11)
#femid<-as.factor(PSexp_comp$FemaleID)
#t11<-glm(Egg_rejected ~ femid, data=PSexp_comp, family = binomial(link = "logit"))
#summary(t11)
#This doesn't work and shouldn't be used!


#specify relcomp, both scaled and unscaled
PSexp_comp$relcomp<-PSexp_comp$unscaled_comp/PSexp_comp$HostAv_COMPLEXITY_reduced_ac
PSexp_comp$relcomp_scaled<-scale(PSexp_comp$unscaled_comp/PSexp_comp$HostAv_COMPLEXITY_reduced_ac)
PSexp_comp$HostAv_COMPLEXITY_reduced_ac_scaled<-scale(PSexp_comp$HostAv_COMPLEXITY_reduced_ac)

# should use subset excluding duplicate females
#PSexp_compsub<-PSexp_comp%>%group_by(FemaleID)%>%sample_n(1) - #did this see output called Subsettedexperimentdata18012020.csv
#write.csv(PSexp_compsub,"Subsettedexperimentdata18012020.csv")
#So the subset I did excluded host nests 2018_PS085, 2018_PS098, 2018_PS168 (=1 egg clutch)
#2020_PS009,2020_PS043, 2020_PS096, 2020_PS159

#To re-create this subset
PSexp_comp<-PSexp_comp%>%mutate(Year_Nest = paste(Year, HostNest, sep = "_"))
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2018_PS085"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2018_PS098"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS009"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS043"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS096"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS159"),]

#then omit FemaleID from further tests
t1<-glm(Egg_rejected ~ comp, data=PSexp_comp, family = binomial(link = "logit"))
t1uns<-glm(Egg_rejected ~ unscaled_comp, data=PSexp_comp, family = binomial(link = "logit"))
summary(t1)
#find complexity does predict rejection as would be expected! Estimate=0.5441,Std Error=0.2223,Z=2.448,p=0.0144
t2<-glm(Egg_rejected ~ comp+HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t2)
#looks like absolute complexity does not predict rejection (p~0.5)
t3<-glm(Egg_rejected ~ HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t3)
#similarly p~0.5
t4<-glm(Egg_rejected ~ comp*HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t4)
#there's an interaction term though, with p=0.09

#Ok, now use same methods as LaBarbera et al 2020

#unfortunately there is a correlation between absolute and relative complexity difference
cor.test(PSexp_comp$relcomp,PSexp_comp$unscaled_comp,method="pearson") #Pearson's r = 0.862, CI0.95 = 0.808 to 0.902,p<2.2e-16
#however, it may still be ok to do model comparisons - see 
#https://research-repository.st-andrews.ac.uk/bitstream/handle/10023/15177/Morrissey_2018_Multiple_regressions_PTPB_3_cc.pdf?sequence=1&isAllowed=y

t5<-glm(Egg_rejected ~ relcomp, data=PSexp_comp, family = binomial(link = "logit"))
summary(t5)
#highly significant!!! estimate(unscaled)=4.3037, SE=1.4028,z=3.068,p=0.00216

#same model but scaled - should produce same results and does
t6<-glm(Egg_rejected ~ relcomp_scaled, data=PSexp_comp, family = binomial(link = "logit"))
summary(t6)
#highly significant!!! estimate(scaled)=0.7299 , SE=0.2379,z=3.068,p=0.00216

#now as with LaBarbera, use both relcomp and absolute stimulus in a model
t7<-glm(Egg_rejected ~ relcomp_scaled+HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t7)
#get relcomp_scaled being highly significant (p=0.0039) but absolute complexity not at all (p=0.654)
#as with LaBarbera, check for interaction
t8<-glm(Egg_rejected ~ relcomp_scaled*HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t8)
#interaction term is also non-significant.
#try model with relative and absolute difference?
t9<-glm(Egg_rejected ~ relcomp_scaled+comp, data=PSexp_comp, family = binomial(link = "logit"))
summary(t9)
#actually get relcomp nearly significant and comp not at all
t10<-glm(Egg_rejected ~ relcomp_scaled+comp+HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
summary(t10)
stepAIC(t10)
#looks like stepAIC suggests best model is using relative difference. I.e. Weber's Law.
#well actually there's a 1.8 AIC difference between r~relcomp_sclaed and r~relcomp_scaled plus hostav_complexity_reduced
#so compare model t7 with model t6 using anova
anova(t6,t7,test="Chisq") #output is nonsignificant
#null hypothesis - the models are the same. Data supports null hypothesis that coefficient of hostav_complexity
#is zero. Therefore take the relcomp only model (t6)

#tests addition of terms (relcomp_scaled+comp) in order
anova(t9, test="Chisq") #relcomp scaled improves the model (p=0.0028), but other predictor(s) don't
#(same happens if I use full model (i.e. model t10)) : anova(t10, test="Chisq")
#test t6 with t1?
anova(t1,t6,test="Chisq")
#doesnt produce p value for some reason. But can probably ignore this (or find out more about Wald's chi squared tests) since 
#stepAIC gives a nice result!
anova(t6,test="Chisq") #p=0.002846
anova(t1,test="Chisq") #p=0.01904



#Best to use likelihood ratio tests with function lrtest 
library(epiDisplay)
lrtest(t1,t9) #this compares model with absolute difference with model with both abs and rel
#Chi-squared 1 d.f. =  4.064484 , P value =  0.04379405 , significant
lrtest(t6,t9) #this compares model with relative difference with model with both abs and rel
#Chi-squared 1 d.f. =  0.09401612 , P value =  0.7591323 , not significant
#therefore the model with both is better than the model with abs but not the model with rel?
# ratio-only model (i.e. relative diff) is not improved by the inclusion of absolute difference 
#while the ratio does improve the difference-only model (i.e. absolute diff)


#Nagelkerke R^2
library(rsq)
library(fmsb)
NagelkerkeR2(t6) #R^2=0.1387681 
NagelkerkeR2(t5) #same value as it's just scaling
NagelkerkeR2(t1) #R^2=0.08542784
#suggests that ~14% of variance is explained by relative difference and 9% by absolute difference
#can also use rsq(t1) and rsq(t5) - get 6% and 12% respectively

#can also use vif to test whether mutlicolinearity is important in the full model (not sure this is useful)
library(caret)
car::vif(t9) #get vif score of 3.157878 Apparently a vif score of 5-10 is problematic so this might not be too bad
car::vif(t10) #with full model it's definitely bad. VIFs are 10.4,13.0,4.0 for relcomp, comp, and HostAvComplexity respectively

#plot model t6, the best model with only relcomp as predictor
library(effects)
all.effects <- allEffects(mod = t6)
plot(all.effects,type = "response", ylim = c(-0.1, 1.1),xlab="Relative complexity difference",ylab="Egg rejected?")
points(x=PSexp_comp$relcomp,y=PSexp_comp$Egg_rejected) #for some reason this doesn't plot. So instead do as described below...

#### ggplot ####
library(ggplot2)
ggplot(PSexp_comp,aes(x=relcomp_scaled,y=Egg_rejected))+
  geom_point()+
  geom_smooth(method="glm")+
  labs(title="Effect of relative complexity difference on egg rejection",y="Egg rejected? (0=accepted, 1=rejected",
       x="Relative complexity difference")+
  scale_x_continuous(breaks=seq(-2, 5, 1))
#plots a linear line showing the general linear model I think?

#now make plot of absolute complexity difference
ggplot(PSexp_comp,aes(x=comp,y=Egg_rejected))+
  geom_point()+
  geom_smooth(method="glm")+
  labs(title="Effect of absolute complexity difference on egg rejection",y="Egg rejected? (0=accepted, 1=rejected",
       x="Absolute complexity difference")+
  scale_x_continuous(breaks=seq(-2, 5, 1))

#now actually plot the correct GLM, using unscaled data for relcomp
#first use predict function. With a Binomial glm you need to use type="response" as an argument - see r docu for predict.glm
#to just produce a model with the glm line but without confidence intervals
#pred is a dataframe with the predicted values 
PSexp_comp$pred<-predict(t6,type="response")
ggplot(PSexp_comp,aes(x=relcomp_scaled,y=Egg_rejected))+
  geom_point()+
  geom_line(aes(y=pred))+
  labs(title="Effect of relative complexity difference on egg rejection",y="Egg rejected? (0=accepted, 1=rejected",
       x="Relative complexity difference")

#fullpred is a dataframe with the predicted values 
predictions<-as.data.frame(predict(t6,type="response",se.fit=TRUE))
PSexp_comp$fullpred<-predictions[,1] #dataframe of predicted values according to model
PSexp_comp$fullse<-predictions[,2] #dataframe of predicted standard errors for each value
PSexp_comp$fullscale<-predictions[,3] #not important
#get upper and lower SEs
PSexp_comp$upr<-PSexp_comp$fullpred+PSexp_comp$fullse #standard errors
PSexp_comp$lwr<-PSexp_comp$fullpred-PSexp_comp$fullse
PSexp_comp$upr95<-PSexp_comp$fullpred+(1.96*PSexp_comp$fullse) #95% confidence intervals
PSexp_comp$lwr95<-PSexp_comp$fullpred-(1.96*PSexp_comp$fullse)

#ggplot with line for the model t6 plus standard errors, with unscaled x axis
ggplot(PSexp_comp,aes(x=relcomp,y=Egg_rejected))+
  geom_point(size=2,shape="circle")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14))+
  geom_line(aes(y=fullpred))+
  geom_ribbon(aes(ymin = lwr, ymax = upr),alpha=0.15)+
  labs(y="Egg rejected? (0=accepted, 1=rejected)",
       x="Relative complexity difference")+
  scale_x_continuous(breaks=seq(0, 1, 0.25))
#same as above but 95% confidence intervals rather than standard errors with unscaled x axis
ggplot(PSexp_comp,aes(x=relcomp,y=Egg_rejected))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=fullpred))+
  geom_ribbon(aes(ymin = lwr95, ymax = upr95),alpha=0.15)+
  labs(y="Egg rejected? (0=accepted, 1=rejected)",
       x="Relative complexity difference")+
  scale_x_continuous(breaks=seq(0, 1, 0.25))

#now same ggplots with absolute complexity difference
#fullpred is a dataframe with the predicted values 
ACDpredictions<-as.data.frame(predict(t1uns,type="response",se.fit=TRUE))
PSexp_comp$ACDfullpred<-ACDpredictions[,1] #dataframe of predicted values according to model
PSexp_comp$ACDfullse<-ACDpredictions[,2] #dataframe of predicted standard errors for each value
PSexp_comp$ACDfullscale<-ACDpredictions[,3] #not important
#get upper and lower SEs
PSexp_comp$ACDupr<-PSexp_comp$ACDfullpred+PSexp_comp$ACDfullse #standard errors
PSexp_comp$ACDlwr<-PSexp_comp$ACDfullpred-PSexp_comp$ACDfullse
PSexp_comp$ACDupr95<-PSexp_comp$ACDfullpred+(1.96*PSexp_comp$ACDfullse) #95% confidence intervals
PSexp_comp$ACDlwr95<-PSexp_comp$ACDfullpred-(1.96*PSexp_comp$ACDfullse)

#ggplot with line for the model t1uns plus standard errors, with unscaled x axis
ggplot(PSexp_comp,aes(x=unscaled_comp,y=Egg_rejected))+
  geom_point(size=2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14))+
  geom_line(aes(y=ACDfullpred))+
  geom_ribbon(aes(ymin = ACDlwr, ymax = ACDupr),alpha=0.15)+
  labs(y="Egg rejected? (0=accepted, 1=rejected)",
       x="Absolute complexity difference")
#same as above but 95% confidence intervals rather than standard errors with unscaled x axis
ggplot(PSexp_comp,aes(x=unscaled_comp,y=Egg_rejected))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=ACDfullpred))+
  geom_ribbon(aes(ymin = ACDlwr95, ymax = ACDupr95),alpha=0.15)+
  labs(y="Egg rejected? (0=accepted, 1=rejected)",
       x="Absolute complexity difference")


####model averaging####
library(MuMIn)
#full model 
fullmod<-glm(Egg_rejected ~ relcomp_scaled+comp+HostAv_COMPLEXITY_reduced_ac, data=PSexp_comp, family = binomial(link = "logit"))
#need to set NA to fail regardless of whether there are NAs
options(na.action="na.fail")
results <- dredge(fullmod)
#results gives all models
results
#Model averaging: this is model.avg(name_of_dredged_thing, subset = whatever_you_choose).
summary(model.avg(results, subset = delta < 2))
#basically find two best models - one with just relative complexity difference, the other with both relcomp and 
#absolute host complexity. In both cases the effect of absolute host complexity is tiny - in the full average 
#z=0.168, p=0.9, in the conditional average, z=0.337 p=0.736
#Incidentally can also run results <- dredge(fullmod,rank=BIC)
# and summary(model.avg(results, rank=BIC, subset = delta < 2)) to use BIC - get same output
#PSexp_comp$femid<-as.factor(PSexp_comp$FemaleID)
#NB if I use glmer so that i can have FemaleID as a factor, the model doesn't converge. 
# But shouldn't do this anyway, since have excluded duplciate nests where femaleID is same
#fullmod<-glmer(Egg_rejected ~ relcomp_scaled+comp+HostAv_COMPLEXITY_reduced_ac+(1|femid), data=PSexp_comp, family = binomial(link = "logit"))

#back to na.omit
options(na.action="na.omit")

#NB can turn model results into a table. E.g.
sel.table<-as.data.frame(results)[1:9]
sel.table
# a little clean-up, lets round things a bit
sel.table[,1:9]<- round(sel.table[,1:9],3)
sel.table[,6:8]<- round(sel.table[,6:8],1)
sel.table
#write.csv(sel.table,"My model selection table.csv", row.names = F)

#--------------------------------------------------------------#

#use expression |a-b|/r - if r is close to zero, then it's a difference model
#if r is close to 1 it's a ratio model.

PSexp_comp$absdiff<-abs(PSexp_comp$Exp_COMPLEXITY_ac-PSexp_comp$HostAv_COMPLEXITY_reduced_ac)
PSexp_comp$hostcomp<-PSexp_comp$HostAv_COMPLEXITY_reduced_ac

#each model is |a-b|/r with r from -2 to 2
PSexp_comp$minustwo<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-2))
modminustwo<-glm(Egg_rejected ~ minustwo, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminustwo)
#AIC=107.04

PSexp_comp$minusone<-PSexp_comp$absdiff/((PSexp_comp$hostcomp)^(-1))
modminusone<-glm(Egg_rejected ~ minusone, data=PSexp_comp, family = binomial(link = "logit"))
summary(modminusone)
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

#create data frame with each value of r, it's AIC, and various powers of r for the polynomial fit
r<-c(-1,-0.75, -0.5,-0.25, 0,0.25, 0.5, 0.75,1,1.25, 1.5,1.75, 2)
modAIC<-c(105.77,105.19,104.45,103.53,102.41,101.17,99.941,98.962,98.443,98.479,99,99.835,100.81) #the AICs for the models with different values of r
rsquare<-r^2
rcube<-r^3
rfour<-r^4
rfive<-r^5
rsix<-r^6

plot(r,modAIC)
dat<-data.frame(r,modAIC,rsquare,rcube,rfour,rfive)
#the following stuff didnt work
#fitmod<-lm(modAIC~poly(r,1),data=dat)
#fitmod2<-lm(modAIC~poly(r,2),data=dat)
#fitmod3<-lm(modAIC~poly(r,3),data=dat)
#fitmod4<-lm(modAIC~poly(r,4),data=dat)
#fitmod5<-lm(modAIC~poly(r,5),data=dat)

#summary(fitmod)
#summary(fitmod2)
#summary(fitmod3)
#summary(fitmod4)
#summary(fitmod5)

#predicted1<-predict(fitmod,data.frame(x=r),interval="confidence",level=0.99)
#predicted2<-predict(fitmod2,data.frame(x=r),interval="confidence",level=0.99)
#predicted3<-predict(fitmod3,data.frame(x=r),interval="confidence",level=0.99)
#predicted4<-predict(fitmod4,data.frame(x=r),interval="confidence",level=0.99)
#predicted5<-predict(fitmod5,data.frame(x=r),interval="confidence",level=0.99)

#plot(r,AIC)
#lines(r,predicted1[,1],col="green",lwd=3)
#lines(r,predicted2[,1],col="red",lwd=3)
#lines(r,predicted3[,1],col="blue",lwd=3)
#lines(r,predicted4[,1],col="black",lwd=3)
#lines(r,predicted5[,1],col="orange",lwd=3)

#predictquintic<-data.frame(r,predicted5)
#predictquartic<-data.frame(r,predicted4)
#predictcubic<-data.frame(r,predicted3)

#now fit models to the data quintic, cubic, quartic
quintmodelfit<-lm(modAIC~r+rsquare+rcube+rfour+rfive,data=dat)
summary(quintmodelfit)
stepAIC(quintmodelfit)

cubemodelfit<-lm(modAIC~r+rsquare+rcube,data=dat)
summary(cubemodelfit)
stepAIC(cubemodelfit)

quartmodelfit<-lm(modAIC~r+rsquare+rcube+rfour,data=dat)
summary(quartmodelfit)
stepAIC(quartmodelfit)

#ggplot with best model, here, quintic
ggplot(dat,aes(x=r,y=modAIC))+
  geom_point(data=dat)+
  geom_function(fun=function(x) 102.39999+(-4.90940*x)+(-0.94387*(x^2))+(1.61021*(x^3))+(0.66213*(x^4))+(-0.35879*(x^5)))+
  labs(y="Model AIC",x="r")

#now check if it's overfitted with predicted rsq (this is unnecessary)
#If the predicted R-squared is small compared to R-squared, 
#you might be over-fitting the model even if the independent variables are statistically significant.
#PRESS and pred_r_squared are functions needed to calculate predicted rsquared
#code from https://rpubs.com/RatherBit/102428
#also see https://statisticsbyjim.com/regression/overfitting-regression-models/
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}
pred_r_squared(quintmodelfit) #get 0.999
#now calculate rsq
rsq(quintmodelfit) #get 1.0
#pred rsq is not much smaller than rsq, so model is not overfitted. 
#out of interest, can also get adjusted rsq
adj.r.sqr <- summary(quintmodelfit)$adj.r.squared #also get 1.0


#so use quintic
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


#other model of AIC against r plotted - this is quartic as opposed to quintic above, would need to change numbers
#ggplot(dat,aes(x=r,y=modAIC))+
#  geom_point(data=dat)+
#  geom_function(fun=function(x) 102.0576+(-4.9355*x)+(0.1609*(x^2))+(1.4528*(x^3))+(-0.2361*(x^4)))

#-------------------------------------------------------------------------------------------#

#### SS data ####
setwd("C:/Users/tdixi/Documents/Complexity_project")
SSdata<-read.csv("StoddardExperiments.csv")
SSdata$complex<-abs(SSdata$ExpComplexity-SSdata$AvHostComplexity)
SSdata$DiffNum<-abs(SSdata$DiffNum)
s1<-glm(Result ~ complex, data=SSdata, family = binomial(link = "logit"))
summary(s1)
s1<-glm(Result ~ complex+JND_Colour, data=SSdata, family = binomial(link = "logit"))

SSdata$Dispersion <- abs(SSdata$Host_Dispersion-SSdata$Exp_Dispersion)
SSdata$MeanPropPower <-abs(SSdata$Host_PropPower-SSdata$Exp_PropPower)
SSdata$MeanFilterSize <-abs(SSdata$Host_FilterSize-SSdata$Exp_FilterSize)
# we also have to edit ours slightly (just to make diff in number absolute)
SSdata$DiffNum<-abs(SSdata$DiffNum)
SSdata$DiffNum_all<-abs(SSdata$DiffNum_all)
SSdata$FS<-abs(SSdata$Exp_FS-SSdata$Host_FS)
SSdata$FSD<-abs(SSdata$Exp_FSD-SSdata$Host_FSD)

#correlations
chart.Correlation(SSdata[c("ExpHost","DiffNum","FSD","FS","Dispersion","JND_Colour","complex")],method="pearson")

#run model
s1<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+JND_Colour+complex, data=SSdata, family = binomial(link = "logit"))
summary(s1)
s2<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+JND_Colour+complex+ExpHost+DiffNum, data=SSdata, family = binomial(link = "logit"))

library(MuMIn)
options(na.action="na.fail")
results <- dredge(s2)
#results gives all models
results
#Model averaging: this is model.avg(name_of_dredged_thing, subset = whatever_you_choose).
summary(model.avg(results, subset = delta < 2))
options(na.action="na.omit")
#get model with exactly same predictors as S&S and Stoddard et al Phil Trans
#so no effect of complexity. This may be because colour differences are swamping other diffs.
# therefore exclude high JND values
SSdata1<-SSdata[which(SSdata$JND_Colour<=3),]#should give n=38
s1<-glm(Result ~ complex, data=SSdata1, family = binomial(link = "logit"))
summary(s1)

s2<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+JND_Colour+complex+ExpHost+DiffNum, data=SSdata1, family = binomial(link = "logit"))
s3<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+JND_Colour+complex+ExpHost, data=SSdata1, family = binomial(link = "logit"))

library(MuMIn)
options(na.action="na.fail")
results <- dredge(s3)
#results gives all models
results
#Model averaging: this is model.avg(name_of_dredged_thing, subset = whatever_you_choose).
summary(model.avg(results, subset = delta < 2))
options(na.action="na.omit")
# now complexity does show up in the final model, with MeanFilterSize,MeanPropPower,Dispersion,DiffNum
SSdata1$MeanFilterSize<-scale(SSdata1$MeanFilterSize)
SSdata1$MeanPropPower<-scale(SSdata1$MeanPropPower)
SSdata1$Dispersion<-scale(SSdata1$Dispersion)
SSdata1$DiffNum<-scale(SSdata1$DiffNum)
SSdata1$complex<-scale(SSdata1$complex)
SSdata1$ExpHost<-scale(SSdata1$ExpHost)

s4<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+complex+ExpHost+DiffNum, data=SSdata1, family = binomial(link = "logit"))
summary(s4)
s5<-glm(Result ~ MeanPropPower+MeanFilterSize+Dispersion+ExpHost+complex, data=SSdata1, family = binomial(link = "logit"))

options(na.action="na.fail")
results <- dredge(s5)
#results gives all models
results
#Model averaging: this is model.avg(name_of_dredged_thing, subset = whatever_you_choose).
summary(model.avg(results, subset = delta < 2))
options(na.action="na.omit")
##again get complexity in final model.
#conditional: complex      Est    1.0890    SE 0.5043    Adjusted SE  0.5230  Z 2.082  P 0.0373 *
#but best model is s6 below - only other model with deltaAIC<2 is more complex (s6 is nested within it) so ignore model averaged coefficients.

s6<-glm(Result ~ MeanPropPower+MeanFilterSize+complex, data=SSdata1, family = binomial(link = "logit"))
summary(s6)
#absolute complexity difference (Estimate ?SE = 1.109 ?0.492, Z = 2.254, P = 0.024
#pattern filter size (Estimate ?SE = 0.954 ?0.435, Z = 2.193, P = 0.028)
#pattern proportion energy (Estimate ?SE = 1.153 ?0.497, Z = 2.318, P = 0.021)

library(hier.part) # stats
library(rsq)
library(fmsb)
NagelkerkeR2(s6) #37.8% (n=38)
pred<-SSdata1[c("MeanPropPower","MeanFilterSize","complex")]
hier.part(SSdata1$Result,pred,fam="binomial",gof = "logLik") #of this, complexity explains 31.1949%

#------------------------------------------------------------------------#

####CF vs prinia ####
#CF_Complexities<-read.csv("CF_complexity_20181920.csv")
#CF_Complexities <- separate(CF_Complexities, Year_Nest_Egg_NestIn, c("Year", "Nest", "Egg", "NestEggIn"), sep = "_")
#CF_Complexities<-CF_Complexities%>%mutate(Year_Nest = paste(Year, Nest, sep = "_"))
#subset by nest
#CF_Complexities_sub<-CF_Complexities%>%group_by(Year_Nest)%>%sample_n(1)
##V IMP - all above is comments because:
#BY HAND append cuckoo finch data set to prinia dataset adding species = P or A
setwd("C:/Users/tdixi/Documents/Complexity_project")
PSCFall_comp<-read.csv("PandCF181920_complexity.csv")
PSCFall_comp<-PSCFall_comp%>%mutate(Year_Nest = paste(Year, Nest, sep = "_"))
PSCFall_comp<-PSCFall_comp%>%mutate(Year_Nest_Species = paste(Year_Nest, Species, sep = "_"))
set.seed(13)
PSCFall_compsub<-PSCFall_comp%>%group_by(Year_Nest_Species)%>%sample_n(1)
anom<-PSCFall_compsub[which(PSCFall_compsub$Species=="A"),]#should give number of CF nests = 30
prinia<-PSCFall_compsub[which(PSCFall_compsub$Species=="P"),]#should give number of P nests = 178

library(ggplot2)

#boxplot
ggplot(PSCFall_compsub,aes(Species,COMPLEXITY_ac))+
  geom_boxplot(fill="grey",outlier.shape = NA)+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  geom_point(position="jitter",)+
  labs(x="Species",
       y="Complexity")+
  scale_x_discrete(labels=c("Cuckoo finch","Prinia"))

kruskal.test(COMPLEXITY_ac ~ Species, data = PSCFall_compsub)
#Kruskal-Wallis chi-squared = 24.217, df = 1, p-value = 8.605e-07
t.test(COMPLEXITY_ac ~ Species, data = PSCFall_compsub)
#t = -9.4562, df = 128.1, p-value < 2.2e-16

shapiro.test(prinia$COMPLEXITY_ac)#p<0.05 so should not assume normality
shapiro.test(anom$COMPLEXITY_ac)#p>0.05 so can assume normality

hist(prinia$COMPLEXITY_ac)
hist(anom$COMPLEXITY_ac)
#both have some skew

#these suggest should use kruskal-wallis as above

#### testing whether other granularity/colour measures important in rejection ####
library(PerformanceAnalytics) # easy correlation plot
library(dplyr)
library(MASS) #for stepaic
library(MuMIn) #model averaging
library(fmsb) #for naglekerke
library(irr)

setwd("C:/Users/tdixi/Documents/Complexity_project")
PSexp_comp<-read.csv("Experiments_complexity.csv",header=T,sep=",")


setwd("C:/Users/tdixi/Documents/Granularity")
PSexp_nocomp<-read.csv("PSExperimentscomplete_oldgran_C1.csv")
PSexp<-left_join(PSexp_nocomp,PSexp_comp,by="X")

PSexp<-PSexp%>%mutate(Year_Nest = paste(Year.x, HostNest.x, sep = "_"))
PSexp$reduced_OldEmax<-abs(PSexp$Exp_OldEmax1_ac-PSexp$HostAv_OldEmax1_reduced_ac)
PSexp$reduced_Eprop<-abs(PSexp$Exp_AvEprop_ac-PSexp$HostAv_reduced_AvEprop_ac)
PSexp$reduced_PD<-abs(PSexp$Exp_AvPD_ac-PSexp$HostAv_reduced_AvPD_ac)
PSexp$reduced_Emax<-abs(PSexp$Exp_AvEmax1_ac-PSexp$HostAv_reduced_AvEmax1_ac)
PSexp$reduced_ExpHost<-PSexp$Av_ExpHost_dist_reducedac
PSexp$reduced_Complexity<-abs(PSexp$Exp_COMPLEXITY_ac-PSexp$HostAv_COMPLEXITY_reduced_ac)


PSexp<-PSexp[!(PSexp$Year_Nest=="2018_PS085"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2018_PS098"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2020_PS009"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2020_PS043"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2020_PS096"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2020_PS159"),]
PSexp<-PSexp[!(PSexp$Year_Nest=="2020_PS159"),]
PSexp<-PSexp[!(PSexp$ClutchSize.x=="1"),]

t2<-glm(Egg_rejected.x ~ reduced_Eprop+reduced_OldEmax+reduced_PD+dS+reduced_ExpHost, data=PSexp, family = binomial(link = "logit"))
summary(t2)

library(MuMIn)
#full model 
fullmod<-glm(Egg_rejected.x ~ reduced_Eprop+reduced_OldEmax+reduced_PD+dS+reduced_ExpHost, data=PSexp, family = binomial(link = "logit"))
#need to set NA to fail regardless of whether there are NAs
options(na.action="na.fail")
results <- dredge(fullmod)
#results gives all models
results
#Model averaging: this is model.avg(name_of_dredged_thing, subset = whatever_you_choose).
summary(model.avg(results, subset = delta < 2))
plot(reduced_OldEprop~Egg_rejected,data=PSexp)

summary(PSexpfull$reduced_Eprop)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0001614 0.0070000 0.0138333 0.0180471 0.0232500 0.0791667 

summary(PSexpfull$reduced_OldEmax)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1250  0.3333  0.3865  0.5625  1.2500 

summary(PSexpfull$reduced_PD)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0001189 0.0295006 0.0655648 0.0823185 0.1253865 0.2883472 

summary(PSexpfull$dS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3619  1.5422  2.5429  2.7898  3.5573 10.0963 


mean(SSdata$MeanFilterSize) #0.6518817
mean(SSdata$JND_Colour) #4.730623
mean(SSdata$Dispersion) #0.2683257
mean(SSdata$MeanPropPower) #0.034

median(SSdata$MeanFilterSize) #0.5
median(SSdata$JND_Colour) #4.38125
median(SSdata$Dispersion) #0.18005
median(SSdata$MeanPropPower) #0.02428333

t.test(SSdata$MeanFilterSize,PSexpfull$reduced_OldEmax)
t.test(SSdata$JND_Colour,PSexpfull$dS)
t.test(SSdata$MeanPropPower,PSexpfull$Eprop)
t.test(SSdata$Dispersion,PSexpfull$PD)

