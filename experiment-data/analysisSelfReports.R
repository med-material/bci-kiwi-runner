library(here)
library(polr)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(lme4)
setwd(here("experiment-data"))
data<-read.csv("kiwiQuantReport.csv",header=TRUE,sep = ",")
data[is.na(data$shamChange),]$shamChange<-0

#first check with Linear model effect for sham rate but not order
summary(step(lm(controlEpisode~shamRate+order,data=data)))
#first check with Linear model effect with shamChange
summary(step(lm(controlEpisode~shamChange,data=data)))
# check with Linear model for just shamchange instead

summary(step(lm(controlEpisode~shamChange,data=data)))

#check with ordinal logistic reqression - no significance
summary(polr(as.factor(FrustEpisode) ~ shamRate, data = data, Hess=TRUE))


percContr.null<- lmer(controlEpisode~(1|PID),data=data,REML=FALSE)
percContr.Sham <-lmer(controlEpisode~shamRate+(1|PID),data=data,REML=FALSE)
percContr.ShamChange <-lmer(controlEpisode~shamChange+(1|PID),data=data,REML=FALSE)
anova(percContr.null,percContr.ShamChange)
anova(percContr.null,percContr.Sham)
percContr.ShamOrder <-lmer(controlEpisode~shamRate+shamChange+(1|PID),data=data,REML=FALSE)
anova(percContr.Sham,percContr.ShamOrder)
