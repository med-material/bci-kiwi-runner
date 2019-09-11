library(here)
library(polr)
library(Rmisc)
library(ggplot2)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(lme4)
require(here)
require(coin)
#verify with excluding PID=2 (only got 9/20 instead of 10/20)
#setwd
setwd(here("experiment-data"))
data<-read.csv("kiwiQuantReport.csv",header=TRUE,sep = ",")
data[is.na(data$shamChange),]$shamChange<-0
#<-data[!data$PID==2,]
data$shamRateCont<-as.numeric(data$shamRate)
 data<-data[order(data$PID, data$shamRate),]
#first check with Linear model effect for sham rate but not order
summary(lm(controlEpisode~shamRateCont,data=data))
#first check with Linear model effect with shamChange
summary(step(lm(controlEpisode~shamChange,data=data)))
# check with Linear model for just shamchange instead

summary(step(lm(controlEpisode~shamChange,data=data)))

friedman.test.with.post.hoc(FrustEpisode~shamRate|PID,data[,c("FrustEpisode","shamRate","PID")],)

friedman.test.with.post.hoc(controlEpisode~shamRate|PID,data[,c("controlEpisode","shamRate","PID")],)


wilcoxsign_test(data[data$shamRate=='0',]$FrustEpisode ~ data[data$shamRate=='15',]$FrustEpisode, distribution="exact")
wilcoxsign_test(data[data$shamRate=='0',]$FrustEpisode ~ data[data$shamRate=='30',]$FrustEpisode, distribution="exact")

wilcoxsign_test(GroupA ~ GroupB, distribution="exact")
wilcoxsign_test(data[data$shamRate=='0',]$controlEpisode ~ data[data$shamRate=='15',]$controlEpisode, distribution="exact")
wilcoxsign_test(data[data$shamRate=='0',]$controlEpisode ~ data[data$shamRate=='30',]$controlEpisode, distribution="exact")


#check with ordinal logistic reqression - no significance
summary(polr(as.factor(FrustEpisode) ~ shamRate, data = data, Hess=TRUE))


percContr.null<- lmer(controlEpisode~(1|PID),data=data,REML=FALSE)
percContr.Sham <-lmer(controlEpisode~shamRate+(1|PID),data=data,REML=FALSE)
percContr.ShamChange <-lmer(controlEpisode~shamChange+(1|PID),data=data,REML=FALSE)
anova(percContr.null,percContr.ShamChange)
anova(percContr.null,percContr.Sham)
percContr.ShamOrder <-lmer(controlEpisode~shamRate+shamChange+(1|PID),data=data,REML=FALSE)
anova(percContr.Sham,percContr.ShamOrder)

# lmer for frustration
Frust.null<- lmer(FrustEpisode~(1|PID),data=data,REML=FALSE)
Frust.Sham <-lmer(FrustEpisode~shamRate+(1|PID),data=data,REML=FALSE)
anova(Frust.null,Frust.Sham)
FrustContr.ShamOrder <-lmer(FrustEpisode~shamRate+shamChange+(1|PID),data=data,REML=FALSE)
# friedmann test on sham amount
anova(Frust.Sham,FrustContr.ShamOrder)


# Plot Sham Rate vs Frustration w/ Error Bars (95% Confidence Interval)
data$shamRate = factor(data$shamRate, levels=c("0", "15", "30"))
data$FrustNormalized = (data$FrustEpisode-1)/6
dsm = summarySE(data, measurevar="FrustNormalized", groupvars=c("shamRate"))
ggplot(dsm, aes(x=dsm$shamRate, y=dsm$FrustNormalized)) +
  geom_errorbar(aes(ymin=dsm$FrustNormalized-dsm$ci, ymax=dsm$FrustNormalized+dsm$ci), width=.1, size=.5) +
  geom_point(size=3.25) +
  geom_line(aes(group=1), size=1.25) +
  ylim(0,1) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  xlab("Fabrication Rate (%)") +
  ylab("Frustration")

# Plot Sham Rate vs Perceived Control w/ Error Bars
data$shamRate = factor(data$shamRate, levels=c("0", "15", "30"))
data$controlNormalized = (data$controlEpisode-1)/6
dsm = summarySE(data, measurevar="controlNormalized", groupvars=c("shamRate"))
ggplot(dsm, aes(x=dsm$shamRate, y=dsm$controlNormalized)) +
  geom_errorbar(aes(ymin=dsm$controlNormalized-dsm$ci, ymax=dsm$controlNormalized+dsm$ci), width=.1, size=.5) +
  geom_point(size=3.25) +
  geom_line(aes(group=1), size=1.25) +
  ylim(0,1) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  xlab("Fabrication Rate (%)") +
  ylab("Perceived Control")
