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
