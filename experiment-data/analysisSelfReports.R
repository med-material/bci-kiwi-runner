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
require(sqldf)

## data preparation --------------------------------------------------------

#verify with excluding PID=2 (only got 9/20 instead of 10/20)
#setwd
setwd(here::here("experiment-data"))
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

## stats analyis modelling  -------------

summary(step(lm(controlEpisode~shamChange,data=data)))

friedman.test.with.post.hoc(FrustEpisode~shamRate|PID,data[,c("FrustEpisode","shamRate","PID")],)

friedman.test.with.post.hoc(controlEpisode~shamRate|PID,data[,c("controlEpisode","shamRate","PID")],)
friedman.test.with.post.hoc(MotivationEpisode~shamRate|PID,data[,c("controlEpisode","shamRate","PID")],)
friedman.test(MotivationEpisode~shamRate|PID,data)

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

# plotting fabrictaion Rate vs Frustration w/ Error Bars (95% Confidence Interval)  ----------- 
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

# plotting fabrictaion Rate vs Frustration w/ Error Bars (95% Confidence Interval)  ) + raw data ----------- 
# Plot Sham Rate vs Frustration w/ Error Bars (95% Confidence Interval
data$shamRate = factor(data$shamRate, levels=c("0", "15", "30"))
data$FrustNormalized = (data$FrustEpisode-1)/6
dsm = summarySE(data, measurevar="FrustNormalized", groupvars=c("shamRate"))
ggplot(dsm, aes(x=dsm$shamRate, y=dsm$FrustNormalized)) +
  geom_violin(data=data,aes(factor(shamRate),FrustNormalized),trim=FALSE)+
  geom_errorbar(aes(ymin=dsm$FrustNormalized-dsm$ci, ymax=dsm$FrustNormalized+dsm$ci), width=.1, size=.5) +
  geom_point(size=3.25)+ 
  geom_line(aes(group=1), size=1.25) +
  geom_jitter(data=data,aes(factor(shamRate),FrustNormalized),width = .2,alpha=.2)+
  ylim(0,1) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  xlab("Fabrication Rate (%)") +
  ylab("Frustration")

# Plot Sham Rate vs Perceived Control w/ Error Bars -----------------
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

#Plot Fabrication Rate vs Perceived Control w/ Error Bars  with raw data ----------
#Plot Sham Rate vs Perceived Control w/ Error Bars  with raw data

ggplot(dsm, aes(x=dsm$shamRate, y=dsm$controlNormalized)) +
  geom_violin(data=data,aes(factor(shamRate),controlNormalized),trim=FALSE)+
  geom_errorbar(aes(ymin=dsm$controlNormalized-dsm$ci, ymax=dsm$controlNormalized+dsm$ci), width=.1, size=.5) +
  geom_point(size=3.25) + 
  geom_line(aes(group=1), size=1.25) +
  #geom_point(data=data,aes(factor(shamRate),controlNormalized))+
  geom_jitter(data=data,aes(factor(shamRate),controlNormalized),width = .2,alpha=.2)+
  ylim(0,1) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  xlab("Fabrication Rate (%)") +   ylab("Perceived Control")

#check if perceived control is just inverse of Frustration, doesn't seem to be the case
summary(step(lm(FrustEpisode~controlEpisode*as.factor(shamRate),data)))

ggplot(data,aes(x=controlEpisode,y=FrustEpisode,colour=shamRate,group=shamRate,alpha=.3))+geom_point()+geom_jitter(width=.1)+theme_bw()+geom_smooth(method = "lm", se = FALSE)

# comparison with people that had some sort of insight into what we were manipulating vs not
sqldf("select ExpInsight, avg(ControlEpisode) from data group by expinsight")
sqldf("select ExpInsight, avg(FrustEpisode) from data group by expinsight")
sqldf("select ExpInsight, median(ControlEpisode) from data group by expinsight")
sqldf("select ExpInsight, median(FrustEpisode) from data group by expinsight")


# analysis of by participant change in ratings between conditions checking for reasons for bi-modal distribution 
ggplot(data,aes(x=shamRate,y=controlNormalized,group=PID,colour=as.factor(PID)))+geom_point(position=position_jitter(w=0.02, h=0.02))+geom_line(position=position_jitter(w=0.02, h=0.02))

ctrldataBL<-sqldf('select PID, controlEpisode as ctrlBL from data where shamRate=0')
#sqldf('select d.PID, d.shamRate, d.controlEpisode - c.ctrlBL as CtrlChange from data as d, ctrldataBL as c where d.PID=c.PID')
ctrlChangeFromSham<-sqldf('select d.PID, d.shamRate, d.controlEpisode, d.controlEpisode - c.ctrlBL as CtrlChange from data as d, ctrldataBL as c where d.PID=c.PID')
ctrlChangeFromSham$aboveAvg<-ifelse(ctrlChangeFromSham$controlEpisode>3,1,0)
sqldf('select shamRate, avg(ctrlchange) from  ctrlChangeFromSham  group by shamRate')
sqldf('select shamRate, aboveAvg,avg(ctrlchange) from  ctrlChangeFromSham  group by shamRate,aboveAvg')
t.test(ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$controlEpisode~ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$aboveAvg)
#test whether people above average in 15%sham saw higher gains getting there than the people inthe lower half
t.test(ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$CtrlChange~ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$aboveAvg)
t.test(ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$controlEpisode-ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$CtrlChange~ctrlChangeFromSham[ctrlChangeFromSham$shamRate==15,]$aboveAvg)


#test whether those who didn't respond well to sham were more frustrated at baseline and at 15%
FrustCtrl<-sqldf('select c.PID, c.shamRate, d.controlEpisode, d.CtrlChange, d.aboveAvg, c.FrustEpisode from ctrlChangeFromSham as d, data as c where d.PID=c.PID and d.shamRate=15')
sqldf('select shamRate, aboveAvg, avg(FrustEpisode) from FrustCtrl group by shamRate, aboveAvg')
t.test(FrustCtrl[FrustCtrl$shamRate==0,]$FrustEpisode~FrustCtrl[FrustCtrl$shamRate==0,]$aboveAvg)
t.test(FrustCtrl[FrustCtrl$shamRate==15,]$FrustEpisode~FrustCtrl[FrustCtrl$shamRate==15,]$aboveAvg)


# analysis of by participant change in ratings between conditions checking for reasons for bi-modal distribution in 30% fab input 
ggplot(data,aes(x=shamRate,y=FrustNormalized,group=PID,colour=as.factor(PID)))+geom_point(position=position_jitter(w=0.02, h=0.02))+geom_line(position=position_jitter(w=0.02, h=0.02))




# analysis of correlation between frust and percC ------------
 require(foreign)
 require(ggplot2)
 require(MASS)
 require(Hmisc)
 require(reshape2)

head(data)
m <- polr( as.factor(FrustEpisode) ~ as.factor(controlEpisode) , data = data, Hess=TRUE)
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

ci <- confint(m)
exp(coef(m))
exp(cbind(OR = coef(m), ci))

corr<-cor.test(data$FrustEpisode,data$controlEpisode,method="spearman")
corr0<-cor.test(data[data$shamRate==0,]$FrustEpisode,data[data$shamRate==0,]$controlEpisode,method="spearman")

cor.test(data[data$shamRate==15,]$FrustEpisode,data[data$shamRate==15,]$controlEpisode,method="spearman")
cor.test(data[data$shamRate==15 |data$shamRate==30,]$FrustEpisode,data[data$shamRate==15|data$shamRate==30,]$controlEpisode,method="spearman")
cor.test(data[data$shamRate==30,]$FrustEpisode,data[data$shamRate==30,]$controlEpisode,method="spearman")

sham0 <- data.frame(cbind(rank(data[data$shamRate==0,]$FrustEpisode, ties.method = 'average'),
                                rank(data[data$shamRate==0,]$controlEpisode, ties.method = 'average')))
colnames(sham0) <- c('FrustEpisode', 'controlEpisode')
rho0 <- cov(sham0) / (sd(sham0$FrustEpisode) * sd(sham0$FrustEpisode))
rho0[[2]]
corr0$estimate

#correlation frust/percC considering ranked ties manually computed as per https://rpubs.com/aaronsc32/spearman-rank-correlation -------
corr<-cor.test(data$FrustEpisode,data$controlEpisode,method="spearman")
sham <- data.frame(cbind(rank(data$FrustEpisode, ties.method = 'average'),
                          rank(data$controlEpisode, ties.method = 'average')))
colnames(sham) <- c('FrustEpisode', 'controlEpisode')
rho <- cov(sham) / (sd(sham$FrustEpisode) * sd(sham$FrustEpisode))
rho[[2]]
corr$estimate
n <- length(sham$FrustEpisode)
r<-cor(x = data$FrustEpisode, y = data$controlEpisode, method = 'pearson')
r
s<-(n^3-n)*(1-r)/6
s
corr$statistic
t <- r * sqrt((n - 2) / (1 - r^2))
p <- 2 * (1-pt(q = t, df = n - 2))
p
corr$p.value

#sham0 frust/percC correlation considering ranked ties manually computed as per https://rpubs.com/aaronsc32/spearman-rank-correlation ----
corr<-cor.test(data[data$shamRate==0,]$FrustEpisode,data[data$shamRate==0,]$controlEpisode,method="spearman")
sham <- data.frame(cbind(rank(data[data$shamRate==0,]$FrustEpisode, ties.method = 'average'),
                         rank(data[data$shamRate==0,]$controlEpisode, ties.method = 'average')))
colnames(sham) <- c('FrustEpisode', 'controlEpisode')
rho <- cov(sham) / (sd(sham$FrustEpisode) * sd(sham$FrustEpisode))
rho[[2]]
corr$estimate
n <- length(sham$FrustEpisode)
r<-cor(x = data[data$shamRate==0,]$FrustEpisode, y = data[data$shamRate==0,]$controlEpisode, method = 'pearson')
r
s<-(n^3-n)*(1-r)/6
s
corr$statistic
t <- r * sqrt((n - 2) / (1 - r^2))
p <- 2 * (1-pt(q = t, df = n - 2))
p
corr$p.value

#sham15 frust/percC correlation considering ranked ties manually computed as per https://rpubs.com/aaronsc32/spearman-rank-correlation ----
corr<-cor.test(data[data$shamRate==15,]$FrustEpisode,data[data$shamRate==15,]$controlEpisode,method="spearman")
sham <- data.frame(cbind(rank(data[data$shamRate==15,]$FrustEpisode, ties.method = 'average'),
                         rank(data[data$shamRate==15,]$controlEpisode, ties.method = 'average')))
colnames(sham) <- c('FrustEpisode', 'controlEpisode')
rho <- cov(sham) / (sd(sham$FrustEpisode) * sd(sham$FrustEpisode))
rho[[2]]
corr$estimate
n <- length(sham$FrustEpisode)
r<-cor(x = data[data$shamRate==15,]$FrustEpisode, y = data[data$shamRate==15,]$controlEpisode, method = 'pearson')
r
s<-(n^3-n)*(1-r)/6
s
corr$statistic
t <- r * sqrt((n - 2) / (1 - r^2))
p <- 2 * (1-pt(q = t, df = n - 2))
p
corr$p.value
#sham30 frust/percC correlation considering ranked ties manually computed as per https://rpubs.com/aaronsc32/spearman-rank-correlation ----
corr<-cor.test(data[data$shamRate==30,]$FrustEpisode,data[data$shamRate==03,]$controlEpisode,method="spearman")
sham <- data.frame(cbind(rank(data[data$shamRate==30,]$FrustEpisode, ties.method = 'average'),
                         rank(data[data$shamRate==30,]$controlEpisode, ties.method = 'average')))
colnames(sham) <- c('FrustEpisode', 'controlEpisode')
rho <- cov(sham) / (sd(sham$FrustEpisode) * sd(sham$FrustEpisode))
rho[[2]]
corr$estimate
n <- length(sham$FrustEpisode)
r<-cor(x = sham$FrustEpisode, y = sham$controlEpisode, method = 'pearson')
r
s<-(n^3-n)*(1-r)/6
s
corr$statistic
t <- r * sqrt((n - 2) / (1 - r^2))
p <- 2 * (1-pt(q = t, df = n - 2))
p
corr$p.value