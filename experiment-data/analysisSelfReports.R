library(here)

data<-read.csv("kiwiQuantReports.csv",header=TRUE,sep = ",")

summary(lm(FrustEpisode~shamRate*order,data=data[data$order<3,]))
