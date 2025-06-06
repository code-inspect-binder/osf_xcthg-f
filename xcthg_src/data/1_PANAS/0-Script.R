# PANAS 

# last mod: 2016-01-21 mh

library(plyr)
library(reshape2)
library(ggplot2)
library(psych)
library(ez)
library(PsychHelperFunctions) # Package available at https://github.com/markushuff/PsychHelperFunctions
library(sciplot)

# Timepoints
# 0: before the match
# 1: half-time
# 2: after the match
# 3: ten days after the match

# PANAS

# measured sub-scales:
# “positive affect”, “negative affect”, “fear”, “attentiveness”, “hostility”, and “self-assurance” 

dat <- read.csv("raw_data.csv",sep=";",dec=",")


dat$Fan <- as.factor(dat$Fangruppe)
dat$Fan <- revalue(dat$Fan,c("1"="BVB", "2"="FCB"))

dat_PANAS <- dat[,c(6,527,484:507)]
dat_PANAS <- dat_PANAS[!is.na(dat_PANAS$negEmo4),]


tmp <- melt(dat_PANAS,id.vars = c("VP_t0","Fan"))
tmp <- tmp[!is.na(tmp$Fan),]

tmp$time <- NA
tmp[grepl("1",tmp$variable),]$time <- "0"
tmp[grepl("2",tmp$variable),]$time <- "1"
tmp[grepl("3",tmp$variable),]$time <- "2"
tmp[grepl("4",tmp$variable),]$time <- "3"

tmp$scale <- NA
tmp[grepl("posEmo",tmp$variable),]$scale <- "Positive Affect"
tmp[grepl("selfass",tmp$variable),]$scale <- "Self-Assurance"
tmp[grepl("attentiv",tmp$variable),]$scale <- "Attentiveness"
tmp[grepl("negEmo",tmp$variable),]$scale <- "Negative Affect"
tmp[grepl("angst",tmp$variable),]$scale <- "Fear"
tmp[grepl("hostility",tmp$variable),]$scale <- "Hostility"

dat <- tmp

# Wie viele VPN?
head(dat)

ddply(dat,.(Fan),summarize,n=length(unique(VP_t0)))


## Plot

dat_agg <- ddply(dat,.(Fan,time,scale),summarise,mean_value=mean(value),se_value=se(value))
dat_agg$Fan <- revalue(dat_agg$Fan,replace=c("BVB"="BVB Fans","FCB"="FCB Fans"))

ggplot(dat_agg,aes(x=time,y=mean_value,color=Fan))+
  facet_wrap(~scale,nrow = 2)+
  geom_point()+
  geom_errorbar(aes(ymin = mean_value-se_value, ymax = mean_value+se_value),width=.3)+
  theme_bw(base_size=20)+
  scale_color_manual(values=c("goldenrod2","darkred"))+
  theme(legend.title = element_blank())+
  labs(xlab("Time Point of Measurement"))+
  labs(ylab("Value"))


library(readr)
write_csv(dat_agg,"dat_PANAS.csv")

## Statistics
### Overall ANOVA
summary(aov(value~Fan*time*scale+Error(VP_t0/(time*scale)),data=dat))

### Separate ANOVAs for each sub-scale
### Pairwise t-tests with adjusted alpha level of .05/4 = .0125 (because there are four relevant comparisons)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Attentiveness",]))
pairwise.t.test(dat[dat$scale=="Attentiveness",]$value,paste(dat[dat$scale=="Attentiveness",]$Fan,dat[dat$scale=="Attentiveness",]$time),p.adj="none")
# at t2 (after the match) significant higher "attentiveness" scores for FCB fans, p = .00634

ez_att <- ezANOVA(data = dat[dat$scale=="Attentiveness",],
                  dv = value,
                  wid = VP_t0,
                  between = Fan,
                  within = time,
                  detailed = TRUE)
ezANOVA.pes(ez_att)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Fear",]))
pairwise.t.test(dat[dat$scale=="Fear",]$value,paste(dat[dat$scale=="Fear",]$Fan,dat[dat$scale=="Fear",]$time),p.adj="none")
# no significant time x fan interaction

ez_fear <- ezANOVA(data = dat[dat$scale=="Fear",],
                  dv = value,
                  wid = VP_t0,
                  between = Fan,
                  within = time,
                  detailed = TRUE)
ezANOVA.pes(ez_fear)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Hostility",]))
pairwise.t.test(dat[dat$scale=="Hostility",]$value,paste(dat[dat$scale=="Hostility",]$Fan,dat[dat$scale=="Hostility",]$time),p.adj="none")
# at t2 (after the match) significant higher "hostility" scores for BVB fans, p = 6.0e-06

ez_hostility <- ezANOVA(data = dat[dat$scale=="Hostility",],
                   dv = value,
                   wid = VP_t0,
                   between = Fan,
                   within = time,
                   detailed = TRUE)
ezANOVA.pes(ez_hostility)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Negative Affect",]))
pairwise.t.test(dat[dat$scale=="Negative Affect",]$value,paste(dat[dat$scale=="Negative Affect",]$Fan,dat[dat$scale=="Negative Affect",]$time),p.adj="none")
# at t2 (after the match) significant higher "negative affect" scores for BVB fans, p = 0.00019

ez_negaff <- ezANOVA(data = dat[dat$scale=="Negative Affect",],
                        dv = value,
                        wid = VP_t0,
                        between = Fan,
                        within = time,
                        detailed = TRUE)
ezANOVA.pes(ez_negaff)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Positive Affect",]))
pairwise.t.test(dat[dat$scale=="Positive Affect",]$value,paste(dat[dat$scale=="Positive Affect",]$Fan,dat[dat$scale=="Positive Affect",]$time),p.adj="none")
# at t0 (before the match) no significant difference in "positive affect" scores, p = 0.0260
# at t1 (half-time) no significant difference in "positive affect" scores, p = 0.0134
# at t2 (after the match) significant higher "positive affect" scores for FCB fans, p = < 2e-16
# at t3 (10 days after the match) no significant difference in "positive affect" scores, p = 0.0890

ez_posaff <- ezANOVA(data = dat[dat$scale=="Positive Affect",],
                     dv = value,
                     wid = VP_t0,
                     between = Fan,
                     within = time,
                     detailed = TRUE)
ezANOVA.pes(ez_posaff)

summary(aov(value~Fan*time+Error(VP_t0/(time)),data=dat[dat$scale=="Self-Assurance",]))
pairwise.t.test(dat[dat$scale=="Self-Assurance",]$value,paste(dat[dat$scale=="Self-Assurance",]$Fan,dat[dat$scale=="Self-Assurance",]$time),p.adj="none")
# at t2 (after the match) significant higher "positive affect" scores for FCB fans, p = 1.7e-07

ez_selfass<- ezANOVA(data = dat[dat$scale=="Self-Assurance",],
                     dv = value,
                     wid = VP_t0,
                     between = Fan,
                     within = time,
                     detailed = TRUE)
ezANOVA.pes(ez_selfass)

###
# Cronbachs Alpha

alpha(dat_PANAS,check.keys=FALSE)