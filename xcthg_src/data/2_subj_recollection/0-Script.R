# Analysis of the subject recollection questionnaire, that was 
# filled-in by the participants 10 to 12 days after the match.

library(plyr)
library(readr)
library(ggplot2)
library(psych)
library(sciplot)
library(lsr)
library(PsychHelperFunctions)  # available at https://github.com/markushuff/PsychHelperFunctions

dat <- read_csv2("questionnaire_subj_recoll.csv")
dat <- dat[!is.na(dat$code),]
participant_code <- read_csv("participant_code.csv")
participant_fan <- read_csv2("VPFan-Liste060214.csv")

fan <- merge(participant_code,participant_fan)
dat <- merge(dat,fan)

# Wie viele VPN?
head(dat)
ddply(dat,.(fan_cl_spiel),summarise,n=length(unique(participant)))

###################
# Subj. recollection questions
#
# proportion questions added to 100 % , e.g., 60% FCB implies 40% BVB
#
# dat$spielanteil_FCB : propotion of game of FCB
# dat$zweikaempfeanteil_FCB : won tacklings by FCB
# dat$torchancenanteil_FCB : proportion of goal-scoring changes by FCB


dat$spielanteil_FCB <- dat$v_229-1
dat$zweikaempfeanteil_FCB <- dat$v_230-1
dat$torchancenanteil_FCB <- dat$v_231-1

alpha(dat[,c("spielanteil_FCB","zweikaempfeanteil_FCB","torchancenanteil_FCB")])

dat$anteil_FCB <- (dat$spielanteil_FCB+dat$zweikaempfeanteil_FCB+dat$torchancenanteil_FCB)/3

plot(density(dat[!is.na(dat$anteil_FCB),]$anteil_FCB))

t.test(dat$anteil_FCB~dat$fan_cl_spiel,var.eq=T,na.rm=T,paired=FALSE)
ddply(dat[!is.na(dat$anteil_FCB),],.(fan_cl_spiel),summarise,
      mean_anteil_FCB = mean(anteil_FCB),
      sd_anteil_FCB = sd(anteil_FCB),
      n = length(anteil_FCB))

cohens_d_unpaired(2.8941,31,20)

dd<-ddply(dat[!is.na(dat$anteil_FCB),],.(fan_cl_spiel),summarise,
              mean=mean(anteil_FCB),
              low=mean(anteil_FCB)-se(anteil_FCB),
              upper=mean(anteil_FCB)+se(anteil_FCB))

dd$team <- "FCB"
dd
dd <- rbind(dd,
            data.frame(fan_cl_spiel="BVB",mean=10-5.043011,low=NA,upper=NA,team="BVB"))
dd <- rbind(dd,
            data.frame(fan_cl_spiel="FCB",mean=10-5.683333,low=NA,upper=NA,team="BVB"))

# Prozent Werte berechnen
dd$mean <- dd$mean*10
dd$low <- dd$low*10
dd$upper <- dd$upper*10


#Plots
dd$fan_cl_spiel <- factor(dd$fan_cl_spiel,levels=c("BVB","FCB"))
dd$fan_cl_spiel <- revalue(dd$fan_cl_spiel,replace=c("BVB"="BVB Fans","FCB"="FCB Fans"))

ggplot(dd,aes(fan_cl_spiel,y=mean,ymin=low,ymax=upper,fill=team))+
  geom_bar(stat="identity")+
  geom_errorbar(width=.1,size=1)+
  scale_fill_manual(values = c("goldenrod2","darkred"))+
  theme_bw(base_size = 20)+
  xlab("Fan")+
  ylab("Contribution to Game [in %]")+
  labs(fill="Team")+
  annotate("text",x=2,y=25,label="56.8%",size=8)+
  annotate("text",x=2,y=75,label="43.2%",size=8)+
  annotate("text",x=1,y=25,label="50.4%",size=8)+
  annotate("text",x=1,y=75,label="49.6%",size=8)+
  theme(axis.title.x=element_blank())


#####
library(boot)
diffsamplemeans <- function(x, d) {
  return(mean(sample(x$var1,replace=T)) - mean(sample(x$var2,replace=T)))
}
b <- boot(list("var1"=dat[dat$fan_cl_spiel=="FCB"&!is.na(dat$anteil_FCB),]$anteil_FCB,
               "var2"=dat[dat$fan_cl_spiel=="BVB"&!is.na(dat$anteil_FCB),]$anteil_FCB),
          diffsamplemeans,
          R=100000)
plot(b)
boot.ci(b,bconf=0.95, type=c("basic", "perc", "norm", "bca"))
