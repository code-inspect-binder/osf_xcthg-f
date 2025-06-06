# Memory and confidence analysis
#
# last mod: 2015-12-07 mh
# last mod: 2016-02-05 mh
# last mod: 2016-02-06 mh

#- Analysis memory items

library(lme4)
library(lmerTest)
library(stringr)
library(FNN)
library(sciplot)
library(car)
library(LMERConvenienceFunctions)
library(plyr)
library(ez)
library(readr)
library(PsychHelperFunctions) # available at https://github.com/markushuff/PsychHelperFunctions
library(ggplot2)
library(gridExtra)
library(tidyr)

dat <- read.csv("memoryBothTestTimes.csv")
# alt und pot fehlkodiert
dat$fan <- NULL

#Anzahl VPn erste und zweiter Testzeitpunkt
tmp <- aggregate(PC ~ testTime + participant, dat, length)
aggregate(testTime ~ participant, tmp, length)
tmp[tmp$participant %in% c(42,45),]
#27 37 46

scenes <- read.csv("scenes.csv")

# Abstand Memoryitems zur jeweils n?chsten "ALL"-Eventgrenze hinzuf?gen
#- ACHTUNG: Gemessen am Ende des Ged?chtnistest-Videos! K?nnte auch ?berlegen, mitte zu nehmen, also alle zeitMemoryItemSekunden -3.5 zu rechnen
eb_times_1half_ALL_95 <- c(299.58,574.81,590.42,760.53,782.85,839.16,944.75,1114.40,1264.50,1282.78,1541.97,1739.74,1784.92,2084.10,2162.60,2190.57,2335.26,2479.40,2537.56)
eb_times_2half_ALL_95 <- c(802.05,845.17,997.52,1292.56,1354.84,1354.99,1458.07,1587.99,1823.28,1862.13,2246.27,2429.27,2516.57,2603.72,2759.43)
scenes$ZeitMemoryItem <- as.character(scenes$ZeitMemoryItem)
scenes$ZeitMemoryItemSekunden <- as.numeric(substr(scenes$ZeitMemoryItem,0,str_locate(scenes$ZeitMemoryItem, ":")[,1]-1)) * 60 +
  as.numeric(substr(scenes$ZeitMemoryItem,str_locate(scenes$ZeitMemoryItem, ":")[,1]+1,nchar(scenes$ZeitMemoryItem)))
scenes <- scenes[!is.na (scenes$ZeitMemoryItemSekunden),]

scenes$eb_dist_sekunden <- NA
scenes$eb_dist_sekunden[scenes$halftime==1] <- knnx.dist(eb_times_1half_ALL_95,scenes$ZeitMemoryItemSekunden[scenes$halftime==1],k=1)
scenes$eb_dist_sekunden[scenes$halftime==2] <- knnx.dist(eb_times_2half_ALL_95+45*60,scenes$ZeitMemoryItemSekunden[scenes$halftime==2],k=1)


dat <- merge(dat,scenes,by=c("Movie"))

dat$testTime <- factor(dat$testTime)
dat$participant <- factor(dat$participant)

# Echte Fanzugehörigkeit hinzufügen
# Nur die echten Fans! (Das sind 10 BVB und 10 FCB Fans, Rest im Eye-Tracking erhoben)
fans <- read.csv2("VPFan-Liste060214.csv")
fans$participant <- fans$Sitzplatz
fans$Sitzplatz <- NULL
dat <- merge(dat,fans)
tmp <- aggregate(Movie ~ participant+fan_orig,dat,length)
aggregate(participant~fan_orig, tmp, length)
tmp <- aggregate(Movie ~ participant+fan_orig+eyetracking,dat,length)
aggregate(participant~fan_orig+eyetracking, tmp, length)
dat$realFan <- FALSE
dat$realFan[as.character(dat$fan_orig) == as.character(dat$fan_cl_spiel)] <- TRUE
tmp <- aggregate(Movie ~ participant+fan_cl_spiel+realFan+eyetracking,dat,length)
aggregate(participant~fan_cl_spiel+realFan+eyetracking, tmp, length)
tmp <- aggregate(Movie ~ participant+fan_cl_spiel+realFan,dat,length)
aggregate(participant~fan_cl_spiel+realFan, tmp, length)


#------
#- Init: Nur Items, die zu T1 und T2 getestet wurden
#------

#- testweise nur die Items, die zu beiden Testzeitpunkten vorkamen
dat <- dat[dat$Movie %in% unique(dat$Movie[dat$testTime == 1]),]

#-------
#- Wie viele Test Items?
dat$count <- 1
unique(dat$Movie)

aggregate(count~team+ball_pos_change+participant,dat[dat$participant==1,],length)

#--------------
#------
#- Allgemeiner EB Effekt, Alle Versuchspersonen, EB als Zeitabstand (nur Items mit Abstand < 200 Sekunden, Box Plot Kriterium)
#-
#- --> EB Items werden besser erinnert als Non-EB Items
#- --> Gibt es Faneffekte, das es stärker für BVB als FCB ist? oder Artefakt der Ausreißer-Items?
#------
#--------------

#- Wie sind EBs um Gedächtnisitems verteilt, d.h. wie nah sind EBs so an den Gedächtnisitems?
tmp <- aggregate(PC ~ eb_dist_sekunden + Movie, data=dat, FUN=length)
hist(tmp$eb_dist_sekunden)

boxplot(tmp$eb_dist_sekunden)
length(tmp$eb_dist_sekunden)
tmp <- tmp[tmp$eb_dist_sekunden<200,]
length(tmp$eb_dist_sekunden)

dat <- dat[dat$eb_dist_sekunden<200,]


tmp <- dat

# Distance to next event boundary in tertiles

tmp$eb_dist_factor <- cut(tmp$eb_dist_sekunden, quantile(tmp$eb_dist_sekunden,probs=c(0,0.33,.67,1.0)),include.lowest = TRUE)
#ddply(tmp,.(fan_cl_spiel,team,eb_dist_factor,testTime,participant),summarize,PC=length(PC),konfidenz=length(konfidenz))
tmp <- ddply(tmp,.(fan_cl_spiel,team,eb_dist_factor,testTime,participant),summarize,PC=mean(PC),konfidenz=mean(konfidenz))

tmp <- tmp[tmp$participant!=42&tmp$participant!=45,] # VP ausschließen, die keine zweite Messung im memory test haben

# Wie viele VPN? 56; 32 BVB + 24 FCB
count(tmp$participant)

count(tmp$fan_cl_spiel)

ez.mod <- ezANOVA(
  data = tmp,
  wid = participant,
  dv = PC,
  within = .(team,eb_dist_factor,testTime),
  between = .(fan_cl_spiel),
  detailed = TRUE
  )

ezANOVA.pes(ez.mod)$ANOVA
write_csv(ezANOVA.pes(ez.mod)$ANOVA,"memory_anova_pc.csv")

ez.mod <- ezANOVA(
  data = tmp[tmp$testTime==1,],
  wid = participant,
  dv = PC,
  within = .(team,eb_dist_factor),
  between = .(fan_cl_spiel),
  detailed = TRUE
)
ezANOVA.pes(ez.mod)

ez.mod <- ezANOVA(
  data = tmp[tmp$testTime==2,],
  wid = participant,
  dv = PC,
  within = .(team,eb_dist_factor),
  between = .(fan_cl_spiel),
  detailed = TRUE
)
ezANOVA.pes(ez.mod)


ezDesign(
  data = tmp,
  x = participant,
  y = testTime
)


#########
# Confidence
#########

ez.mod <- ezANOVA(
  data = tmp,
  wid = participant,
  dv = konfidenz,
  within = .(team,eb_dist_factor,testTime),
  between = .(fan_cl_spiel),
  detailed = TRUE
)
ezANOVA.pes(ez.mod)
write_csv(ezANOVA.pes(ez.mod)$ANOVA,"memory_anova_confidence.csv")


library(plyr)
ddply(tmp,.(team,fan_cl_spiel),summarise,meam_Performance=mean(PC),se_Performance=se(PC))

ddply(tmp,.(team,fan_cl_spiel),summarise,mean_Confidence=mean(konfidenz),se_Confidence=se(konfidenz))

##############
## Plot Figure 2
###############


dat_fig_2_PC <-
  ddply(
    tmp,.(eb_dist_factor,team,testTime),summarize,mean = mean(PC),se = se(PC)
  )
dat_fig_2_PC$measure <- "Proportion Correct"

dat_fig_2_PC_overall <-
  ddply(
    tmp,.(eb_dist_factor,testTime),summarize,mean = mean(PC),se = se(PC)
  )
dat_fig_2_PC_overall$measure <- "Proportion Correct"
dat_fig_2_PC_overall$team <- "Aggregated"

dat_fig_2_PC <- rbind(dat_fig_2_PC,dat_fig_2_PC_overall)

dat_fig_2_confidence <-
  ddply(
    tmp,.(eb_dist_factor,team,testTime),summarize,mean = mean(konfidenz),se = se(konfidenz)
  )
dat_fig_2_confidence$measure <- "Confidence"

dat_fig_2_confidence_overall <-
  ddply(
    tmp,.(eb_dist_factor,testTime),summarize,mean = mean(konfidenz),se = se(konfidenz)
  )
dat_fig_2_confidence_overall$measure <- "Confidence"
dat_fig_2_confidence_overall$team <- "Aggregated"

dat_fig_2_confidence <- rbind(dat_fig_2_confidence,dat_fig_2_confidence_overall)


dat_fig_2_PC$testTime <- revalue(dat_fig_2_PC$testTime,replace = c("1"="Immediate Test",
                                                                   "2"="Delayed Test"))
dat_fig_2_PC$team <- factor(dat_fig_2_PC$team,levels=c("BVB Items","FCB Items","Aggregated"))
dat_fig_2_confidence$team <- factor(dat_fig_2_confidence$team,levels=c("BVB Items","FCB Items","Aggregated"))

dat_fig_2_confidence$testTime <- revalue(dat_fig_2_confidence$testTime,replace = c("1"="Immediate Test",
                                                                   "2"="Delayed Test"))

p1 <-
  ggplot(dat_fig_2_PC,aes(x = eb_dist_factor,y = mean,color = team, group=team)) +
  geom_point(size=2) +
  geom_line(size=2)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),width = .2,size=2) +
  facet_grid( ~ testTime, scale = "free") +
  scale_color_manual(values = c("lightgoldenrod1","indianred1","black"))+
  theme_bw(base_size = 20)+
  theme(legend.position="top", legend.direction="horizontal") +
  theme(axis.title.x=element_blank(),axis.title.x=element_blank())+
  labs(y="Proportion Correct",color="")

p2 <-
  ggplot(dat_fig_2_confidence,aes(x = eb_dist_factor,y = mean,color = team,group=team)) +
  geom_point(size=2) +
  geom_line(size=2)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),width = .2,size=2) +
  facet_grid( ~ testTime, scale = "free") +
  scale_color_manual(values = c("lightgoldenrod1","indianred1","black"),guide=FALSE)+
  theme_bw(base_size = 20)+
  labs(x="Distance to Event Boundary [sec]",y="Confidence")
  

grid.arrange(p1,p2)

##############
# Export data for Bayes factor analysis
##############

# 1. PC; ANOVA design

head(tmp)

dat_all_agg <- tmp
dat_all_agg$konfidenz <- NULL
dat_all_agg_wide <- dat_all_agg %>% unite(team_eb_dist_factor_testTime,team,eb_dist_factor,testTime) %>% spread(key=team_eb_dist_factor_testTime,value = PC)
write_csv(dat_all_agg_wide,"PC_wide_cl.csv")

# 2. PC; t-test design

head(tmp)

dat_all_agg <- aggregate(PC~participant+fan_cl_spiel,tmp,mean)
write_csv(dat_all_agg,"PC_wide_fan_cl.csv")

