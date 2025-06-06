# Comparision: prediction of gaze of own fan group (= coherence, NSS) vs. prediction gaze of other fan group

library(stringr)
library(plyr)
library(readr)
library(readxl)

#--
#-  1.) Loading normal NSS data
#--

# Verschieden lange qf0...qfn Variablen zu einheitlichen Variablen vereinen (bei BVB, FCB, all und Halbzeiten ja unterschiedliche Anzahl VPn drin)
# jeden Datensatz auf maximale Anzhal einzel-nss Werte erweitern, damit datenframes zusammenpacken kann
calculate_qf_and_tmp_nss_values <- function(tmp)
{
  tmp$q_num <- 0
  tmp$q_num_ist_null <- 0
  tmp$q_sum <- 0
  for (var in names(tmp)[!is.na(str_locate(names(tmp),"qf")[,1])]) # alle Variablen, die "qf" enthalten
  {
    tmp$q_num <- tmp$q_num + 1
    tmp$q_num_ist_null[tmp[,var] == 0] <- tmp$q_num_ist_null[tmp[,var] == 0] + 1
    tmp$q_sum <- tmp$q_sum + tmp[,var]
    tmp[,var] <- NULL
  }
  for (vp in 0:8)
  {
    tmp[,paste("tmp_nss",vp,sep="")] <- NA
    varname <- 
    if (paste("nss",vp,sep="") %in% names(tmp))
    {
      tmp[,paste("tmp_nss",vp,sep="")] <- tmp[,paste("nss",vp,sep="")]
      tmp[,paste("nss",vp,sep="")] <- NULL
    }
  } 
#  for (var in names(tmp)[!is.na(str_locate(names(tmp),"nss[0-9]+")[,1])]) # alle Variablen, die "nns" und zahl enthalten, also f?r einzel-vpn
#  {
#    tmp[,var] <- NULL
#  }
  return(tmp)
}

wd <- getwd()
paths <- dir(path = paste(wd,"/nss-values-xy-grid",sep=""), pattern = "*.txt", full = T) 
dat.raw <- lapply(paths, read.table, header=T)

#- Generate data frame 
dat <- calculate_qf_and_tmp_nss_values(data.frame(dat.raw[1]))

for (i in 2:length(dat.raw))
{
  dat <- rbind(dat,calculate_qf_and_tmp_nss_values(data.frame(dat.raw[i])))
}

dat <- dat[dat$t <= 45*60*1000*1000,]

# Wegen Programmabbruch NSS Erzeugung neu gestartet: Pr?fung, ob alle Daten vorhanden und auch nur genau 1 mal
tmp <- expand.grid(FAN=unique(dat$FAN),
                   HALF=unique(dat$HALF),
                   t=seq(0,45*60*1000*1000,40*1000))
length(merge(tmp,dat)$t) == length(tmp$t)
length(dat$t) == length(tmp$t)

# Zeitangabe in Sekunden ?ndern
dat$t <- dat$t / 1000 / 1000

dat.nss <- dat

#--
#-  2.) Predict by other group laden
#--

wd <- getwd()
paths <- dir(path = paste(wd,"/nss-predict-by-other-team-xy-grid",sep=""), pattern = "*.txt", full = T) 
dat.raw <- lapply(paths, read.table, header=T)

#- Generate data frame 
dat <- calculate_qf_and_tmp_nss_values(data.frame(dat.raw[1]))

for (i in 2:length(dat.raw))
{
  dat <- rbind(dat,calculate_qf_and_tmp_nss_values(data.frame(dat.raw[i])))
}

dat <- dat[dat$t <= 45*60*1000*1000,]

# Wegen Programmabbruch NSS Erzeugung neu gestartet: Pr?fung, ob alle Daten vorhanden und auch nur genau 1 mal
tmp <- expand.grid(FAN=unique(dat$FAN),
                   HALF=unique(dat$HALF),
                   t=seq(0,45*60*1000*1000,40*1000))
length(merge(tmp,dat)$t) == length(tmp$t)
length(dat$t) == length(tmp$t)

# Zeitangabe in Sekunden ?ndern
dat$t <- dat$t / 1000 / 1000

dat.predict_other <- dat



dat.nss$FAN_PREDICTOR <- dat.nss$FAN
dat.predict_other$n_reference <- NA

tmp1 <- dat.nss[,order(names(dat.nss))]
tmp2 <- dat.predict_other[,order(names(dat.predict_other))]

dat.all <- rbind(tmp1,tmp2)



#------------------
#---------
#- 1.) Plot: Entwicklung NSS ?ber Zeit hinweg -> predict self vs other
#- 2.) Mittlere NSS BVB vs FCB (predict self vs other) ?ber erste Halbzeit hinweg
#---------
#------------------

dat <- dat.all

#---------
#- Filter: Nur "gute" Daten betrachten
#---------

# Filter: mind f?r 4 VPn NSS berechnet und daraus Mittelwert f?r Gruppe gebildet
length(dat[,1])
dat <- dat[dat$n_subjects - dat$n_nan >= 4,]
length(dat[,1])

# Filter: Fixation Map Berechnung basiert auf mind. 4 VPn (Beachten: sagt nur, dass f?r 4 VPn im Zeitfenster um relevante Stelle Daten f?r Fixation Map Berechnung vorhanden waren. Sagt nicht unbedingt aus, dass Daten auch nah in Zeit am eigentlich untersuchten Zeitpunkt waren. K?nnten auch am Rand des Zeitfensters gelegen haben)
length(dat[,1])
dat <- dat[dat$q_num - dat$q_num_ist_null  >= 4,]
length(dat[,1])

(270004-263775)/270004

# Filter: Fixation Map Berechnung: mind 50% der vorhandenen VPn valide Daten
#length(dat[,1])
#dat <- dat[dat$q_num_ist_null / dat$q_num <= 0.5,]
#length(dat[,1])

# Filter: Fixation Map Berechnung: Von den VPn mit validen Daten gehen im Mittel mind. 10 Datenpunkte ein (Achtung: MITTEL! Bedeutet also nicht, dass von jeder VPn mind. 10 Datenpunkte eingegangen sind. Wenn das will, dann ?hnlich zu q_num_ist_null neue Variable einf?hren)
#length(dat[,1])
#dat <- dat[dat$q_sum / (dat$q_num-dat$q_num_ist_null) >= 10,]
#length(dat[,1])

#- Overlay Plot
# Zum Malen auf TRUE setzen oder einfach inneren Teil ausf?hren
if (TRUE)
{
  for (min in 0:50)
  {
    for (half in levels(factor(dat$HALF)))
    {
      for (fan in levels(factor(dat$FAN)))
      {
        png(paste("plots/overlay_predict_",fan,"_self_black_vs_other_blue_",half,"_",min,"_",min+1,".png", sep=""))
        print(plot(NA, main=paste(half,". Halbzeit: Overlay (",min,"-",min+1,")",sep=""), xlim=c(0,60), ylim=c(min(dat$nss_nanmean),max(dat$nss_nanmean)), ylab="NSS")      )
        color <- "green"
        symbol <- 1
        for (fan_predictor in levels(factor(dat$FAN_PREDICTOR)))
        {
          if(fan == fan_predictor){color="black"; symbol <- 2}
          if(fan != fan_predictor){color="blue"; symbol <- 3}
          tmp <- dat[dat$t > min*60 & dat$t < (min+1)*60 & dat$FAN == fan & dat$HALF == half & dat$FAN_PREDICTOR == fan_predictor,]
          tmp$t <- (tmp$t - min*60)
          if (length(tmp$t) > 0)
          {
            print(points(nss_nanmean ~ t, tmp, col=color, pch=symbol))
          }
        }
        dev.off()
      }
    }  
  }
}

#- Mittleres NSS BVB vs. FCB (self vs. other)

dat$FAN_PREDICTOR <- as.character(dat$FAN_PREDICTOR)
dat$FAN_PREDICTOR[dat$FAN != dat$FAN_PREDICTOR] <- "Other Fans"
dat$FAN_PREDICTOR[dat$FAN == dat$FAN_PREDICTOR] <- "Same Fans"
dat$FAN_PREDICTOR <- factor(dat$FAN_PREDICTOR, levels=c("Same Fans","Other Fans"))

#--
# in 40 ms Schritten
#--

#--
# Plot
#--

if (TRUE)
{
  png("nss-BVB.png",width=900,height=300)
  par(mar=c(4.5, 4.5, 0.1, 0.1))
  plot(NA, xlim=c(0,max(dat$t)), ylim=c(0,max(max(dat$nss_nanmean,na.rm=T),20.5)),xlab="Time in Seconds",ylab="Gaze Coherence (NSS)", cex.lab=1.8, cex.axis=1.8)
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",], points(nss_nanmean ~ t, col="grey")) # #15489022
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",], points(nss_nanmean ~ t, col="goldenrod2")) # #ff660022
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="white", lwd=7, lty="solid"))
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="white", lwd=7, lty="solid"))
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="grey", lwd=4, lty="solid"))
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="goldenrod2", lwd=4, lty="solid"))
  legend(0, 20, c("BVB Fans","FCB Fans"), col=c("goldenrod2","grey"), lty="solid", lwd=4, title="Reference Group", cex=1.6)
  title("BVB Fans", line = -2,cex.main=2)
  dev.off()
  
  png("nss-FCB.png",width=900,height=300)
  par(mar=c(4.5, 4.5, 0.1, 0.1))
  plot(NA, xlim=c(0,max(dat$t)), ylim=c(0,max(max(dat$nss_nanmean,na.rm=T),20.5)),xlab="Time in Seconds",ylab="Gaze Coherence (NSS)", cex.lab=1.8, cex.axis=1.8)
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Other Fans",], points(nss_nanmean ~ t, col="grey")) # #15489022
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Same Fans",], points(nss_nanmean ~ t, col="darkred")) # #ff660022
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="white", lwd=7, lty="solid"))
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="white", lwd=7, lty="solid"))
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="grey", lwd=4, lty="solid"))
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="darkred", lwd=4, lty="solid"))
  legend(0, 20, c("FCB Fans","BVB Fans"), col=c("darkred","grey"), lty="solid", lwd=4, title="Reference Group", cex=1.6)
  title("FCB Fans", line = -2,cex.main=2)
  dev.off()
}


if (TRUE)
{
  png("nss-BVB_line.png",width=900,height=300)
  par(mar=c(4.5, 4.5, 0.1, 0.1))
  plot(NA, xlim=c(0,max(dat$t)), ylim=c(0,max(max(dat$nss_nanmean,na.rm=T),20.5)),xlab="Time in Seconds",ylab="Gaze Coherence (NSS)", cex.lab=1.8, cex.axis=1.8)
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="grey", lwd=4, lty="solid"))
  with(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="goldenrod2", lwd=4, lty="solid"))
  legend(0, 20, c("BVB Fans","FCB Fans"), col=c("goldenrod2","grey"), lty="solid", lwd=4, title="Reference Group", cex=1.6)
  title("BVB Fans", line = -2,cex.main=2)
  dev.off()
  
  png("nss-FCB_line.png",width=900,height=300)
  par(mar=c(4.5, 4.5, 0.1, 0.1))
  plot(NA, xlim=c(0,max(dat$t)), ylim=c(0,max(max(dat$nss_nanmean,na.rm=T),20.5)),xlab="Time in Seconds",ylab="Gaze Coherence (NSS)", cex.lab=1.8, cex.axis=1.8)
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Other Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="grey", lwd=4, lty="solid"))
  with(dat[dat$FAN=="FCB" & dat$FAN_PREDICTOR=="Same Fans",], lines(lowess(nss_nanmean ~ t,f=0.001), col="darkred", lwd=4, lty="solid"))
  legend(0, 20, c("FCB Fans","BVB Fans"), col=c("darkred","grey"), lty="solid", lwd=4, title="Reference Group", cex=1.6)
  title("FCB Fans", line = -2,cex.main=2)
  dev.off()
}

BVB_BVB <- lowess(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",]$nss_nanmean ~ dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$t,f=0.001)
BVB_FCB <- lowess(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$nss_nanmean ~ dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$t,f=0.001)
FCB_BVB <- lowess(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$nss_nanmean ~ dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$t,f=0.001)
FCB_FCB <- lowess(dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Same Fans",]$nss_nanmean ~ dat[dat$FAN=="BVB" & dat$FAN_PREDICTOR=="Other Fans",]$t,f=0.001)

dat_nss_lowess <- data.frame(BVB_BVB)
dat_nss_lowess <- rename(dat_nss_lowess,c("x"="time",y="BVB_BVB"))
dat_nss_lowess <- cbind(dat_nss_lowess,BVB_FCB[2],FCB_BVB[2],FCB_FCB[2])
names(dat_nss_lowess)[c(3:5)] <- c("BVB_FCB","FCB_BVB","FCB_FCB")
write_csv(dat_nss_lowess,"nss.csv")

#--
# Statistische Auswertung auf VPn-Ebene
#
# 1.) je Zeitpunkt f?r jede VPn NSS Wert bestimmen auf Basis eigene vs. fremde Fangruppe
# 2.) je Zeitpunkt und VPn Differenz der beiden Werte bilden (wieviel sagt eigene Fangruppe mehr den Blick vorher als andere Fangruppe)
# 3.) Differenzen ?ber zeitpunkte aggregieren, also VPn Ebene
# 4.) unterscheiden sich die Differenzen signifikant von 0?
#--
# :
# 
tmp <- dat[,c("t","FAN","FAN_PREDICTOR","tmp_nss0","tmp_nss1","tmp_nss2","tmp_nss3","tmp_nss4","tmp_nss5","tmp_nss6","tmp_nss7","tmp_nss8")]
tmp <- reshape(tmp, idvar=c("t","FAN","FAN_PREDICTOR"), varying=c("tmp_nss0","tmp_nss1","tmp_nss2","tmp_nss3","tmp_nss4","tmp_nss5","tmp_nss6","tmp_nss7","tmp_nss8"),v.names=c("nss"),times=0:8,timevar="vp",direction="long")
# VPn entfernen, die es bei BVB gar nicht gab (6-8)
length(tmp$FAN)
tmp <- tmp[!(tmp$FAN=="BVB" & tmp$vp >= 6),]
length(tmp$FAN)
# Zeitpunkte entfernen, bei denen es fehlende NSS Messwerte gibt
tmp <- tmp[!is.na(tmp$nss),]
length(tmp$FAN)
tmp$vp <- paste0(tmp$FAN,tmp$vp)
tmp$FAN <- NULL # BVB und FCB zusammen schmei?en, daher nicht mehr ben?tigt
tmp <- reshape(tmp, idvar=c("t","vp"), timevar="FAN_PREDICTOR",direction="wide")
tmp$nss_same_minus_other <- tmp$"nss.Same Fans" - tmp$"nss.Other Fans"
length(tmp$nss_same_minus_other)
tmp <- tmp[!is.na(tmp$nss_same_minus_other),] # Zeitpunkte entfernen, bei denen f?r Same Fans oder Other Fans kein NSS Wert berechnet werden konnte
length(tmp$nss_same_minus_other)

# Daten noch in Analyse: 896938 * 2 (da je Zeitpunkt 2 Werte)
# Vor Ausschluss NA Daten waren: 1982409

# Somit Anteil Daten noch enthalten:
896938 * 2 / 1982409

# Auf VPn Ebene agregieren, wieviel im Mittel Same oder Other je Zeitpunkt besser vorhergesagt hat
tmp <- aggregate(nss_same_minus_other ~ vp, tmp, mean)
t.test(tmp$nss_same_minus_other)
mean(tmp$nss_same_minus_other)
sd(tmp$nss_same_minus_other)

# Export data for bayes analysis
write_csv(tmp,"t_test_0_nss_cl.csv")
