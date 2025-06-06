# Segmentation agreement calculation
#
# Idea: Den Segmentation Magnitude Wert eines jeden Tastendrucks einer 
# jeden VP aus den jeweiligen SegMag Objekten raussuchen und gucken ob 
# sich dieser in Bezug auf eigene und fremde Fangruppe unterscheidet.
# Diese Berrechnung ist OHNE Autokorrelation, d.h. die jeweilige VPN wird 
# bei der Berechnung des SegMag Wertes nicht berücksichtigt.

library(segmag)
library(sciplot)
library(plyr)
library(ez)
library(PsychHelperFunctions) # available at https://github.com/markushuff/PsychHelperFunctions
library(readr)
library(tidyr)

#- global parameters
game_start_1 <- 1369507587.900 # sec
game_start_2 <- 1369511215.700 # sec
gauss_sd <- 1.000 # sec
gauss_offset <- 0  #-0.800 # sec

#- read in data
dat <- read.csv("joint_segmentation_data.csv")
dat$vp <- as.numeric(as.character(dat$vp))

#- Fanzugehoerigkeit
fan <- read.csv2("VPFan-Liste060214.csv")
fan$vp <- fan$Sitzplatz
fan$Sitzplatz <- NULL
length(dat$vp)
dat <- merge(dat, fan, by="vp")
length(dat$vp)

#- Faktorisieren
dat$vp <- factor(dat$vp)

#- split for half time; ACHTUNG: Nur grob! Nicht exakte Spielzeit beachtet -> das erfolgt unten
dat_1half <- dat[(dat$global_time >= game_start_1 & dat$global_time <=
                  (game_start_2-5*60)),c(1,4,7)]
dat_2half <- dat[(dat$global_time >= game_start_2),c(1,4,7)]

dat_1half$time <- dat_1half$global_time - game_start_1
dat_2half$time <- dat_2half$global_time - game_start_2

#--------
#--------
# 1. Halbzeit
#--------
#--------

add_seg_agreement <- function(data_half, time_end_half) {
  cur_half <- data_half[data_half$time < time_end_half,]
  
  #- Variable anlegen
  cur_half$seg_agree_own <- NA
  cur_half$seg_agree_other <- NA
  
  ## seg agreement
  
  for(v in unique(cur_half$vp))
  {
    print(v)
    
    cur_fan <- fan$fan_cl_spiel[fan$vp==v]
    if (length(cur_fan) != 1) stop("invalid length of 'cur_van'")
    
    ifelse(
      cur_fan == "BVB",
      {
        own_team <- "BVB"
        other_team <- "FCB"
      }
      ,
      ifelse(
        cur_fan == "FCB",
        {
          own_team <- "FCB"
          other_team <- "BVB"
        }
        ,
        stop("invalid fan group!")
      )
    )
    
    segmag_own <- segmag(vp,time,cur_half[cur_half$fan_cl_spiel==own_team & cur_half$vp!=v,],time_min=0,time_max=time_end_half,gauss_offset=gauss_offset,gauss_sd=gauss_sd)
    #- Normalisieren der Segmentation Magnitude
    segmag_own$data$segmentation_magnitude <- segmag_own$data$segmentation_magnitude/length(unique(segmag_own$ids))
    
    segmag_other <- segmag(vp,time,cur_half[cur_half$fan_cl_spiel==other_team & cur_half$vp!=v,],time_min=0,time_max=time_end_half,gauss_offset=gauss_offset,gauss_sd=gauss_sd)
    #- Normalisieren der Segmentation Magnitude
    segmag_other$data$segmentation_magnitude <- segmag_other$data$segmentation_magnitude/length(unique(segmag_other$ids))
    
    
    for(t in cur_half$time[cur_half$vp==v])
    { 
      print(t)
      
      cur_half$seg_agree_own[cur_half$vp==v & cur_half$time==t] <- segmag_own$data$segmentation_magnitude[round(segmag_own$data$time,2) == round(t,2)]
      cur_half$seg_agree_other[cur_half$vp==v & cur_half$time==t] <- segmag_other$data$segmentation_magnitude[round(segmag_other$data$time,2) == round(t,2)]
    } 
  }
  
  return(cur_half)
}

#--------
#--------
# 1. Halbzeit
#--------
#--------

dat_1half <- add_seg_agreement(dat_1half, 45.5 * 60) # 45.5: Bis 1/2 Minute nach Abpfiff

#--------
#--------
# 2. Halbzeit
#--------
#--------

dat_2half <- add_seg_agreement(dat_2half, 49.5 * 60) # 49.5: Bis 1/2 Minute nach Abpfiff

#--------
#--------
# Ueber beide Halbzeiten hinweg berechnen
#--------
#--------

dat <- rbind(dat_1half,dat_2half)

dat$seg_own_minus_other <- dat$seg_agree_own - dat$seg_agree_other
dat <- aggregate(seg_own_minus_other ~ vp, dat, mean)
t.test(dat$seg_own_minus_other, mu=0)
mean(dat$seg_own_minus_other)
sd(dat$seg_own_minus_other)

library(BayesFactor)
bf <- ttestBF(dat$seg_own_minus_other,nullInterval=c(0,Inf))
1/bf[1]
