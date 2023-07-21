### packages
{library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)}


## Read in sleep data
setwd("~/Library/CloudStorage/OneDrive-Colostate/CSU/GrassRats/GrassRats/Animal_data/sleepData/SleepBout_histogram_files")
#Phase 1 animals DAILY SLEEP TOTALS ####
anim_ID_phase1 = read.csv("Chamber_Animal_IDs.csv")
anim_ID_bothPhases = read.csv("Chamber_Animal_IDs_Sex_bothPhases.csv")

## 4 week photoperiod total daily sleep ####

#Phase1
fourWeekStart_long = read.csv("4WeekPhotoperiod_08Apr2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekStart_long = fourWeekStart_long[13:24,]

fourWeekStart_short = read.csv("4WeekPhotoperiod_08Apr2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekStart_short = fourWeekStart_short[1:12,]

fourWeekEnd_long = read.csv("4WeekPhotoperiod_24Apr2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekEnd_long = fourWeekEnd_long[13:24,]

fourWeekEnd_short = read.csv("4WeekPhotoperiod_24Apr2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekEnd_short = fourWeekEnd_short[1:12,]

fourWeekMid_long = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekMid_long = fourWeekMid_long[13:24,]
    
fourWeekMid_short = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")                  

fourWeekMid_short = fourWeekMid_short[1:12,]

#Phase2

fourWeekStart_phase2combined = read.csv("../Phase2_SleepBout_histogram_files/Phase2_4WeekPhotoperiod_03Jul2019_combined.csv")
fourWeekMid_phase2combined = read.csv("../Phase2_SleepBout_histogram_files/Phase2_4WeekPhotoperiod_22Jul2019_combined.csv")

fourWeekStart_Neutral2 = fourWeekStart_phase2combined[-4,] # remove G28
fourWeekStart_Neutral2 = fourWeekStart_phase2combined[12:22,] #split neutral animals

fourWeekStart_Neutral2 = fourWeekStart_Neutral2 %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8", "PERCENTSLEEPDAY9", "PERCENTSLEEPDAY10", 
         "PERCENTSLEEPDAY11", "PERCENTSLEEPDAY12", "PERCENTSLEEPDAY13", "PERCENTSLEEPDAY14",
         "PERCENTSLEEPDAY15", "PERCENTSLEEPDAY16", "PERCENTSLEEPDAY17", "PERCENTSLEEPDAY18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_Neutral2 = fourWeekStart_Neutral2[,-(3:12)]# take last 8 days of data


fourWeekStart_Short2 = fourWeekStart_phase2combined[1:11,] #split short animals
fourWeekStart_Short2 = fourWeekStart_Short2 %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8", "PERCENTSLEEPDAY9", "PERCENTSLEEPDAY10", 
         "PERCENTSLEEPDAY11", "PERCENTSLEEPDAY12", "PERCENTSLEEPDAY13", "PERCENTSLEEPDAY14",
         "PERCENTSLEEPDAY15", "PERCENTSLEEPDAY16", "PERCENTSLEEPDAY17", "PERCENTSLEEPDAY18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_Short2 = fourWeekStart_Short2[,-(3:12)]# take last 8 days of data

fourWeekMid_Neutral2 = fourWeekMid_phase2combined[-4,] # remove G28
fourWeekMid_Neutral2 = fourWeekMid_phase2combined[12:22,] #split neutral animals
fourWeekMid_Neutral2 = fourWeekMid_Neutral2  %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekMid_Short2 = fourWeekMid_phase2combined[1:11,] #split short animals
fourWeekMid_Short2 = fourWeekMid_Short2  %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")


#Join last 2-weeks of 4 week acclimation data frames and caluclate average daily sleep
#for short and long photoperiod animals for phase 1 and phase 2 

#Phase 1 
lastTwo4Week_long = merge(fourWeekMid_long, fourWeekEnd_long, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_long) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")


lastTwo4Week_short = merge(fourWeekMid_short, fourWeekEnd_short, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_short) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                 "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                 "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")


#Phase 2 
lastTwo4Week_long2 = merge(fourWeekStart_Neutral2, fourWeekMid_Neutral2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_long2) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                 "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                 "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")

lastTwo4Week_short2 = merge(fourWeekStart_Short2, fourWeekMid_Short2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_short2) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                  "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                  "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")

##Merge Phase 1 and Phase 2 animals to match the anim_ID_bothPhases dataframe
DailySleepBothPhases = rbind(lastTwo4Week_short, lastTwo4Week_long, lastTwo4Week_short2, lastTwo4Week_long2)
write.csv(DailySleepBothPhases, "DailySleepBothPhases_FINAL.csv")
rownames(DailySleepBothPhases) = DailySleepBothPhases$GrassRat_ID
#anim_ID_bothPhases = anim_ID_bothPhases[-c(47,48),]


#match data frames by animal_IDS
intersection = match(anim_ID_bothPhases$GrassRat_ID, DailySleepBothPhases$GrassRat_ID)
DailySleepBothPhases_reorder = DailySleepBothPhases[intersection, ]

#melt data frame
DailySleepBothPhases2 = melt(DailySleepBothPhases_reorder, id.vars= c("GrassRat_ID", "TestDate"), 
                             measure.vars=c("SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                            "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                            "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14"))

DailySleepBothPhases2$Photoperiod = anim_ID_bothPhases$Photoperiod
DailySleepBothPhases2$Sugar_conc = anim_ID_bothPhases$Sugar_conc
DailySleepBothPhases2$Sugar = anim_ID_bothPhases$Sugar
DailySleepBothPhases2$ParentPair = anim_ID_bothPhases$ParentPair
DailySleepBothPhases2$Photoperiod = as.factor(DailySleepBothPhases2$Photoperiod)

#STATS
#LongPhotoperiod Mean and sd
Natural = DailySleepBothPhases2 %>% filter(Photoperiod == "Neutral")
mean(Natural$value)
sd(Natural$value)

Short = DailySleepBothPhases2 %>% filter(Photoperiod == "Short")
mean(Short$value)
sd(Short$value)

# mixed models
library(car)
DailySleepBothPhases2$Photoperiod = as.factor(DailySleepBothPhases2$Photoperiod)
DailySleepBothPhases2$Sugar = as.factor(DailySleepBothPhases2$Sugar)
fourWeekSleep = lmer(value~Photoperiod*Sugar + (1|GrassRat_ID), DailySleepBothPhases2, REML = TRUE)
qqp(resid(fourWeekSleep))
plot(density(resid(fourWeekSleep)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekSleep, type=2, ddf="Kenward-Roger")

#Normalize and rerun
library(bestNormalize)
bestobj = bestNormalize(DailySleepBothPhases2$value)
bestobj # ordernorm best
bestobj1 = orderNorm(DailySleepBothPhases2$value)
DailySleepBothPhases2$valueNorm = bestobj1$x.t

fourWeekSleep2 = lmer(valueNorm~Photoperiod + (1|GrassRat_ID), DailySleepBothPhases2, REML = TRUE)
qqp(resid(fourWeekSleep2))
plot(density(resid(fourWeekSleep2)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekSleep2, type=2, ddf="Kenward-Roger")

## SLEEP BOUTS ####

#Phase1
fourWeekEnd_longBouts = read.csv("4WeekPhotoperiod_24Apr2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekEnd_longBouts = fourWeekEnd_longBouts[13:24,]

fourWeekEnd_shortBouts = read.csv("4WeekPhotoperiod_24Apr2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekEnd_shortBouts = fourWeekEnd_shortBouts[1:12,]


fourWeekMid_longBouts = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6", "BOUTLENGTHNITE7", "BOUTLENGTHNITE8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekMid_longBouts = fourWeekMid_longBouts[13:24,]

fourWeekMid_shortBouts = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv")%>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6", "BOUTLENGTHNITE7", "BOUTLENGTHNITE8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekMid_shortBouts = fourWeekMid_shortBouts[1:12,]

#Phase2
fourWeekStart_phase2combined = read.csv("../Phase2_SleepBout_histogram_files/Phase2_4WeekPhotoperiod_03Jul2019_combined.csv")
fourWeekMid_phase2combined = read.csv("../Phase2_SleepBout_histogram_files/Phase2_4WeekPhotoperiod_22Jul2019_combined.csv")

fourWeekStart_longBouts2 = fourWeekStart_phase2combined[-4,] # remove G28
fourWeekStart_shortBouts2 = fourWeekStart_phase2combined[12:22,] #split neutral animals

fourWeekStart_longBouts2 = fourWeekStart_longBouts2 %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6", "BOUTLENGTHNITE7", "BOUTLENGTHNITE8", "BOUTLENGTHNITE9",
         "BOUTLENGTHNITE10", "BOUTLENGTHNITE11", "BOUTLENGTHNITE12", "BOUTLENGTHNITE13",
         "BOUTLENGTHNITE14", "BOUTLENGTHNITE15", "BOUTLENGTHNITE16", "BOUTLENGTHNITE17",
         "BOUTLENGTHNITE18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_longBouts2 = fourWeekStart_longBouts2[,-(3:12)]# take last 8 days of data


fourWeekStart_Short2 = fourWeekStart_phase2combined[1:11,] #split short animals
fourWeekStart_Short2 = fourWeekStart_Short2 %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6", "BOUTLENGTHNITE7", "BOUTLENGTHNITE8", "BOUTLENGTHNITE9",
         "BOUTLENGTHNITE10", "BOUTLENGTHNITE11", "BOUTLENGTHNITE12", "BOUTLENGTHNITE13",
         "BOUTLENGTHNITE14", "BOUTLENGTHNITE15", "BOUTLENGTHNITE16", "BOUTLENGTHNITE17",
         "BOUTLENGTHNITE18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_Short2 = fourWeekStart_Short2[,-(3:12)]# take last 8 days of data

fourWeekMid_Neutral2 = fourWeekMid_phase2combined[-4,] # remove G28
fourWeekMid_Neutral2 = fourWeekMid_phase2combined[12:22,] #split neutral animals
fourWeekMid_Neutral2 = fourWeekMid_Neutral2  %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekMid_Short2 = fourWeekMid_phase2combined[1:11,] #split short animals
fourWeekMid_Short2 = fourWeekMid_Short2  %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHNITE1","BOUTLENGTHNITE2",
         "BOUTLENGTHNITE3","BOUTLENGTHNITE4","BOUTLENGTHNITE5",
         "BOUTLENGTHNITE6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")


#Join last 2-weeks of 4 week acclimation data frames and caluclate average daily sleep
#for short and long photoperiod animals for phase 1 and phase 2 

#Phase 1 
lastTwo4Week_longBouts = merge(fourWeekMid_longBouts, fourWeekEnd_longBouts, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_longBouts) = c("GrassRat_ID", "TestDate", "BoutNight1","BoutNight2", "BoutNight3", "BoutNight4",
                                "BoutNight5", "BoutNight6", "BoutNight7", "BoutNight8", "BoutNight9", "BoutNight10",
                                "BoutNight11", "BoutNight12", "BoutNight13", "BoutNight14")

lastTwo4Week_shortBouts = merge(fourWeekMid_shortBouts, fourWeekEnd_shortBouts, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_shortBouts) = c("GrassRat_ID", "TestDate", "BoutNight1","BoutNight2", "BoutNight3", "BoutNight4",
                                     "BoutNight5", "BoutNight6", "BoutNight7", "BoutNight8", "BoutNight9", "BoutNight10",
                                     "BoutNight11", "BoutNight12", "BoutNight13", "BoutNight14")

#Phase 2 
lastTwo4Week_longBouts2 = merge(fourWeekStart_longBouts2, fourWeekMid_Neutral2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_longBouts2) = c("GrassRat_ID", "TestDate", "BoutNight1","BoutNight2", "BoutNight3", "BoutNight4",
                                      "BoutNight5", "BoutNight6", "BoutNight7", "BoutNight8", "BoutNight9", "BoutNight10",
                                      "BoutNight11", "BoutNight12", "BoutNight13", "BoutNight14")

lastTwo4Week_shortBouts2 =  merge(fourWeekStart_Short2, fourWeekMid_Short2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_shortBouts2) = c("GrassRat_ID", "TestDate", "BoutNight1","BoutNight2", "BoutNight3", "BoutNight4",
                                      "BoutNight5", "BoutNight6", "BoutNight7", "BoutNight8", "BoutNight9", "BoutNight10",
                                      "BoutNight11", "BoutNight12", "BoutNight13", "BoutNight14")

##Merge Phase 1 and Phase 2 animals to match the anim_ID_bothPhases dataframe
SleepBoutsBothPhases = rbind(lastTwo4Week_longBouts, lastTwo4Week_shortBouts, lastTwo4Week_longBouts2, 
                             lastTwo4Week_shortBouts2)
SleepBoutsBothPhases = SleepBoutsBothPhases[-40, ]

write.csv(DailySleepBothPhases, "SleepBoutsBothPhases_FINAL.csv")

#match data frames by animal_IDS
intersection2 = match(anim_ID_bothPhases$GrassRat_ID, SleepBoutsBothPhases$GrassRat_ID)
SleepBoutsBothPhases_reorder = SleepBoutsBothPhases[intersection2, ]


#melt data frame
SleepBoutsBothPhases2 = melt(SleepBoutsBothPhases_reorder, id.vars= c("GrassRat_ID", "TestDate"), 
                             measure.vars=c("BoutNight1","BoutNight2", "BoutNight3", "BoutNight4",
                                            "BoutNight5", "BoutNight6", "BoutNight7", "BoutNight8", "BoutNight9", "BoutNight10",
                                            "BoutNight11", "BoutNight12", "BoutNight13", "BoutNight14"))

SleepBoutsBothPhases2$Photoperiod = anim_ID_bothPhases$Photoperiod
SleepBoutsBothPhases2$Sugar_conc = anim_ID_bothPhases$Sugar_conc
SleepBoutsBothPhases2$Sugar = anim_ID_bothPhases$Sugar
SleepBoutsBothPhases2$ParentPair = anim_ID_bothPhases$ParentPair
SleepBoutsBothPhases2$Photoperiod = as.factor(SleepBoutsBothPhases2$Photoperiod)
SleepBoutsBothPhases2 = SleepBoutsBothPhases2 %>% na.omit()
#STATS
#LongPhotoperiod Mean and sd
Naturalbout = SleepBoutsBothPhases2 %>% filter(Photoperiod == "Neutral")
mean(Naturalbout$value)
sd(Naturalbout$value)

Short = SleepBoutsBothPhases2 %>% filter(Photoperiod == "Short")
mean(Short$value)
sd(Short$value)

# mixed models
library(car)
library(lme4)
library(lmerTest)
library(pbkrtest)
fourWeekBouts = lmer(value~Photoperiod + (1|GrassRat_ID), SleepBoutsBothPhases2, REML = TRUE)
qqp(resid(fourWeekBouts))
plot(density(resid(fourWeekBouts)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekBouts, type=2, ddf="Kenward-Roger")

#Normalize and rerun
library(bestNormalize)
bestobj2 = bestNormalize(SleepBoutsBothPhases2$value)
bestobj2 # ordernorm best
bestobj3 = orderNorm(SleepBoutsBothPhases2$value)
SleepBoutsBothPhases2$valueNorm = bestobj3$x.t

fourWeekBouts2 = lmer(valueNorm~Photoperiod + (1|GrassRat_ID), SleepBoutsBothPhases2, REML = TRUE)
qqp(resid(fourWeekBouts2 ))
plot(density(resid(fourWeekBouts2 )))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekBouts, type=2, ddf="Kenward-Roger")

### Sleep data for last 2-weeks on sugar ####

setwd("/Users/coledeal/Library/CloudStorage/OneDrive-Colostate/CSU/GrassRats/GrassRats/Animal_data/sleepData/Phase2_SleepBout_histogram_files")
#Phase1
## THERE ARE ONLY 6 DAYS of DATA for PHASE 1 
#Phase2
firstTwo8perc_long = read.csv("Phase2_8%sucrose_12Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2", 
         "PERCENTSLEEPNIGHT3", "PERCENTSLEEPNIGHT4", "PERCENTSLEEPNIGHT5", 
         "PERCENTSLEEPNIGHT6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
firstTwo8perc_long = firstTwo8perc_long[12:22,]
firstTwo8perc_long = firstTwo8perc_long[, -(3:4)] #remove first 2 days, so adds up to 2-weeks
#firstTwo8perc_long = firstTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose

firstTwo8perc_short = read.csv("Phase2_8%sucrose_12Aug2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2", 
         "PERCENTSLEEPNIGHT3", "PERCENTSLEEPNIGHT4", "PERCENTSLEEPNIGHT5", 
         "PERCENTSLEEPNIGHT6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
firstTwo8perc_short = firstTwo8perc_short[1:11,]
firstTwo8perc_short = firstTwo8perc_short[-(4),] #remove animal G28
#firstTwo8perc_short = firstTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose
firstTwo8perc_short = firstTwo8perc_short[,-(3:4)] #remove first 2 days, so adds up to 2-weeks


midTwo8perc_long = read.csv("Phase2_8%sucrose_21Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2", 
         "PERCENTSLEEPNIGHT3", "PERCENTSLEEPNIGHT4", "PERCENTSLEEPNIGHT5", 
         "PERCENTSLEEPNIGHT6", "PERCENTSLEEPNIGHT7", "PERCENTSLEEPNIGHT8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
midTwo8perc_long = midTwo8perc_long[12:22,]
#midTwo8perc_long = midTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose


midTwo8perc_short = read.csv("Phase2_8%sucrose_21Aug2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2", 
         "PERCENTSLEEPNIGHT3", "PERCENTSLEEPNIGHT4", "PERCENTSLEEPNIGHT5", 
         "PERCENTSLEEPNIGHT6", "PERCENTSLEEPNIGHT7", "PERCENTSLEEPNIGHT8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
midTwo8perc_short = midTwo8perc_short[1:11,]
midTwo8perc_short = midTwo8perc_short[-(4),] #remove animal G28
#midTwo8perc_short = midTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose

lastTwo8perc_long = read.csv("Phase2_8%sucrose_30Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
lastTwo8perc_long = lastTwo8perc_long[12:22,]
#lastTwo8perc_long = lastTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose

lastTwo8perc_short = read.csv("Phase2_8%sucrose_30Aug2019_short.csv") %>% 
  dplyr::select("MOUSE_ID", "TESTDATE","PERCENTSLEEPNIGHT1", "PERCENTSLEEPNIGHT2") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
lastTwo8perc_short = lastTwo8perc_short[1:11,]
lastTwo8perc_short = lastTwo8perc_short[-(4),] #remove animal G28
#lastTwo8perc_short = lastTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose

## Merge data into one frame
HighSucroseLong = merge(firstTwo8perc_long, midTwo8perc_long, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
HighSucroseLong = merge(HighSucroseLong, lastTwo8perc_long, by = "GrassRat_ID")

HighSucroseLong = HighSucroseLong %>% select(-TESTDATE)
colnames(HighSucroseLong) = c("GrassRat_ID", "TestDate", "SleepNight1","SleepNight2", "SleepNight3", "SleepNight4",
                                 "SleepNight5", "SleepNight6", "SleepNight7", "SleepNight8", "SleepNight9", "SleepNight10",
                                 "SleepNight11", "SleepNight12", "SleepNight13", "SleepNight14")

HighSucroseShort = merge(firstTwo8perc_short, midTwo8perc_short, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
HighSucroseShort  = merge(HighSucroseShort, lastTwo8perc_short, by = "GrassRat_ID")
HighSucroseShort = HighSucroseShort %>% select(-TESTDATE)

colnames(HighSucroseShort) = c("GrassRat_ID", "TestDate", "SleepNight1","SleepNight2", "SleepNight3", "SleepNight4",
                                  "SleepNight5", "SleepNight6", "SleepNight7", "SleepNight8", "SleepNight9", "SleepNight10",
                                  "SleepNight11", "SleepNight12", "SleepNight13", "SleepNight14")

##Merge Phase 1 and Phase 2 animals to match the anim_ID_bothPhases dataframe
DailySleepBothPhasesSuc = rbind(HighSucroseLong, HighSucroseShort)
write.csv(DailySleepBothPhasesSuc, "DailySleepBothPhasesSuc_FINAL.csv")
rownames(DailySleepBothPhasesSuc) = DailySleepBothPhasesSuc$GrassRat_ID
#anim_ID_bothPhases = anim_ID_bothPhases[-c(47,48),]

anim_ID_phase2 = anim_ID_bothPhases %>% filter(Phase == 2) 
anim_ID_phase2 = anim_ID_phase2[-4,]
#match data frames by animal_IDS
intersection = match(anim_ID_phase2$GrassRat_ID, DailySleepBothPhasesSuc$GrassRat_ID)
DailySleepBothPhasesSuc_reorder = DailySleepBothPhasesSuc[intersection, ]

#melt data frame
DailySleepBothPhaseSuc2 = melt(DailySleepBothPhasesSuc_reorder, id.vars= c("GrassRat_ID", "TestDate"), 
                             measure.vars=c("SleepNight1","SleepNight2", "SleepNight3", "SleepNight4",
                                            "SleepNight5", "SleepNight6", "SleepNight7", "SleepNight8", "SleepNight9", "SleepNight10",
                                            "SleepNight11", "SleepNight12", "SleepNight13", "SleepNight14")) %>% na.omit()

DailySleepBothPhaseSuc2$Photoperiod = anim_ID_phase2$Photoperiod
DailySleepBothPhaseSuc2$Sugar_conc = anim_ID_phase2$Sugar_conc
DailySleepBothPhaseSuc2$Sugar = anim_ID_phase2$Sugar
DailySleepBothPhaseSuc2$ParentPair = anim_ID_phase2$ParentPair
DailySleepBothPhaseSuc2$Photoperiod = as.factor(DailySleepBothPhaseSuc2$Photoperiod)

#STATS
#LongPhotoperiod Mean and sd
Natural = DailySleepBothPhaseSuc2 %>% filter(Photoperiod == "Neutral")
mean(Natural$value)
sd(Natural$value)

Short = DailySleepBothPhaseSuc2 %>% filter(Photoperiod == "Short")
mean(Short$value)
sd(Short$value)

# mixed models
library(car)
highSucroseSleep = lmer(value~Photoperiod + (1|GrassRat_ID), DailySleepBothPhaseSuc2, REML = TRUE)
qqp(resid(highSucroseSleep))
plot(density(resid(highSucroseSleep)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(highSucroseSleep, type=2, ddf="Kenward-Roger")


## Average percetn sleep 6-9 weeks after short photoperiod was initially exposed to 4:20
my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))
#Both phases
postsugar_percbouts1 <- read.csv("HighSucrose_29May2019_PercentHourlySleep_processed.csv")
postsugar_percbouts2 <- read.csv("Phase2_8%sucrose_21Aug2019_PercentHourlySleep_processed.csv") 

m.postsugar_percbouts1 <- melt(postsugar_percbouts1, 
                               id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                               measure.vars=c("G9", "G5","G14","G16", "G18", "G10",
                                              "G8", "G2", "T10", "T5", "T17", "T12",
                                              "G4", "G1", "G13", "G17", "G19", "G11",
                                              "G12", "G3", "T9", "T11", "T19", "T13")) 

m.postsugar_percbouts2 <- melt(postsugar_percbouts2, 
                               id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                               measure.vars=c("G35", "G23", "G24", "G28", "G29", "G22", 
                                              "T1", "G36", "G27", "G40", "G33",
                                              "G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                              "G38", "G30")) 

##Merge phase 1 and 2
percbouts <- rbind(m.postsugar_percbouts1, m.postsugar_percbouts2)
## Add sugar availability column
mer.percbouts <- merge(percbouts, anim_ID_sex, by.x = "variable", by.y="GrassRat_ID")

##Both phases
my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))
##Both phases
ggplot(mer.percbouts, aes(Hour_shifted,value)) +  
  stat_pop_sleep_trial(aes(col=Photoperiod.y)) + my_theme +
  ylab("Percentage hourly sleep") + xlab("Hour of day")+ 
  labs(color = "Photoperiod") +
  expand_limits(y=c(NA, 90)) + 
  scale_x_continuous(breaks =seq(0,24,1)) +
  scale_color_manual(values = c("#56B4E9", "#D55E00")) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"))
ggsave("percentsleep.tiff", width = 6.5, height =4, dpi = 300)
  
  
  