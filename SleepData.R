### packages
{library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)}


### Trying out stat_pop_etho style plot
stat_pop_sleep_trial <- function(mapping = NULL, data = NULL,
                                 geom = "smooth", position = "identity",
                                 ...,
                                 method = mean_se,
                                 method.args = list(),
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPopEtho_trial,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = method.args,
      se=TRUE,
      ...
    )
  )
}
#

## Read in sleep data
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

fourWeekStart_phase2combined = read.csv("Phase2_4WeekPhotoperiod_03Jul2019_combined.csv")
fourWeekMid_phase2combined = read.csv("Phase2_4WeekPhotoperiod_22Jul2019_combined.csv")

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
DailySleepBothPhases2$Photoperiod = as.factor(DailySleepBothPhases2$Photoperiod)
DailySleepBothPhases2$Sugar = as.factor(DailySleepBothPhases2$Sugar)
fourWeekSleep = lmer(value~Photoperiod + (1|GrassRat_ID), DailySleepBothPhases2, REML = TRUE)
qqp(resid(fourWeekSleep))
plot(density(resid(fourWeekSleep)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekSleep, ddf="Kenward-Roger")

## SLEEP BOUTS ####

#Phase1
fourWeekEnd_longBouts = read.csv("4WeekPhotoperiod_24Apr2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekEnd_longBouts = fourWeekEnd_longBouts[13:24,]

fourWeekEnd_shortBouts = read.csv("4WeekPhotoperiod_24Apr2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekEnd_shortBouts = fourWeekEnd_shortBouts[1:12,]


fourWeekMid_longBouts = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6", "BOUTLENGTHDAY7", "BOUTLENGTHDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekMid_longBouts = fourWeekMid_longBouts[13:24,]

fourWeekMid_shortBouts = read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv")%>% 
  select("MOUSE_ID", "TESTDATE","BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6", "BOUTLENGTHDAY7", "BOUTLENGTHDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID") 

fourWeekMid_shortBouts = fourWeekMid_shortBouts[1:12,]

#Phase2
fourWeekStart_phase2combined = read.csv("Phase2_4WeekPhotoperiod_03Jul2019_combined.csv")
fourWeekMid_phase2combined = read.csv("Phase2_4WeekPhotoperiod_22Jul2019_combined.csv")

fourWeekStart_longBouts2 = fourWeekStart_phase2combined[-4,] # remove G28
fourWeekStart_shortBouts2 = fourWeekStart_phase2combined[12:22,] #split neutral animals

fourWeekStart_longBouts2 = fourWeekStart_longBouts2 %>% 
  select("MOUSE_ID", "TESTDATE",  "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6", "BOUTLENGTHDAY7", "BOUTLENGTHDAY8", "BOUTLENGTHDAY9",
         "BOUTLENGTHDAY10", "BOUTLENGTHDAY11", "BOUTLENGTHDAY12", "BOUTLENGTHDAY13",
         "BOUTLENGTHDAY14", "BOUTLENGTHDAY15", "BOUTLENGTHDAY16", "BOUTLENGTHDAY17",
         "BOUTLENGTHDAY18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_longBouts2 = fourWeekStart_longBouts2[,-(3:12)]# take last 8 days of data


fourWeekStart_Short2 = fourWeekStart_phase2combined[1:11,] #split short animals
fourWeekStart_Short2 = fourWeekStart_Short2 %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6", "BOUTLENGTHDAY7", "BOUTLENGTHDAY8", "BOUTLENGTHDAY9",
         "BOUTLENGTHDAY10", "BOUTLENGTHDAY11", "BOUTLENGTHDAY12", "BOUTLENGTHDAY13",
         "BOUTLENGTHDAY14", "BOUTLENGTHDAY15", "BOUTLENGTHDAY16", "BOUTLENGTHDAY17",
         "BOUTLENGTHDAY18") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
fourWeekStart_Short2 = fourWeekStart_Short2[,-(3:12)]# take last 8 days of data

fourWeekMid_Neutral2 = fourWeekMid_phase2combined[-4,] # remove G28
fourWeekMid_Neutral2 = fourWeekMid_phase2combined[12:22,] #split neutral animals
fourWeekMid_Neutral2 = fourWeekMid_Neutral2  %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")

fourWeekMid_Short2 = fourWeekMid_phase2combined[1:11,] #split short animals
fourWeekMid_Short2 = fourWeekMid_Short2  %>% 
  select("MOUSE_ID", "TESTDATE", "BOUTLENGTHDAY1","BOUTLENGTHDAY2",
         "BOUTLENGTHDAY3","BOUTLENGTHDAY4","BOUTLENGTHDAY5",
         "BOUTLENGTHDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")


#Join last 2-weeks of 4 week acclimation data frames and caluclate average daily sleep
#for short and long photoperiod animals for phase 1 and phase 2 

#Phase 1 
lastTwo4Week_longBouts = merge(fourWeekMid_longBouts, fourWeekEnd_longBouts, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_longBouts) = c("GrassRat_ID", "TestDate", "BoutDay1","BoutDay2", "BoutDay3", "BoutDay4",
                                "BoutDay5", "BoutDay6", "BoutDay7", "BoutDay8", "BoutDay9", "BoutDay10",
                                "BoutDay11", "BoutDay12", "BoutDay13", "BoutDay14")

lastTwo4Week_shortBouts = merge(fourWeekMid_shortBouts, fourWeekEnd_shortBouts, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_shortBouts) = c("GrassRat_ID", "TestDate", "BoutDay1","BoutDay2", "BoutDay3", "BoutDay4",
                                      "BoutDay5", "BoutDay6", "BoutDay7", "BoutDay8", "BoutDay9", "BoutDay10",
                                      "BoutDay11", "BoutDay12", "BoutDay13", "BoutDay14")

#Phase 2 
lastTwo4Week_longBouts2 = merge(fourWeekStart_longBouts2, fourWeekMid_Neutral2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_longBouts2) = c("GrassRat_ID", "TestDate", "BoutDay1","BoutDay2", "BoutDay3", "BoutDay4",
                                      "BoutDay5", "BoutDay6", "BoutDay7", "BoutDay8", "BoutDay9", "BoutDay10",
                                      "BoutDay11", "BoutDay12", "BoutDay13", "BoutDay14")

lastTwo4Week_shortBouts2 =  merge(fourWeekStart_Short2, fourWeekMid_Short2, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
colnames(lastTwo4Week_shortBouts2) = c("GrassRat_ID", "TestDate", "BoutDay1","BoutDay2", "BoutDay3", "BoutDay4",
                                       "BoutDay5", "BoutDay6", "BoutDay7", "BoutDay8", "BoutDay9", "BoutDay10",
                                       "BoutDay11", "BoutDay12", "BoutDay13", "BoutDay14")

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
                             measure.vars=c("BoutDay1","BoutDay2", "BoutDay3", "BoutDay4",
                                            "BoutDay5", "BoutDay6", "BoutDay7", "BoutDay8", "BoutDay9", "BoutDay10",
                                            "BoutDay11", "BoutDay12", "BoutDay13", "BoutDay14"))

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
SleepBoutsBothPhases2$Photoperiod = as.factor(SleepBoutsBothPhases2$Photoperiod)
SleepBoutsBothPhases2$Sugar = as.factor(SleepBoutsBothPhases2$Sugar)

fourWeekBouts = lmer(value~Photoperiod + (1|GrassRat_ID), SleepBoutsBothPhases2, REML = TRUE)
qqp(resid(fourWeekBouts))
plot(density(resid(fourWeekBouts)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(fourWeekBouts, ddf="Kenward-Roger")

### Sleep data for last 2-weeks on sugar ####
#Phase1
## THERE ARE ONLY 6 DAYS of DATA for PHASE 1 
#Phase2
firstTwo8perc_long = read.csv("Phase2_8%sucrose_12Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
firstTwo8perc_long = firstTwo8perc_long[12:22,]
firstTwo8perc_long = firstTwo8perc_long[, -(3:4)] #remove first 2 days, so adds up to 2-weeks
#firstTwo8perc_long = firstTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose

firstTwo8perc_short = read.csv("Phase2_8%sucrose_12Aug2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
firstTwo8perc_short = firstTwo8perc_short[1:11,]
firstTwo8perc_short = firstTwo8perc_short[-(4),] #remove animal G28
#firstTwo8perc_short = firstTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose
firstTwo8perc_short = firstTwo8perc_short[,-(3:4)] #remove first 2 days, so adds up to 2-weeks


midTwo8perc_long = read.csv("Phase2_8%sucrose_21Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
midTwo8perc_long = midTwo8perc_long[12:22,]
#midTwo8perc_long = midTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose


midTwo8perc_short = read.csv("Phase2_8%sucrose_21Aug2019_short.csv") %>% 
  select("MOUSE_ID", "TESTDATE",  "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2", 
         "PERCENTSLEEPDAY3", "PERCENTSLEEPDAY4", "PERCENTSLEEPDAY5", 
         "PERCENTSLEEPDAY6", "PERCENTSLEEPDAY7", "PERCENTSLEEPDAY8") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
midTwo8perc_short = midTwo8perc_short[1:11,]
midTwo8perc_short = midTwo8perc_short[-(4),] #remove animal G28
#midTwo8perc_short = midTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose

lastTwo8perc_long = read.csv("Phase2_8%sucrose_30Aug2019_long.csv") %>% 
  select("MOUSE_ID", "TESTDATE", "PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
lastTwo8perc_long = lastTwo8perc_long[12:22,]
#lastTwo8perc_long = lastTwo8perc_long[c(1,2,5,7,10),] # keep animals only on 8% sucrose

lastTwo8perc_short = read.csv("Phase2_8%sucrose_30Aug2019_short.csv") %>% 
  dplyr::select("MOUSE_ID", "TESTDATE","PERCENTSLEEPDAY1", "PERCENTSLEEPDAY2") %>% 
  dplyr::rename("GrassRat_ID" = "MOUSE_ID")
lastTwo8perc_short = lastTwo8perc_short[1:11,]
lastTwo8perc_short = lastTwo8perc_short[-(4),] #remove animal G28
#lastTwo8perc_short = lastTwo8perc_short[c(1,2,4,6,9),] # keep animals only on 8% sucrose

## Merge data into one frame
HighSucroseLong = merge(firstTwo8perc_long, midTwo8perc_long, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
HighSucroseLong = merge(HighSucroseLong, lastTwo8perc_long, by = "GrassRat_ID")

HighSucroseLong = HighSucroseLong %>% select(-TESTDATE)
colnames(HighSucroseLong) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                              "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                              "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")

HighSucroseShort = merge(firstTwo8perc_short, midTwo8perc_short, by = "GrassRat_ID") %>%
  select(-TESTDATE.y) 
HighSucroseShort  = merge(HighSucroseShort, lastTwo8perc_short, by = "GrassRat_ID")
HighSucroseShort = HighSucroseShort %>% select(-TESTDATE)

colnames(HighSucroseShort) = c("GrassRat_ID", "TestDate", "SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                               "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                               "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")

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
                             measure.vars=c("SleepDay1","SleepDay2", "SleepDay3", "SleepDay4",
                                            "SleepDay5", "SleepDay6", "SleepDay7", "SleepDay8", "SleepDay9", "SleepDay10",
                                            "SleepDay11", "SleepDay12", "SleepDay13", "SleepDay14")) %>% na.omit()

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

DailySleepBothPhaseSuc2
# mixed models
library(car)
highSucroseSleep = lmer(value~Photoperiod*Sugar_conc + (1|GrassRat_ID), DailySleepBothPhaseSuc2, REML = TRUE)
qqp(resid(highSucroseSleep))
plot(density(resid(highSucroseSleep)))

if(requireNamespace("pbkrtest", quietly = TRUE)) 
  anova(highSucroseSleep, ddf="Kenward-Roger")


## Sleep Figure for MS ####
## Read in files
anim_ID_sex <- read.csv("Chamber_Animal_IDs_Sex_bothPhases.csv")
piezoday_4wk1 <- read.csv("4WeekPhotoperiod_24Apr2019_combined_with_sub_day.csv")
piezoday_2wk1 <- read.csv("2WeekAcclimation_26Mar2019_longTime.csv")
piezoday_LowSugar1 <- read.csv("LowSucrose_andTest_07May2019_combined_with_sub_day.csv")
piezoday_HighSugar1 <- read.csv("HighSucrose_29May2019_combined_with_sub_day.csv")
piezoday_presugar_bouts1 <- read.csv("4WeekPhotoperiod_24Apr2019SleepBout_MeanSB.csv")
piezoday_postsugar_bouts1 <- read.csv("HighSucrose_29May2019SleepBout_MeanSB_processed.csv") ## For mean hourly sleep bouts
postsugar_percbouts1 <- read.csv("HighSucrose_29May2019_PercentHourlySleep_processed.csv") ## For % hourly sleep
postsugar_percbouts2 <- read.csv("Phase2_8%sucrose_21Aug2019_PercentHourlySleep_processed.csv") ## For % hourly sleep

bound_prepost_sugar <- read.csv("HourlySleepBouts_PrePostSugar.csv")

piezoday_4wk2 <- read.csv("Phase2_4WeekPhotoperiod_22Jul2019_combined_with_sub_day.csv")
piezoday_2wk2 <- read.csv("Phase2_2WeekAcclimation_19Jun2019_long.csv")
piezoday_LowSugar2 <- read.csv("Phase2_LowSucroseTest_03Aug2019_combined_with_sub_day.csv")
piezoday_HighSugar2 <- read.csv("Phase2_8%sucrose_21Aug2019_combined_with_sub_day.csv") ## Removed G28
piezoday_presugar_bouts2 <- read.csv("Phase2_4WeekPhotoperiod_22Jul2019SleepBout_MeanSB.csv")
piezoday_postsugar_bouts2 <- read.csv("Phase2_8%sucrose_21Aug2019SleepBout_MeanSB.csv")

## Melt to form long dataframes 
#phase 1
m.piezoday_short1 <- melt(piezoday_4wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                            "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short1 <- melt(piezoday_4wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                             "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                             "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                             "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                             "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                             "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                             "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                             "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                             "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))

m.piezoday_long1 <- melt(piezoday_2wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                           "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                           "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                           "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                           "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                           "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                           "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                           "PERCENTSLEEPDAY5"))

m.piezobout_long1 <- melt(piezoday_2wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                            "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                            "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                            "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                            "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                            "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                            "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                            "BOUTLENGTHDAY5"))

m.piezoday_LowSugar1 <- melt(piezoday_LowSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                                    "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                                    "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                                    "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                                    "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                                    "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                                    "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                                    "PERCENTSLEEPDAY5"))

m.piezobout_LowSugar1 <- melt(piezoday_LowSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                                     "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                                     "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                                     "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                                     "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                                     "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                                     "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                                     "BOUTLENGTHDAY5"))

m.piezoday_HighSugar1 <- melt(piezoday_HighSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                                      "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                                      "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                                      "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                                      "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                                      "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                                      "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                                      "PERCENTSLEEPDAY5"))

m.piezobout_HighSugar1 <- melt(piezoday_HighSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                                       "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                                       "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                                       "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                                       "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                                       "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                                       "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                                       "BOUTLENGTHDAY5"))

#phase 2

m.piezoday_short2 <- melt(piezoday_4wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                            "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short2 <- melt(piezoday_4wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                             "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                             "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                             "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                             "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                             "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                             "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                             "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                             "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))

m.piezoday_long2 <- melt(piezoday_2wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                           "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                           "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                           "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                           "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                           "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                           "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                           "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6",
                                                                                           "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6", 
                                                                                           "PERCENTSLEEPTOT7", "PERCENTSLEEPNIGHT7",
                                                                                           "PERCENTSLEEPDAY7", "PERCENTSLEEPTOT8",
                                                                                           "PERCENTSLEEPNIGHT8", "PERCENTSLEEPDAY8", 
                                                                                           "PERCENTSLEEPTOT9", "PERCENTSLEEPNIGHT9",
                                                                                           "PERCENTSLEEPDAY9", "PERCENTSLEEPTOT10",
                                                                                           "PERCENTSLEEPNIGHT10", "PERCENTSLEEPDAY10",
                                                                                           "PERCENTSLEEPTOT11", "PERCENTSLEEPNIGHT11", 
                                                                                           "PERCENTSLEEPDAY11"))
m.piezoday_long2$variable <- revalue(m.piezoday_long2$variable, 
                                     c("PERCENTSLEEPTOT10"="PERCENTSLEEPTOTX","PERCENTSLEEPNIGHT10"="PERCENTSLEEPNIGHTX",
                                       "PERCENTSLEEPDAY10"="PERCENTSLEEPDAYX", "PERCENTSLEEPTOT11"="PERCENTSLEEPTOTY",
                                       "PERCENTSLEEPNIGHT11"="PERCENTSLEEPNIGHTY","PERCENTSLEEPDAY11"="PERCENTSLEEPDAYY"))



m.piezobout_long2 <- melt(piezoday_2wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                            "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                            "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                            "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                            "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                            "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                            "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                            "BOUTLENGTHDAY5", "BOUTLENGTHTOT6",
                                                                                            "BOUTLENGTHNITE6", "BOUTLENGTHDAY6",
                                                                                            "BOUTLENGTHTOT7", "BOUTLENGTHNITE7",
                                                                                            "BOUTLENGTHDAY7", "BOUTLENGTHTOT8",
                                                                                            "BOUTLENGTHNITE8", "BOUTLENGTHDAY8",
                                                                                            "BOUTLENGTHTOT9", "BOUTLENGTHNITE9",
                                                                                            "BOUTLENGTHDAY9", "BOUTLENGTHTOT10",
                                                                                            "BOUTLENGTHNITE10", "BOUTLENGTHDAY10",
                                                                                            "BOUTLENGTHTOT11", "BOUTLENGTHNITE11",
                                                                                            "BOUTLENGTHDAY11"))

m.piezobout_long2$variable <- revalue(m.piezobout_long2$variable, 
                                      c("BOUTLENGTHTOT10"="BOUTLENGTHTOTX","BOUTLENGTHNITE10"="BOUTLENGTHNITEX",
                                        "BOUTLENGTHDAY10"="BOUTLENGTHDAYX", "BOUTLENGTHTOT11"="BOUTLENGTHTOTY",
                                        "BOUTLENGTHNITE11"="BOUTLENGTHNITEY","BOUTLENGTHDAY11"="BOUTLENGTHDAYY"))


m.piezoday_LowSugar2 <- melt(piezoday_LowSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                                    "PERCENTSLEEPDAY1"))

m.piezobout_LowSugar2 <- melt(piezoday_LowSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                                     "BOUTLENGTHDAY1"))

m.piezoday_HighSugar2 <- melt(piezoday_HighSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                                      "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                                      "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                                      "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                                      "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                                      "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                                      "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                                      "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6",
                                                                                                      "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6",
                                                                                                      "PERCENTSLEEPTOT7", "PERCENTSLEEPNIGHT7",
                                                                                                      "PERCENTSLEEPDAY7", "PERCENTSLEEPTOT8",
                                                                                                      "PERCENTSLEEPNIGHT8", "PERCENTSLEEPDAY8"))

m.piezobout_HighSugar2 <- melt(piezoday_HighSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                                       "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                                       "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                                       "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                                       "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                                       "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                                       "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                                       "BOUTLENGTHDAY5", "BOUTLENGTHTOT6",
                                                                                                       "BOUTLENGTHNITE6", "BOUTLENGTHDAY6",
                                                                                                       "BOUTLENGTHTOT7", "BOUTLENGTHNITE7",
                                                                                                       "BOUTLENGTHDAY7", "BOUTLENGTHTOT8",
                                                                                                       "BOUTLENGTHNITE8", "BOUTLENGTHDAY8"))

#### Mean hourly sleep bout length ####
## CAUTION DO NOT RUN IF TRYING OUT HOURLY SLEEP MODELS OR PLOTS
m.postsugar_bouts1 <- melt(piezoday_postsugar_bouts1, 
                           id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                           measure.vars=c("G9", "G5","G14","G16", "G18", "G10",
                                          "G8", "G2", "T10", "T5", "T17", "T12",
                                          "G4", "G1", "G13", "G17", "G19", "G11",
                                          "G12", "G3", "T9", "T11", "T19", "T13"))                                                                               

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="AM"] <- 
  m.postsugar_bouts1$Hour[m.postsugar_bouts1$am_pm=="AM"]

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="PM"  & m.postsugar_bouts1$Hour!=12] <- 
  12+m.postsugar_bouts1$Hour[m.postsugar_bouts1$am_pm=="PM" & m.postsugar_bouts1$Hour!=12]

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="PM"  & m.postsugar_bouts1$Hour==12] <- 12
m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="AM"  & m.postsugar_bouts1$Hour==12] <- 00



m.postsugar_bouts1$Date <- as.POSIXct(paste(paste(m.postsugar_bouts1$Day, m.postsugar_bouts1$Month, 
                                                  m.postsugar_bouts1$Year, sep="/"),
                                            paste(m.postsugar_bouts1$Hour2, m.postsugar_bouts1$Minute, sep=":")), 
                                      format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_bouts1$Photoperiod <- 0
m.postsugar_bouts1$Photoperiod[m.postsugar_bouts1$variable 
                               %in% c("G9", "G5","G14","G16", "G18", "G10",
                                      "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar_bouts1$Photoperiod[m.postsugar_bouts1$variable 
                               %in% c("G4", "G1", "G13", "G17", "G19", "G11",
                                      "G12", "G3", "T9", "T11", "T19", "T13")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_bouts1$Hour_shifted <- m.postsugar_bouts1$Hour2
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Photoperiod=="Short"] <- 
  m.postsugar_bouts1$Hour2[m.postsugar_bouts1$Photoperiod=="Short"]+(3)
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==24] <- 0
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==25] <- 1
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==26] <- 2


## Procesing perentage bouts the same way as the mean sleep bouts one
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

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="AM"] <- 
  m.postsugar_percbouts1$Hour[m.postsugar_percbouts1$am_pm=="AM"]

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="PM"  & m.postsugar_percbouts1$Hour!=12] <- 
  12+m.postsugar_percbouts1$Hour[m.postsugar_percbouts1$am_pm=="PM" & m.postsugar_percbouts1$Hour!=12]

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="PM"  & m.postsugar_percbouts1$Hour==12] <- 12
m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="AM"  & m.postsugar_percbouts1$Hour==12] <- 00

m.postsugar_percbouts1$Date <- as.POSIXct(paste(paste(m.postsugar_percbouts1$Day, m.postsugar_percbouts1$Month, 
                                                      m.postsugar_percbouts1$Year, sep="/"),
                                                paste(m.postsugar_percbouts1$Hour2, m.postsugar_percbouts1$Minute, sep=":")), 
                                          format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_percbouts1$Photoperiod <- 0
m.postsugar_percbouts1$Photoperiod[m.postsugar_percbouts1$variable 
                                   %in% c("G9", "G5","G14","G16", "G18", "G10",
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar_percbouts1$Photoperiod[m.postsugar_percbouts1$variable 
                                   %in% c("G4", "G1", "G13", "G17", "G19", "G11",
                                          "G12", "G3", "T9", "T11", "T19", "T13")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_percbouts1$Hour_shifted <- m.postsugar_percbouts1$Hour2
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Photoperiod=="Short"] <- 
  m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$Photoperiod=="Short"]+(3)
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==24] <- 0
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==25] <- 1
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==26] <- 2

## Create a subjDayNight column
#### Add a column for subjective day/night
#### TRIED 0600 - 1800 but now trying 0400 to 1700 from stat_pop_etho graph
m.postsugar_percbouts1$SubjDayNight <- "NA"
m.postsugar_percbouts1$SubjDayNight[m.postsugar_percbouts1$Hour_shifted<17 & m.postsugar_percbouts1$Hour_shifted>=4] <- "Day"
m.postsugar_percbouts1$SubjDayNight[m.postsugar_percbouts1$Hour_shifted>=17|m.postsugar_percbouts1$Hour_shifted<4] <- "Night"

m.postsugar_percbouts2$SubjDayNight <- "NA"
m.postsugar_percbouts2$SubjDayNight[m.postsugar_percbouts2$Hour_shifted<17 & m.postsugar_percbouts2$Hour_shifted>=4] <- "Day"
m.postsugar_percbouts2$SubjDayNight[m.postsugar_percbouts2$Hour_shifted>=17|m.postsugar_percbouts2$Hour_shifted<4] <- "Night"


## For phase 2
m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="AM"] <- 
  m.postsugar_percbouts2$Hour[m.postsugar_percbouts2$am_pm=="AM"]

m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="PM"  & m.postsugar_percbouts2$Hour!=12] <- 
  12+m.postsugar_percbouts2$Hour[m.postsugar_percbouts2$am_pm=="PM" & m.postsugar_percbouts2$Hour!=12]

m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="PM"  & m.postsugar_percbouts2$Hour==12] <- 12
m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="AM"  & m.postsugar_percbouts2$Hour==12] <- 00

m.postsugar_percbouts2$Date <- as.POSIXct(paste(paste(m.postsugar_percbouts2$Day, m.postsugar_percbouts2$Month, 
                                                      m.postsugar_percbouts2$Year, sep="/"),
                                                paste(m.postsugar_percbouts2$Hour2, m.postsugar_percbouts2$Minute, sep=":")), 
                                          format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_percbouts2$Photoperiod <- 0
m.postsugar_percbouts2$Photoperiod[m.postsugar_percbouts2$variable 
                                   %in% c("G35", "G23", "G24", "G28", "G29", "G22", 
                                          "T1", "G36", "G27", "G40", "G33")] <- "Short"
m.postsugar_percbouts2$Photoperiod[m.postsugar_percbouts2$variable 
                                   %in% c("G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                          "G38", "G30")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_percbouts2$Hour_shifted <- m.postsugar_percbouts2$Hour2
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Photoperiod=="Short"] <- 
  m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$Photoperiod=="Short"]+(3)
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==24] <- 0
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==25] <- 1
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==26] <- 2

##Merge phase 1 and 2
percbouts <- rbind(m.postsugar_percbouts1, m.postsugar_percbouts2)

## Add sugar availability column
mer.percbouts <- merge(percbouts, anim_ID_sex, by.x = "variable", by.y="GrassRat_ID")

head(mer.percbouts)

# Daily sleep ####
#phase 1
m.piezoday_long1$Treatment <- "2wk"
m.piezoday_short1$Treatment <- "4wk"
m.piezoday_LowSugar1$Treatment <- "LowSugar"
m.piezoday_HighSugar1$Treatment <- "HighSugar"

m.piezobout_long1$Treatment <- "2wk"
m.piezobout_short1$Treatment <- "4wk"
m.piezobout_LowSugar1$Treatment <- "LowSugar"
m.piezobout_HighSugar1$Treatment <- "HighSugar"

m.piezoday_short1 <- rename(m.piezoday_short1, "GrassRatID"="MOUSE_ID")
m.piezoday_long1 <- rename(m.piezoday_long1, "GrassRatID"="MOUSE_ID")
m.piezoday_LowSugar1 <- rename(m.piezoday_LowSugar1, "GrassRatID"="MOUSE_ID")
m.piezoday_HighSugar1 <- rename(m.piezoday_HighSugar1, "GrassRatID"="MOUSE_ID")

m.piezobout_short1 <- rename(m.piezobout_short1, "GrassRatID"="MOUSE_ID")
m.piezobout_long1 <- rename(m.piezobout_long1, "GrassRatID"="MOUSE_ID")
m.piezobout_LowSugar1 <- rename(m.piezobout_LowSugar1, "GrassRatID"="MOUSE_ID")
m.piezobout_HighSugar1 <- rename(m.piezobout_HighSugar1, "GrassRatID"="MOUSE_ID")

m.piezobout_4wksugars1 <- rbind(m.piezobout_short1, m.piezobout_LowSugar1, m.piezobout_HighSugar1)
m.piezoday_4wksugars1 <- rbind(m.piezoday_short1, m.piezoday_LowSugar1, m.piezoday_HighSugar1)

#phase 2
m.piezoday_long2$Treatment <- "2wk"
m.piezoday_short2$Treatment <- "4wk"
m.piezoday_LowSugar2$Treatment <- "LowSugar"
m.piezoday_HighSugar2$Treatment <- "HighSugar"

m.piezobout_long2$Treatment <- "2wk"
m.piezobout_short2$Treatment <- "4wk"
m.piezobout_LowSugar2$Treatment <- "LowSugar"
m.piezobout_HighSugar2$Treatment <- "HighSugar"

m.piezoday_short2 <- rename(m.piezoday_short2, "GrassRatID"="MOUSE_ID")
m.piezoday_long2 <- rename(m.piezoday_long2, "GrassRatID"="MOUSE_ID")
m.piezoday_LowSugar2 <- rename(m.piezoday_LowSugar2, "GrassRatID"="MOUSE_ID")
m.piezoday_HighSugar2 <- rename(m.piezoday_HighSugar2,"GrassRatID"="MOUSE_ID")

m.piezobout_short2 <- rename(m.piezobout_short2, "GrassRatID"="MOUSE_ID")
m.piezobout_long2 <- rename(m.piezobout_long2, "GrassRatID"="MOUSE_ID")
m.piezobout_LowSugar2 <- rename(m.piezobout_LowSugar2, "GrassRatID"="MOUSE_ID")
m.piezobout_HighSugar2 <- rename(m.piezobout_HighSugar2,"GrassRatID"="MOUSE_ID")

m.piezobout_4wksugars2 <- rbind(m.piezobout_short2, m.piezobout_LowSugar2, m.piezobout_HighSugar2)
m.piezoday_4wksugars2 <- rbind(m.piezoday_short2, m.piezoday_LowSugar2, m.piezoday_HighSugar2)

## After this rbinding, you'll have  combined files for all Phase 1 and phase 2 animals
# rbind() each pair of phase 1 and Phase 2 files
## e.g. m.piezobout_short1 <- rbind(m.piezobout_short1,m.piezobout_short2)
## After binding phase files, you should have one _short, one _long, one _4wksugars
m.piezobout_short <- rbind(m.piezobout_short1,m.piezobout_short2)
m.piezobout_long <- rbind(m.piezobout_long1,m.piezobout_long2)

m.piezoday_short <- rbind(m.piezoday_short1,m.piezoday_short2)
m.piezoday_long <- rbind(m.piezoday_long1,m.piezoday_long2)

m.piezobout_4wksugars <- rbind(m.piezobout_4wksugars1,m.piezobout_4wksugars2)
m.piezoday_4wksugars <- rbind(m.piezoday_4wksugars1,m.piezoday_4wksugars2)

## Add chamber numbers to data frame
anim_ID_sex = rename(anim_ID_sex, "GrassRatID" = "GrassRat_ID")

#rename columns
names(m.piezoday_4wksugars)[names(m.piezoday_4wksugars) == "MOUSE_ID"] <- "GrassRat_ID"
names(m.piezoday_long)[names(m.piezoday_long) == "MOUSE_ID"] <- "GrassRat_ID"
names(m.piezobout_short)[names(m.piezobout_short) == "MOUSE_ID"] <- "GrassRat_ID"

m.piezoday_4wksugars <- merge(m.piezoday_4wksugars, anim_ID_sex, by=c("GrassRat_ID"))
m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_ID_sex, by=c("GrassRat_ID"))

m.piezoday_long <- merge(m.piezoday_long, anim_ID_sex, by=c("GrassRat_ID"))

m.piezobout_short <- merge(m.piezobout_short, anim_ID_sex, by=c("GrassRat_ID"))


## Add photoperiod column to each dataframe
add_photoperiod <- function(y) {
  y$Photoperiod <- 0
  for(i in 1:nrow(y)) {
    y$Photoperiod[i][y$Chamber[i]<13 & y$Phase[i]==1] <- "Short"
    y$Photoperiod[i][y$Chamber[i]>12 & y$Phase[i]==1] <- "Neutral"
    y$Photoperiod[i][y$Chamber[i]<12 & y$Phase[i]==2] <- "Short"
    y$Photoperiod[i][y$Chamber[i]>11 & y$Phase[i]==2] <- "Neutral"
  }
  return(y)
}

m.piezobout_4wksugars <- add_photoperiod(m.piezobout_4wksugars)
m.piezoday_4wksugars <- add_photoperiod(m.piezoday_4wksugars)
m.piezobout_long <- add_photoperiod(m.piezobout_long)
m.piezoday_long <- add_photoperiod(m.piezoday_long)
m.piezobout_short <- add_photoperiod(m.piezobout_short)
m.piezoday_short <- add_photoperiod(m.piezoday_short)

##Summary stats
mpiezoday_long = m.piezoday_long %>% filter(variable == "PERCENTSLEEPDAY1" | variable == "PERCENTSLEEPDAY2" | variable == "PERCENTSLEEPDAY3" | variable == "PERCENTSLEEPDAY4" | variable == "PERCENTSLEEPDAY5" | variable == "PERCENTSLEEPDAY6" | variable == "PERCENTSLEEPDAY7" | variable == "PERCENTSLEEPDAY8" | variable == "PERCENTSLEEPDAY9" | variable == "PERCENTSLEEPDAYX" | variable == "PERCENTSLEEPDAYY") %>% filter (Photoperiod == "Neutral")
mean(mpiezoday_long$value)
sd(mpiezoday_long$value)

## Sleep Plot for MS 
my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))
##Both phases
ggplot(mer.percbouts, aes(Hour_shifted,value)) +  
  stat_pop_sleep_trial(aes(col=Photoperiod.y)) + my_theme +
  ylab("Percentage hourly sleep") + xlab("Hour of day")+ 
  labs(color = "Photoperiod") + 
  scale_x_continuous(breaks =seq(0,24,1)) +
  scale_color_manual(values = c("#56B4E9", "#D55E00")) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"))

ggsave("percentsleep.png", width = 6.5, height =4, dpi = 800)
  
  
  