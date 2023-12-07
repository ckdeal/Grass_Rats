# Read in required packages
{library(plyr)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(multcomp)
  library(dplyr)
  library(data.table)
  library(car)
  library(tidyverse)
  library(ggplot2)
  library(ggpubr)
  library(emmeans)
  library(effects)
  library(rstatix)
  library(plotrix)
  library(bestNormalize)
  library(sjPlot)
  library(vtable)
  library(easystats)
  library(merDeriv)
  library(patchwork)
  library(lmerTest)}


## Read in data files
conc <- read.csv("SugarConcTest_weights_05Feb2020.csv")
fatpad <- read.csv(("Fat_pads.csv"))
weights <- read.csv(("Animal_weights_forMassChange.csv"))
liverdata = read.csv("Merged_Liver.csv")
age = read.csv("Chamber_Animal_IDs_Sex_bothPhases.csv")

# Set theme
my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

# Animal Age data ####
#both phases
age$startAge = (age$AgeAtDeath_days - 61)
age = age %>% filter(GrassRat_ID != "G28")
age = age[-(46:47),]

mean(age$startAge, na.rm = TRUE)
sd(age$startAge, na.rm = TRUE)

agephase1 = age %>% filter(Phase == 1)
mean(agephase1$startAge, na.rm = TRUE)
sd(agephase1$startAge, na.rm = TRUE)

agephase2 = age %>% filter(Phase == 2)
mean(agephase2$startAge, na.rm = TRUE)
sd(agephase2$startAge, na.rm = TRUE)

# ANIMAL WEIGHT DATA ####
weight
# subset data and set factors for body mass change over experiment
weight = weights %>% dplyr::select(Individual, Group, Sugar, Photoperiod, Weight_start, Weight_euthanasia) %>% group_by(Sugar, Photoperiod) %>% 
  na.omit()
#add pct change from start to euthanasia
weight = weight %>% mutate(pctChange = (Weight_euthanasia - Weight_start)/Weight_start*100)
# summary stats for pct change
mean(weight$pctChange) #mean
sd(weight$pctChange) #sd

## model this pct change
pct = lmer(value~variable + (1|Individual), weight2)
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(pct, ddf="Kenward-Roger")

qqp(resid(pct))
hist(resid(pct))

## Subset data in 4-week acclimation ####
#group by 4-week photoperiod
wt = weights %>% group_by(Photoperiod) %>% summarise(fourwkMean = mean(pct_presugar), fourwksd = sd(pct_presugar))
wt$Photoperiod = as.factor(wt$Photoperiod)

#group by euthanasia by photoperiod and sugar
wt2 = weights %>% group_by(Photoperiod, Sugar) %>% summarise(euthanasiaMean = mean(pct_euthanasia), euthanasiasd = sd(pct_euthanasia))
wt2$Photoperiod = as.factor(wt2$Photoperiod)
wt2$Sugar = as.factor(wt2$Sugar)

## Plot mean and sd faceted
wt$title = "4-week Acclimation"
wtplot = ggplot(weights, aes(Photoperiod, pct_presugar)) + 
  geom_boxplot(aes(fill = Photoperiod), outlier.shape = NA) + 
  geom_point(aes(fill = Photoperiod), size = 1.5, shape = 21, 
             position = position_jitterdodge()) + my_theme + 
  labs(y = "Percent body mass change (%)") +
  ylim(0,35) + facet_wrap(~wt$title) + 
  scale_fill_manual(name = "Photoperiod", labels = c("Neutral", "Short"), 
                    values = c("#56B4E9", "#D55E00")) # vermillion and sky blue color blind friendly

wtplot
ggsave("acc2.tiff", width = 5, height = 3.5, dpi = 300)

wt2$title = "Euthanasia"
wtplot2 = ggplot(weights, aes(Photoperiod, pct_euthanasia, fill = Sugar)) + 
  geom_boxplot(aes(fill = Sugar), outlier.shape = NA) + 
  geom_point(aes(fill = Sugar), size = 1.5, shape = 21, 
             position = position_jitterdodge()) + my_theme + scale_fill_manual(values = c("blue", "grey80")) + facet_wrap(~wt2$title) + theme(axis.title.y = element_blank())
wtplot2

#get legends
legend_1 = get_legend(wtplot)
legend_2 = get_legend(wtplot2)
legends = ggarrange(legend_1, legend_2, nrow = 2)

rm_legend <- function(p){p + theme(legend.position = "none")}
plots = ggarrange(rm_legend(wtplot), rm_legend(wtplot2), nrow = 1)

ggarrange(plots, legends, widths = c(0.85, 0.15), nrow = 1) 

ggsave("4week.png", path = NULL, width=8, height=4.5, dpi = 800)
dev.off()

library(cowplot)
plot_grid(plots, legends, nrow = 1, align = "h")

## Model the change in body mass data for 4-week acclimation
weights$Individual = as.factor(weights$Individual)
weights$Photoperiod = as.factor(weights$Photoperiod)
weights$Sugar = as.factor(weights$Sugar)
str(weights)

## Run kruskal.test
kruskal.test(weights$pct_presugar~weights$Photoperiod)

## Kruskal wallis for weight at euthanasia
inter = interaction(weights$Photoperiod, weights$Sugar)
kruskal.test(weights$pct_euthanasia, weights$Photoperiod)
kruskal.test(weights$pct_euthanasia, weights$Sugar)
kruskal.test(pct_euthanasia ~ inter, data = weights) # test interaction

# Dunns test for post-hoc
library(FSA)
dunnTest(pct_euthanasia ~ inter, data = weights, method="bh")


# FAT PAD DATA ####
#set factors
fatpad$Individual = as.character(fatpad$Individual)
fatpad$Treatment = as.factor(fatpad$Treatment)
fatpad$Sugar = as.factor(fatpad$Sugar)

ggplot(fatpad, aes(Treatment, Fat_pad_wt_g, fill = Sugar)) + 
  geom_boxplot(aes(fill = Sugar), outlier.shape = NA) + 
  geom_point(aes(fill = Sugar), size = 1.5, shape = 21, 
             position = position_jitterdodge()) + 
  my_theme + 
  labs(y = "Fat pad weight (g)") + 
  scale_fill_manual(name="Sugar", labels=c("High", "None"), 
                    values = c("blue", "grey80")) + 
  scale_x_discrete(name = "Photoperiod", breaks = c("Long", "Short"), labels = c("Neutral", "Short"))

ggsave("fatpad.png", width = 5, height = 4, dpi = 800)

## Fatpad model
str(fatpad)
fatpad$Sugar = as.factor(fatpad$Sugar)
fataov = aov(Fat_pad_wt_g~Treatment*Sugar, fatpad)
qqp(resid(fataov))
plot(density(resid(fataov)))
summary(fataov)
pairs(emmeans(fataov, ~Treatment|Sugar))

# LIVER STEATOSIS ####
# Set factors and numerics
liverdata$ID = as.factor(liverdata$ID)
liverdata$PercentArea = as.numeric(liverdata$PercentArea)
liverdata$PhotoSugar = as.factor(liverdata$PhotoSugar)
liverdata$Photoperiod = as.factor(liverdata$Photoperiod)
liverdata$Sugar = as.factor(liverdata$Sugar)

## Plots of steatosis data both phases
ggplot(liverdata, aes(PhotoSugar, PercentArea)) + 
  geom_boxplot(aes(fill=Sugar), outlier.shape = NA) + my_theme + 
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(hjust=0.5, vjust=0.8)) + 
  geom_point(aes(fill = Sugar), size = 1.5, shape = 21, 
             position = position_jitterdodge()) + 
  xlab("Photoperiod") +
  scale_fill_manual(values = c("blue", "grey80")) +
  ylab("Macrosteatosis in liver (%)") +
  scale_x_discrete(labels=c("NeutralHigh" = "Neutral", "NeutralNone" = "Neutral",
                            "ShortHigh" = "Short", "ShortNone" = "Short")) + 
  scale_fill_manual(name="Sugar", labels=c("High", "None"), 
                    values = c("blue", "grey80"))

ggsave("macrosteatosis.png", height = 4, width = 5, dpi = 800)

## STEATOSIS MODEL ##
liverlm = lmer(PercentArea ~ Photoperiod*Sugar + (1|ID), liverdata)
qqp(resid(liverlm))

#Normalizd data
str(liverdata)
BNobject3 = bestNormalize(liverdata$PercentArea)
BNobject3 #boxcox transform is best
box_obj = bestNormalize::boxcox(liverdata$PercentArea)
liverdata$PercentNorm = box_obj$x.t # add normalized data to df

liverlm2 = lmer(PercentNorm~Photoperiod*Sugar + (1|ID), liverdata)
qqp(resid(liverlm2))
plot(density(resid(liverlm2)))
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(liverlm2, ddf="Kenward-Roger")

# SUGAR CONSUMPTION ####
#set factors
str(conc)
conc$Consumed = as.numeric(conc$Consumed)
conc$Sugar_conc = as.numeric(conc$Sugar_conc)
conc$Treatment = as.factor(conc$Treatment)

## Subset data to only include 2% consumption during the 2-day Test period
twopercentTest = conc %>% select(IndivSugarDay, Day, Month, Year, Indiv, 
                                 Pre_test, Treatment, Animal_wt_g,	
                                 Sex,	Sugar_conc,	Sugar_ConcDay, 
                                 Consumed, Water_mmt_good, Sugar_mmt_good) %>% 
  filter(Sugar_conc == "0.02") %>% filter(Pre_test == "Test") %>%
  filter(Water_mmt_good == "Y") %>% filter(Sugar_mmt_good == "Y") %>% 
  filter(Month != 8) %>% filter(Day != 7)

str(twopercentTest)
twopercentTest$Consumed = as.numeric(twopercentTest$Consumed)
twopercentTest$Treatment = as.factor(twopercentTest$Treatment)
# Look at data
hist(twopercentTest$Consumed)

# Turn consumption into a percentage
twopercentTest$PercConsumed = twopercentTest$Consumed*100

## Plot 2% consumption over 2-day Test period
ggplot(twopercentTest, aes(Treatment, PercConsumed, group = Treatment)) + 
  geom_boxplot(aes(fill = Treatment), outlier.shape = NA) + 
  geom_point(aes(fill = Treatment), size = 1.5, shape = 21, 
             position = position_jitterdodge()) + 
  my_theme + 
  labs(y = "Proportion of 2% solution consumed (%)", x = "Photoperiod") + 
  scale_fill_manual(values = c("blue", "grey80")) + 
  scale_x_discrete(labels=c("Long" = "Neutral", "Short" = "Short")) + 
  scale_fill_manual(name= c("Photoperiod"), labels=c("Neutral", "Short"), 
                    values = c("#56B4E9", "#D55E00"))


ggsave("twopercentconsume.png", width = 5, height = 4, dpi = 800)

## 2% consumption model
twopercentlm = lmer(Consumed~Treatment + (1|Indiv), twopercentTest)
qqp(resid(twopercentlm))
plot(density(resid(twopercentlm)))
plot(twopercentlm)

# normalize and rerun
library(bestNormalize)
bestobj = bestNormalize(twopercentTest$Consumed)
bestobj #
bestobj1 = orderNorm(twopercentTest$Consumed)
twopercentTest$ConsumedNorm = bestobj1$x.t

#rerun
twopercentlm = lmer(ConsumedNorm~Treatment + (1|Indiv), twopercentTest)
qqp(resid(twopercentlm))
plot(density(resid(twopercentlm)))
plot(twopercentlm)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(twopercentlm, ddf="Kenward-Roger")

#run with sex for reviewer 1 comments
twopercentlm1 = lmer(ConsumedNorm~Treatment*Sex + (1|Indiv), twopercentTest)
qqp(resid(twopercentlm1))
plot(twopercentlm1)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(twopercentlm1, ddf="Kenward-Roger")

## Subset data into animals during the 0% and 8% Test
zeroAnd8 = conc %>% filter(Pre_test == "Test")
write.csv(zeroAnd8, "SucroseTest.csv") # clean data to remove 0.02, 0.04, 0.06 sucrose animals
 
zeroAnd8Clean = read.csv("SucroseTest.csv")
zeroAnd8Clean = zeroAnd8Clean %>% filter(Sugar_mmt_good == "Y") %>% filter(Water_mmt_good == "Y") 
zeroAnd8Clean = zeroAnd8Clean %>% filter(is.na(zeroAnd8Clean$DopamineStage))

zeroAnd8Clean = zeroAnd8Clean %>% filter(Sugar_conc == 0 | Sugar_conc == 0.08)
zeroAnd8Clean = zeroAnd8Clean %>% select(-DopamineStage)
zeroAnd8Clean = zeroAnd8Clean %>% select(Indiv, Date, Day, Month, Year, DaySinceStart, 
                                         Treatment,Sugar_conc,Sugar_mmt_good,
                                         Water_mmt_good, 
                                         Consumed, Fed_water_g_perday,
                                         Fed_sugarsoln_g_perday, Sex)


write.csv(zeroAnd8Clean, "zeroAnd8Clean.csv") ## remove dates up to 
# 5/16 (phase 1) and up to 8/13 (phase 2) that were part of the 'ramping up' dates

str(zeroAnd8Clean)
zeroAnd8CleanNEW = read.csv("zeroAnd8Clean.csv")

zeroAnd8CleanNEW$Sugar_conc_Char[which(zeroAnd8CleanNEW$Sugar_conc=='0')] = 'Water'
zeroAnd8CleanNEW$Sugar_conc_Char[which(zeroAnd8CleanNEW$Sugar_conc=='0.08')] = "8% Sucrose"

zeroAnd8CleanNEW$Indiv = as.factor(zeroAnd8CleanNEW$Indiv)
zeroAnd8CleanNEW$Treatment = as.factor(zeroAnd8CleanNEW$Treatment)
zeroAnd8CleanNEW$Sex = as.factor(zeroAnd8CleanNEW$Sex)
zeroAnd8CleanNEW$Consumed = as.numeric(zeroAnd8CleanNEW$Consumed)
zeroAnd8CleanNEW$Sugar_conc = as.factor(zeroAnd8CleanNEW$Sugar_conc)

# Look at difference in consumption between 0.00
# FIlter out 8% animals for 0% water preference ####
#sugar consumption preference
NoSugarAnimals = zeroAnd8CleanNEW %>% 
  select(Indiv, Day, Treatment,Sugar_conc,Sugar_mmt_good,Water_mmt_good, 
         Consumed, Fed_water_g_perday,Fed_sugarsoln_g_perday, Sex) %>% 
  filter(Sugar_conc == 0.00) %>% filter(Sugar_mmt_good == "Y") %>% filter(Water_mmt_good == "Y") 
NoSugarAnimals$Indiv = as.factor(NoSugarAnimals$Indiv)
NoSugarAnimals$Day = as.factor(NoSugarAnimals$Day)
NoSugarAnimals$Sex = as.factor(NoSugarAnimals$Sex)

# bottle 1 filter
NosugarAnimalsBottle1 = NoSugarAnimals %>% select(-Fed_sugarsoln_g_perday)
NosugarAnimalsBottle1$Bottle = 1 #add bottle column
colnames(NosugarAnimalsBottle1)[8] = 'Drank' #change column name
#bottle 2 filter
NosugarAnimalsBottle2 = NoSugarAnimals %>% select(-Fed_water_g_perday)
NosugarAnimalsBottle2$Bottle = 2 #add bottle column
colnames(NosugarAnimalsBottle2)[8] = 'Drank'

#merge the frames by ID and Day
NoSugarAnimalsNEW = rbind(NosugarAnimalsBottle1, NosugarAnimalsBottle2)
NoSugarAnimalsNEW$Treatment = as.factor(NoSugarAnimalsNEW$Treatment)
NoSugarAnimalsNEW$Bottle = as.factor(NoSugarAnimalsNEW$Bottle)
NoSugarAnimalsNEW$Indiv = as.factor(NoSugarAnimalsNEW$Indiv)
NoSugarAnimalsNEW$Day = as.factor(NoSugarAnimalsNEW$Day)

#nosugarperlm = lmer(BottlePref ~ (1|Indiv) + (1|Day), NoSugarAnimals, REML = TRUE)
nosugarperlm = lmer(Drank ~ Bottle + Treatment + (1|Indiv) + (1|Day), NoSugarAnimalsNEW, REML = TRUE)
qqp(resid(nosugarperlm))
plot(density(resid(nosugarperlm)))

nosugarperlm = lmer(Drank ~ Bottle + Treatment*Sex + (1|Indiv) + (1|Day), NoSugarAnimalsNEW, REML = TRUE)
qqp(resid(nosugarperlm))
plot(density(resid(nosugarperlm)))

#Normalize and rerun
library(bestNormalize)
bestobj = bestNormalize(NoSugarAnimalsNEW$Drank)
bestobj #
bestobj1 = arcsinh_x(NoSugarAnimalsNEW$Drank)
NoSugarAnimalsNEW$valueNormWater = bestobj1$x.t

nosugarperlm1 = lmer(valueNormWater ~ Bottle + Treatment + (1|Indiv) + (1|Day), NoSugarAnimalsNEW, REML = TRUE)
qqp(resid(nosugarperlm1))
plot(density(resid(nosugarperlm1)))
plot(nosugarperlm1)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(nosugarperlm1, ddf="Kenward-Roger")

#look at sex in response to reviewer 1
nosugarperlm2 = lmer(valueNormWater ~ Bottle + Treatment*Sex + (1|Indiv) + (1|Day), NoSugarAnimalsNEW, REML = TRUE)
qqp(resid(nosugarperlm2))
plot(density(resid(nosugarperlm2)))
plot(nosugarperlm2)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(nosugarperlm2, ddf="Kenward-Roger")

summary(pairs(emmeans(nosugarperlm2, ~Treatment)))
summary(pairs(emmeans(nosugarperlm2, ~Sex)))

# filter out 0% animals ####
EightperAnimals = zeroAnd8CleanNEW  %>% 
  select(Indiv,Day, Treatment,Sugar_conc, Sugar_mmt_good, Water_mmt_good, 
         Consumed, Fed_water_g_perday, Fed_sugarsoln_g_perday, Sex) %>% 
  filter(Sugar_conc == 0.08) %>% filter(Sugar_mmt_good == "Y") %>% filter(Water_mmt_good == "Y")
EightperAnimals$Indiv = as.factor(EightperAnimals$Indiv)
EightperAnimals$Day = as.factor(EightperAnimals$Day)
#bottle 1 filter
EightperAnimalsBottle1 = EightperAnimals %>% select(-Fed_sugarsoln_g_perday)
EightperAnimalsBottle1$Bottle = 1 #add bottle column
colnames(EightperAnimalsBottle1)[8] = 'Drank'
#bottle 2 filter
EightperAnimalsBottle2 = EightperAnimals %>% select(-Fed_water_g_perday)
EightperAnimalsBottle2$Bottle = 2 #add bottle column
colnames(EightperAnimalsBottle2)[8] = 'Drank'

#merge the frames by ID and Day
EightperAnimalsNEW = rbind(EightperAnimalsBottle1, EightperAnimalsBottle2)

str(EightperAnimalsNEW)
EightperAnimalsNEW$Treatment = as.factor(EightperAnimalsNEW$Treatment)
EightperAnimalsNEW$Bottle = as.factor(EightperAnimalsNEW$Bottle)

# relationship of second bottle consumed to the first bottle
eightperlm = lmer(Drank ~ Bottle + Treatment + (1|Indiv) + (1|Day), EightperAnimalsNEW, REML = TRUE)
qqp(resid(eightperlm))
plot(density(resid(eightperlm)))
plot(eightperlm)

eightperlm = lmer(Drank ~ Bottle + Treatment*Sex + (1|Indiv) + (1|Day), EightperAnimalsNEW, REML = TRUE)
qqp(resid(eightperlm))
plot(density(resid(eightperlm)))

library(bestNormalize)
bestobj = bestNormalize(EightperAnimalsNEW$Drank)
bestobj # ordernorm best
bestobj1 = orderNorm(EightperAnimalsNEW$Drank)
EightperAnimalsNEW$valueNormSugar = bestobj1$x.t

eightperlm1 = lmer(valueNormSugar ~ Bottle + Treatment + (1|Indiv) + (1|Day), EightperAnimalsNEW, REML = TRUE)
qqp(resid(eightperlm1))
plot(density(resid(eightperlm1)))
plot(eightperlm1)

# run glmmTMB with beta distribution instead of lmer because of nonnormal distribution
library(glmmTMB)
library(mgcv)
library(DHARMa)
# make Drank values between 0 and 1 for model
EightperAnimalsNEW$DrankNew = EightperAnimalsNEW$Drank/100

eightperlm = glmmTMB(DrankNew ~ Bottle + Treatment + (1|Indiv) + (1|Day), data = EightperAnimalsNEW,
                    family = ordbeta())

car::Anova(eightperlm)

simulationOutput <- simulateResiduals(fittedModel = eightperlm, plot =F)

plot(simulationOutput)
plotQQunif(simulationOutput)

eightperlm1 = glmmTMB(DrankNew ~ Bottle + Treatment + Sex + Treatment*Sex + (1|Indiv) + (1|Day), data = EightperAnimalsNEW,
                     family = ordbeta())

car::Anova(eightperlm1)
summary(pairs(emmeans(eightperlm1, ~Sex)))

simulationOutput <- simulateResiduals(fittedModel = eightperlm1, plot =F)

plot(simulationOutput)
plotQQunif(simulationOutput)

## Figure 4
## Plot 0% and 8% consumption
threeWeekTest = read.csv("zeroAnd8Clean.csv") # open as new data frame
# set factors
threeWeekTest$Treatment = as.factor(threeWeekTest$Treatment)
threeWeekTest$Sugar_conc = as.factor(threeWeekTest$Sugar_conc)
# make consumption as a percentage
threeWeekTest$Consumed = threeWeekTest$Consumed*100
threeWeekTest$Sugar_conc_Char = NA
threeWeekTest$Sugar_conc_Char[which(threeWeekTest$Sugar_conc=='0')] = 'Water'
threeWeekTest$Sugar_conc_Char[which(threeWeekTest$Sugar_conc=='0.08')] = "8% Sucrose"

ggplot(zeroAnd8Clean, aes(Treatment, Consumed)) + 
  geom_boxplot(aes(fill = Treatment), outlier.shape = NA) + 
  geom_point(aes(fill = Treatment), size = 1.5, shape = 21, 
             position = position_jitterdodge()) +
  facet_grid(~factor(Sugar_conc_Char, levels = c("Water", "8% Sucrose"))) + my_theme + 
  scale_fill_manual(values = c("blue", "grey80")) + 
  labs(y = "Proportion consumed of second bottle (%)", x = "Photoperiod") + scale_x_discrete(labels=c("Long" = "Neutral", "Short" = "Short")) + 
  scale_fill_manual(name="Photoperiod", labels=c("Neutral", "Short"), 
                    values = c("#56B4E9", "#D55E00"))

ggsave("0and8.tiff", width =5 , height = 4, dpi = 300)

save.image("Animal_Data.RData")
