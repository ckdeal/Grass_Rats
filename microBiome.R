# MICROBIOME DATA
library(here)
library(phyloseq)
library(ggplot2)
library(ape)
library(qiime2R)
library(tidyverse)
library(plyr)
library(tidyverse)
library(qiime2R)
library(ggrepel) # for offset labels
library(ggtree) # for visualizing phylogenetic trees
library(ape) # for manipulating phylogenetic trees

# read in theme and colors
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 10) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_colors5 <- c("#004e64", "#ffba08", "#73a580", "#f786aa", "#685369")

## Read in processed files
merged_file = read.table("combined_otu_tax.tsv")

## QIIME2
otu <- read.table(("otu_table.txt"), header = TRUE)
head(otu)

#	Read in taxonomy table
tax	<- read.table(("taxonomy.tsv"), sep = '\t', header = TRUE)
head(tax)

names(otu)[1] <- "OTUID"
names(tax)[1] <- "OTUID"

# Merge files
merged_file <- merge(otu, tax, by.x = c("OTUID"), by.y = c("OTUID"))
head(merged_file)

# Note: the number of rows should equal your shortest file length, drops taxonomy
# for OTUs that don't exist in your OTU table
# # Output merged .txt file
out_path <- ("combined_otu_tax.tsv")
write.table(merged_file, file = out_path, sep = "\t", col.names = TRUE, row.names = FALSE)


## Part 2 
# Read in OTU table
otu_table_in <- read.csv(("otu_matrix.csv"), sep = ",", row.names = 1)
otu_table_in <- as.matrix(otu_table_in)

# Read in taxonomy
# Separated by kingdom, phylum, class, order, family, genus, species
taxonomy <- read.csv(("taxonomy.csv"), sep = ",", row.names = 1)
taxonomy <- as.matrix(taxonomy)

# Read in metadata
metadata <- read.csv(("metadata.csv"), row.names = 1, fill=T)
## Found this error in labelling Dec 2020, correcting GrassRat_ID for GRP021 from T5 to T9
## Did this manually for "metadata_cut_for_shannon.csv
metadata$GrassRat_ID[metadata$TubeID=="T9 L.int"] <- "T9"

# Read in tree
phy_tree <- read_tree("tree.nwk")

# Import all as phyloseq objects
OTU <- otu_table(otu_table_in, taxa_are_rows = TRUE)
TAX <- tax_table(taxonomy)
META <- sample_data(metadata, )

# Sanity checks for consistent OTU names
taxa_names(TAX)
taxa_names(OTU)
taxa_names(phy_tree)

# Same sample names
sample_names(OTU)
sample_names(META)

# Finally merge!
ps <- phyloseq(OTU, TAX, META, phy_tree)
ps


## Plots
library(phyloseq)
library(ggplot2)
library(ape)
library(qiime2R)
library(tidyverse)
library(plyr)
library(tidyverse)
library(qiime2R)
library(ggrepel) # for offset labels
library(ggtree) # for visualizing phylogenetic trees
library(ape) # for manipulating phylogenetic trees

Qmeta <- read.csv(("metadata_cut_for_shannon.csv"), fill=T, stringsAsFactors=FALSE)
shannon <- read_qza("shannon_vector.qza")

## Check for full overlap between metadata and shannon file
shannon<-shannon$data %>% rownames_to_column("SampleID") 
gplots::venn(list(metadata=Qmeta$SampleID, shannon=shannon$SampleID))

Qmeta$Sucrose_long <- Qmeta$Sucrose
Qmeta$Sucrose_long <- revalue(Qmeta$Sucrose_long, c("yes"="HighSucrose", "no" = "NoSucrose"))
Qmeta$PhotoSugar <- paste(Qmeta$Photoperiod, Qmeta$Sucrose_long, sep="_")
Qmeta$PhotoSugar[Qmeta$PhotoSugar=="NA_NA"] <- "NA"

# Join metadata and shannon
Qmeta<-  Qmeta %>% 
  left_join(shannon, by = "SampleID")
head(Qmeta)

## 
Qmeta = Qmeta %>% filter(Type == "Fecal")
Qmeta$Experimental.Trial = as.factor(Qmeta$Experimental.Trial)
as.factor(Qmeta$Experimental.Trial)
Qmeta$Photoperiod = as.factor(Qmeta$Photoperiod)
Qmeta$Sucrose = as.factor(Qmeta$Sucrose)

Qmeta$Experimental.Trial = recode_factor(Qmeta$Experimental.Trial, "2wk acclim" = "0", 
                                         "4wk photoperiod" = "4", "0% HCS" = "7",
                                         "8% HCS" = "7") # reorder/rename levels


## Plot shannon diversity over time
ggplot(Qmeta, aes(x=Experimental.Trial, y=shannon_entropy, color=PhotoSugar, group = PhotoSugar)) +
  stat_summary(geom="errorbar", fun.data=mean_se, width=0, size=1) +
  stat_summary(geom="line", fun.data=mean_se, size=1.2, alpha=0.8) +
  stat_summary(geom="point", fun.data=mean_se) +
  xlab("Time fecal sample collected (weeks)") +
  ylab("Shannon Diversity") +
  #scale_x_discrete(name ="Experimental Trial", 
  #limits=c("2w acclim.", "4wk photoperiod", "0% HCS", "8% HCS")) +
  theme_classic() + # try other themes like theme_bw() or theme_classic()
  scale_color_viridis_d(name="Photoperiod_Sugar") # use different color scale which is color blind friendly
ggsave("Shannon_by_treatment.tiff", height=3, width=5, dpi=300)
dev.off()

## model shannon diversity by treatment and photoperiod
#set factors
# just run trial 2
alphalm = lmer(shannon_entropy ~ Experimental.Trial*Photoperiod*Sucrose + (1|GrassRat_ID), data = Qmeta, REML = TRUE)

car::qqp(resid(alphalm))
plot(density(resid(alphalm)))

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(alphalm, ddf="Kenward-Roger")

alphalm2 = lmer(shannon_entropy ~ Experimental.Trial*Photoperiod*Sucrose + (1|GrassRat_ID) + (1|ParentPair), data = Qmeta, REML = TRUE)
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(alphalm2, ddf="Kenward-Roger")

car::qqp(resid(alphalm2))
plot(density(resid(alphalm2)))

## Inestine shannon diversity
## Shannon diversity for intestine 
Qmeta <- read.csv(("metadata_cut_for_shannon.csv"), fill=T, stringsAsFactors=FALSE)
shannon <-read_qza("shannon_vector.qza")

## Check for full overlap between metadata and shannon file
shannon<-shannon$data %>% rownames_to_column("SampleID") 
gplots::venn(list(metadata=Qmeta$SampleID, shannon=shannon$SampleID))


Qmeta$Sucrose_long <- Qmeta$Sucrose
Qmeta$Sucrose_long <- revalue(Qmeta$Sucrose_long, c("yes"="HighSucrose", "no" = "NoSucrose"))
Qmeta$PhotoSugar <- paste(Qmeta$Photoperiod, Qmeta$Sucrose_long, sep="_")
Qmeta$PhotoSugar[Qmeta$PhotoSugar=="NA_NA"] <- "NA"

# Join metadata and shannon
# Join metadata and shannon
Qmeta<-  Qmeta %>% 
  left_join(shannon, by = "SampleID")
head(Qmeta)

Qmeta2 = Qmeta %>% filter(Type == "Int") %>% select(-Experimental.Trial) %>%
  filter(GrassRat_ID != "B17" & GrassRat_ID != "N5")

my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

Qmeta2 %>% ggplot(aes(x = PhotoSugar, y = shannon_entropy)) + geom_boxplot(aes(fill = PhotoSugar)) + 
  geom_point(aes(fill = PhotoSugar), size = 1.5, shape = 21, 
             position = position_jitterdodge()) +
  theme_classic() + labs(y = "Shannon Diversity", x = "Treatment") + my_theme + 
  theme(axis.text.x = element_blank()) 

ggsave("intshannon.png", width = 5, height = 4, dpi = 800)

## model intestine diversity
## model intestine
Qmeta2$Sucrose_long = as.factor(Qmeta2$Sucrose_long)
Qmeta2$Photoperiod = as.factor(Qmeta2$Photoperiod)

intlm = aov(shannon_entropy ~ Photoperiod*Sucrose_long, Qmeta2)

summary(intlm)
pairs(emmeans(intlm, ~Photoperiod|Sucrose_long))

# PCoA plot
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  #filter(Trial==1) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=3, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme2 + facet_grid(.~Type) +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  scale_color_manual(name="Parent Pair", values=my_colors5) +
  #facet_grid(.~Trial) +
  #geom_text(aes(label=paste(GrassRat_ID, Sample, sep = ", ")), alpha=1, col='black') +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))

ggsave("PCoA.png", height=4, width=7, dpi = 800) 


# correlate microbiome with liver steatosis
Qmeta <- read.csv(("metadata_cut_for_shannon.csv"), fill=T, stringsAsFactors=FALSE)

Qmeta$Sucrose_long <- Qmeta$Sucrose
Qmeta$Sucrose_long <- revalue(Qmeta$Sucrose_long, c("yes"="HighSucrose", "no" = "NoSucrose"))
Qmeta$PhotoSugar <- paste(Qmeta$Photoperiod, Qmeta$Sucrose_long, sep="_")
Qmeta$PhotoSugar[Qmeta$PhotoSugar=="NA_NA"] <- "NA"

# Join metadata and shannon
Qmeta<-  Qmeta %>% 
  left_join(shannon, by = "SampleID")
head(Qmeta)
## 
Qmeta$Experimental.Trial = as.character(Qmeta$Experimental.Trial)
as.factor(Qmeta$Experimental.Trial)

Qmeta$Experimental.Trial = recode_factor(Qmeta$Experimental.Trial, "2wk acclim" = "0", 
                                         "4wk photoperiod" = "4", "0% HCS" = "7",
                                         "8% HCS" = "7") # reorder/rename levels

Qmeta2 = Qmeta %>% filter(Type == "Int") %>%
  filter(GrassRat_ID != "B17" & GrassRat_ID != "N5") %>% select(-Experimental.Trial)

Qmeta3 = Qmeta %>% filter(Type == "Fecal") %>% 
  filter(GrassRat_ID != "B17" & GrassRat_ID != "N5")

# Correlate fecal shannon diversity and percent liver steatosis
fecalMicroANDliver = lmer(shannon_entropy ~ PercentArea_liver_fat + (1|GrassRat_ID) + (1|Experimental.Trial), Qmeta3, REML = TRUE)
qqp(resid(fecalMicroANDliver))

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(fecalMicroANDliver, ddf="Kenward-Roger")

fecalMicroANDliver2 = lmer(shannon_entropy ~ PercentArea_liver_fat + (1|GrassRat_ID) + (1|Experimental.Trial) + (1|ParentPair), Qmeta3, REML = TRUE)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(fecalMicroANDliver2, ddf="Kenward-Roger")

plot(shannon_entropy ~ PercentArea_liver_fat, Qmeta)

# intestine
intMicroANDliver = lmer(shannon_entropy ~ PercentArea_liver_fat + (1|ParentPair), Qmeta2, REML = TRUE)
qqp(resid(intMicroANDliver))

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(intMicroANDliver, ddf="Kenward-Roger")

## Micriobiome bar plots by treatment ####
ubiome_rarified = read.csv("GRP_20191213.rarefied-class-feature-table.csv")
meta = read.csv("GRP_20191213.metadata_AS.csv")
metadata = read.csv("GRP_20191213.metadata.csv")
meta_litter = read.csv("Meta_litter_parents.csv")

## Subsetting metadata to useful columns
litterEndCol = which(colnames(meta_litter)=="Percent_liver_fat")
meta_litter = meta_litter[,1:litterEndCol]
metaEndCol = which(colnames(meta)=="Sugar")
mmeta = meta[,1:metaEndCol]
mmeta_litter = merge(mmeta, meta_litter, by="GrassRat_ID")

Startcol = which(colnames(ubiome_rarified)=="GR_Total")
Endcol = which(colnames(ubiome_rarified)=="NegPlateB_G07")
m.ubiome = ubiome_rarified[,-c(Startcol:Endcol)] %>% ## using tidyr for melting
  gather(SampleID, value, GRP009:GRP182)

m.ubiome_cleaned = m.ubiome[!is.na(m.ubiome$D2) & m.ubiome$D2 != "", ]

merge_ubiome = merge(mmeta_litter, m.ubiome_cleaned, by = "SampleID")
head(merge_ubiome)

ggplot(merge_ubiome, aes(x = Daylength, y = value, fill = D2)) +
  geom_bar(stat = "identity") +
  labs(x = "Treatment", y = "Count", fill = "Microbiome Class") +
  theme_bw() + facet_grid(~Sugar, labeller = labeller(Sugar = c("High" = "8% Sucrose", "None" = "Water"))) + 
  theme(legend.title = element_text(size = 10, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"),   # Adjust the size of the legend key
        legend.text = element_text(size = 7))  # Adjust the size of the legend text
ggsave("microbiomeBYclass.tiff", height=4, width=7, dpi = 600)

#####
meltmicroFeatures1 = data.frame(Class = microFeatures$D2, microFeatures[,8:173])
meltmicroFeatures = melt(meltmicroFeatures1, id.vars = c("Class"))
meltmicroFeaturesMerge = merge(meltmicroFeatures, metadata)

class_summary <- meltmicroFeaturesMerge %>%
  group_by(Class, Photoperiod, Sugar) %>%
  mutate(Total_Counts = sum(value),
         Proportion = value / sum(value))

aggregated_df <- aggregate(Proportion ~ Class + Photoperiod + Sugar + Type + SampleID, data = class_summary, FUN = sum)
aggregated_df = aggregated_df[-c(1,24,47,70, 93, 116, 139, 162),]
aggregated_df = aggregated_df %>%filter(Type == "Int")

## MAKE COUNTS INTO PROPORTIONS
ggplot(aggregated_df, aes(x = Photoperiod, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(x = "Treatment", y = "Count", fill = "Microbiome Class") +
  theme_bw() + facet_grid(~Sugar, labeller = labeller(Sugar = c("High" = "8% Sucrose", "None" = "Water"))) + 
  theme(legend.title = element_text(size = 10, hjust = 0.5), 
        legend.key.size = unit(0.5, "cm"),   # Adjust the size of the legend key
        legend.text = element_text(size = 7))  # Adjust the size of the legend text
ggsave("microbiomeBYclass.tiff", height=4, width=7, dpi = 600)




