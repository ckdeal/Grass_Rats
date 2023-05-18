# MICROBIOME DATA


#Packages
#Part1 
library(here)

#Part 2
#install.packages("phyloseq")
library(phyloseq)
library(ggplot2)
library(ape)

#Part 3
library(qiime2R)
library(tidyverse)
library(plyr)

#Part 4
library(tidyverse)
library(qiime2R)
library(ggrepel) # for offset labels
library(ggtree) # for visualizing phylogenetic trees
library(ape) # for manipulating phylogenetic trees


## Read in processed files

merged_file = read.table("Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/combined_otu_tax.tsv")

## QIIME2
otu <- read.table(("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/otu_table.txt"), header = TRUE)
head(otu)

#	Read in taxonomy table
tax	<- read.table(("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/taxonomy.tsv"), sep = '\t', header = TRUE)
head(tax)

names(otu)[1] <- "OTUID"
names(tax)[1] <- "OTUID"

# Merge files
merged_file <- merge(otu, tax, by.x = c("OTUID"), by.y = c("OTUID"))
head(merged_file)

# Note: the number of rows should equal your shortest file length, drops taxonomy
# for OTUs that don't exist in your OTU table
# # Output merged .txt file
out_path <- ("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/combined_otu_tax.tsv")
write.table(merged_file, file = out_path, sep = "\t", col.names = TRUE, row.names = FALSE)


## Part 2 
# Read in OTU table
otu_table_in <- read.csv(("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/otu_matrix.csv"), sep = ",", row.names = 1)
otu_table_in <- as.matrix(otu_table_in)

# Read in taxonomy
# Separated by kingdom, phylum, class, order, family, genus, species
taxonomy <- read.csv(("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/taxonomy.csv"), sep = ",", row.names = 1)
taxonomy <- as.matrix(taxonomy)

# Read in metadata
metadata <- read.csv(("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/metadata.csv"), row.names = 1, fill=T)
## Found this error in labelling Dec 2020, correcting GrassRat_ID for GRP021 from T5 to T9
## Did this manually for "metadata_cut_for_shannon.csv
metadata$GrassRat_ID[metadata$TubeID=="T9 L.int"] <- "T9"

# Read in tree
phy_tree <- read_tree("/Microbiome_Data/GRP_20191213_16S_qiime/phyloseq/tree.nwk")

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


## Part 3 plots
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
shannon <-read_qza("shannon_vector.qza")

## Sanity check: Check for full overlap between metadata and shannon file
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
Qmeta$Experimental.Trial = as.character(Qmeta$Experimental.Trial)
as.factor(Qmeta$Experimental.Trial)

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
alphalm = lme4::lmer(shannon_entropy ~ Experimental.Trial*Photoperiod + (1|GrassRat_ID), data = Qmeta)

if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(alphalm, ddf="Kenward-Roger")

car::Anova(alphalm, test.statistic = ("F"))
car::qqp(resid(alphalm))
plot(density(resid(alphalm)))
pairs(emmeans(alphalm, ~Experimental.Trial|Photoperiod))

alphalm2 = lme4::lmer(shannon_entropy ~ Experimental.Trial*Photoperiod + (1|ParentPair), data = Qmeta)
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(alphalm2, ddf="Kenward-Roger")

car::qqp(resid(alphalm2))
plot(density(resid(alphalm2)))
pairs(emmeans(alphalm2, ~Experimental.Trial|Photoperiod)) # controlling for parent does not change effect


## Inestine shannon diversity
## Shannon diversity for intestine 
Qmeta <- read.csv(("metadata_cut_for_shannon.csv"), fill=T, stringsAsFactors=FALSE)
shannon <-read_qza("shannon_vector.qza")

## Sanity check: Check for full overlap between metadata and shannon file
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

Qmeta2 = Qmeta %>% filter(Type == "Int") %>% select(-Experimental.Trial) %>%
  filter(GrassRat_ID != "B17" & GrassRat_ID != "N5")

Qmeta3 = Qmeta %>% filter(Type == "Fecal") %>% select(-Experimental.Trial) %>%
  filter(GrassRat_ID != "B17" & GrassRat_ID != "N5")


## plot intestine

my_theme <- theme_classic(base_size = 12) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))
Qmeta2 %>% ggplot(aes(x = PhotoSugar, y = shannon_entropy)) + geom_boxplot(aes(fill = PhotoSugar)) + 
  theme_classic() + labs(y = "Shannon Diversity", x = "Treatment") + my_theme + 
  theme(axis.text.x = element_blank())
## model intestine diversity
## model intestine
Qmeta$Sucrose_long = as.factor(Qmeta$Sucrose_long)
Qmeta2$Photoperiod = as.factor(Qmeta$Photoperiod)

intlm = aov(shannon_entropy ~ Photoperiod*Sucrose_long, Qmeta2)

summary(intlm)
pairs(emmeans(intlm, ~Photoperiod|Sucrose_long))

# PCoA plot
uwunifrac<-read_qza("unweighted_unifrac_pcoa_results.qza")
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size = 3, stroke = 2) +
  my_theme2 +
  scale_color_manual(name="Parent Pair", values=my_colors5) +
  scale_shape_manual(values=c(17,2,19,1,18), name="Photoperiod_Sugar") +
  scale_size_continuous(range = c(1,12), name="Shannon Diversity") +
  facet_grid(.~Trial) +
  labs(x = "PCoA1", y = "PCoA2") + 
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))
ggsave("PCoA.tiff", height=4, width=5, dpi = 300) 
