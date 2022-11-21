### THIS FILE SUMMARISES DATA COLLECTED FOR ELIZA CONGDON-FOLEY's MATSTERS STUDY ON REVEGETATION OUTCOMES

# Project: ELIZA CONGDON-FOLEY's MATSTERS STUDY ON REVEGETATION OUTCOMES  #
# Required data files:  XXXXXX
# 
# Purpose: statistical analysis of revegetation outcomes                  #
# Author: ELIZA CONGDON-FOLEY                                             #
# Date: XXXXXX                                              #


# load required packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(glmmTMB)
library(sjPlot)
library(psych)
library(tidyr)
library(car)
library(emmeans)
library(performance)
library(jtools)

# Set working directory - WERG drive
setwd("~/uomShare/wergProj/Eliza_Thesis_Nov22") # change to match path on your computer

# load data
sitedata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Site info")
beltdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Belt transect counts")
heightdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Tree Heights")
dbhdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "DBH")
stratadata<- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Strata Heights")
canopydata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Canopy")
quadratdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "1m quadrat")
soildata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Soil samples")


#### Q1/2a. NATIVE TREE AND SHRUB Richness ----

# bring in site type into beltdata dataframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]

#BRING IN TRANSECT LENGTH DATA 

#### Calculate the number native tree and shrub species per site
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation',
         nativevegcoveragecounts = 'native veg coverage counts',
         nativevegcoveragepercentage = 'native veg percentage coverage') 


# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)


# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)


# data exploration - belts
glimpse(beltdata)
describe(beltdata)
# describeBy(beltdata, beltdata$site)


# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits))

beltsummarycomplete<-tidyr::complete(beltdatasummary,site, origin, fill=list (richness=0, nostems=0, norecruits=0))

# filter to consider only native
beltdatasummarynative <- filter(beltdatasummary, origin == "Native")
describeBy(beltdatasummarynative, beltdatasummarynative$sitetype)

# graph native and exotic species richness
nativetreerichness <- ggplot(
  data = beltdatasummarynative, aes(x=reorder(site, -richness), y=richness, fill = sitetype)) +
  geom_bar(stat="identity", color= "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Richness of native trees and shrubs") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())


nativetreerichness

#ggsave(nativetreerichness, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativetreerichness.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model it
nativerichnessbysitetypemodel <- glm(richness ~ sitetype, data = beltdatasummarynative, family = poisson(link = "log"))
summary(nativerichnessbysitetypemodel)
r2(nativerichnessbysitetypemodel)

describe(beltdata)





#### Q1/2a. NATIVE TREE AND SHRUB ABUNDANCE ----

nativetreeabundance <- ggplot(
  data = beltdatasummarynative, aes(x=reorder(site,-nostems), y=nostems, fill = sitetype)) +
  geom_bar(stat="identity", color= "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Abundance of native trees and shrubs") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())


nativetreeabundance


# ggsave(nativetreeabundance, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativetreeabundances.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model it
nativeabundancebysitetypemodel <- glm(nostems ~ sitetype, data = beltdatasummarynative, family = poisson(link = "log"))
summary(nativeabundancebysitetypemodel)
r2(nativeabundancebysitetypemodel)




#### Q1/2b. 1m QUADRAT - Native Ground Cover species & other ground cover ----


# bring in site type into quadratdata dataframe

quadratdata$sitetype <- sitedata$'Site type'[match(quadratdata$'Site Name', sitedata$'Site Name')]

describe(quadratdata)

# Rename the different coverage types

quadratdata <- quadratdata %>%
  rename(site = 'Site Name',
         transectno = 'Transect Number',
         beltpoint = 'Belt point',
         exotic = 'Exotic',
         native = 'Native plants',
         coarsewd = 'Coarse WD',
         finewd = 'Fine WD & Litter',
         bareground = 'Bare Ground',
         rockcover = 'Rock Cover',
         water = 'Water') 



# identify factors
quadratdata$site <- as.factor(quadratdata$site)
quadratdata$beltpoint <- as.factor(quadratdata$beltpoint)
quadratdata$sitetype <- as.factor(quadratdata$sitetype)


# identify variables
quadratdata$transectno <- as.numeric(quadratdata$transectno)
quadratdata$exotic <-as.numeric(quadratdata$exotic)
quadratdata$native <-as.numeric(quadratdata$native)
quadratdata$coarsewd <-as.numeric(quadratdata$coarsewd)
quadratdata$finewd <-as.numeric(quadratdata$finewd)
quadratdata$bareground <-as.numeric(quadratdata$bareground)
quadratdata$rockcover <-as.numeric(quadratdata$rockcover)
quadratdata$water <-as.numeric(quadratdata$water)





# summarise
quadratdatasummary <- quadratdata %>%
  group_by(site, sitetype) %>%
  summarise(
    exotic = mean(exotic, na.rm = TRUE), 
    native = mean(native, na.rm = TRUE),
    coarsewd = mean(coarsewd,na.rm = TRUE ),
    finewd = mean(finewd,na.rm = TRUE ),
    bareground = mean(bareground, na.rm = TRUE ),
    rockcover = mean(rockcover, na.rm = TRUE ),
    water = mean(water, na.rm = TRUE)
  )

quadratdatasummary

quadratdatasummaryremnant <- filter(quadratdatasummary, sitetype == "Remnant")
quadratdatasummaryworks <- filter(quadratdatasummary, sitetype == "Works")


describe(quadratdatasummarynative)
describe(quadratdatasummaryworks)

# graph native ground cover percentages

nativegroundcoverpercentage <- ggplot(
  data = quadratdatasummary,  aes(x=reorder(site,-native), y=native, fill = sitetype)) +
  geom_bar(stat="identity", color= "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Percentage native of ground cover") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

nativegroundcoverpercentage


#ggsave(nativegroundcoverpercentage, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativegroundcoverpercentage.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model native ground cover using glmm
quadratdata$nativebeta <- (quadratdata$native+1)/102
nativegroundcoverbysitetypeglmm <- glmmTMB(nativebeta ~ sitetype+(1|site), data = quadratdata, family=beta_family(link="logit"))
summary(nativegroundcoverbysitetypeglmm)
summ(nativegroundcoverbysitetypeglmm)




# graph exotic ground cover percentages

exoticgroundcoverpercentage <- ggplot(
  data = quadratdatasummary,  aes(x=reorder(site,-exotic), y=exotic, fill = sitetype)) +
  geom_bar(stat="identity",colour = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Percentage exotic of ground cover") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

exoticgroundcoverpercentage

#ggsave(exoticgroundcoverpercentage, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/exoticgroundcoverpercentage.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model exotic ground cover using glmm
quadratdata$exoticbeta <- (quadratdata$exotic+1)/102
exoticgroundcoverbysitetypeglmm <- glmmTMB(exoticbeta ~ sitetype+(1|site), data = quadratdata, family=beta_family(link="logit"))
summary(exoticgroundcoverbysitetypeglmm)
r2(exoticgroundcoverbysitetypeglmm)


# graph finewd ground cover percentages
finewdgroundcoverpercentage <- ggplot(
  data = quadratdatasummary,  aes(x=reorder(site,-finewd), y=finewd, fill = sitetype)) +
  geom_bar(stat="identity",colour = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Percentage of fine woody debris ground cover") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())
finewdgroundcoverpercentage

#ggsave(finewdgroundcoverpercentage, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/finewdgroundcoverpercentage.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# model finewd ground cover using glmm
quadratdata$finewdbeta <- (quadratdata$finewd+1)/102
finewdgroundcoverbysitetypeglmm <- glmmTMB(finewdbeta ~ sitetype+(1|site), data = quadratdata, family=beta_family(link="logit"))
summary(finewdgroundcoverbysitetypeglmm)


# graph coarsewd ground cover percentages
coarsewdgroundcoverpercentage <- ggplot(
  data = quadratdatasummary,  aes(x=reorder(site,-coarsewd), y=coarsewd, fill = sitetype)) +
  geom_bar(stat="identity",color = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Percentage of coarse woody debris ground cover") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

coarsewdgroundcoverpercentage

ggsave(coarsewdgroundcoverpercentage, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/coarsewdgroundcoverpercentage.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# model coarsewd ground cover using glmm 
quadratdata$coarsewdbeta <- (quadratdata$coarsewd+1)/102
coarsewdgroundcoverbysitetypeglmm <- glmmTMB(coarsewdbeta ~ sitetype + (1|site), data = quadratdata, family=beta_family(link="logit"))
summary(coarsewdgroundcoverbysitetypeglmm)


# graph  bareground ground cover percentages
baregroundgroundcoverpercentage <- ggplot(
  data = quadratdatasummary,  aes(x=reorder(site,-bareground), y=bareground, fill = sitetype)) +
  geom_bar(stat="identity",color = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Percentage of bare ground cover") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())
baregroundgroundcoverpercentage

ggsave(baregroundgroundcoverpercentage, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/baregroundgroundcoverpercentage.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model  bareground ground cover using glmm 
quadratdata$baregroundbeta <- (quadratdata$ bareground+1)/102
baregroundgroundcoverbysitetypeglmm <- glmmTMB(baregroundbeta ~ sitetype+(1|site), data = quadratdata, family=beta_family(link="logit"))
summary(baregroundgroundcoverbysitetypeglmm)






#### Q1/2b 1m QUADRAT - COMPAIRING AVERAGE COVERAGE TYPE ----

#COMBINED GRAPH
# pivot longer
quadratdatasummarylong <- quadratdatasummary %>%
  pivot_longer(cols = exotic:bareground, names_to = "covertype", values_to = "cover")
quadratdatasummarylong$covertype <- factor(quadratdatasummarylong$covertype, levels = c("native", "exotic", "finewd", "coarsewd", "bareground"))


#summarise
quadratdatalongsummary <- quadratdatasummarylong %>%
  group_by(sitetype, covertype) %>%
  summarise(
    meancover = mean(cover),
    coverse = sd(cover)/sqrt(n()))



# graph it
covertypecomparisonfigure <- ggplot(
  data = quadratdatalongsummary, aes(x=covertype, y= meancover, fill = sitetype)) +
  geom_bar(stat = "identity", aes(fill = sitetype), colour = "black", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin=meancover-coverse, ymax=meancover+coverse), position = position_dodge(0.9, preserve = "single"), width=0.1, size=0.5, color="black") +
  ylim(0, 100) +
  labs(x= NULL, y = "Cover %") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

covertypecomparisonfigure

#ggsave(covertypecomparisonfigure, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/covertypecomparisonfigure.tiff", width = 16, height = 12, units = "cm", dpi = 600)




#### Q1/2c STRATA HEIGHTS - CANOPY COVER VEGETATION HEIGHT DIVERSITY ----

#  CANOPY COVER 


# lets look at the canopy first
# bring in site type into beltdata dataframe
canopydata$sitetype <- sitedata$'Site type'[match(canopydata$'Site Code', sitedata$'Site code')]

# rename 'messy' factor names
canopydata <- canopydata %>%
  rename(site = 'Site Name',
         transect = 'Transect Number',
         beltm = `Belt intercept`,
         canopy = 'Canopy')


# identify factors
canopydata$sitetype <- as.factor(canopydata$sitetype)
canopydata$site <- as.factor(canopydata$site)
canopydata$transect <- as.factor(canopydata$transect)
canopydata$beltm <- as.factor(canopydata$beltm)
canopydata$canopy <- as.factor(canopydata$canopy)
canopydata$canopy <- factor(canopydata$canopy,levels=c("P","A"),labels=c('presence','absence'))

# exclude transects rows with NAs
canopydata <- canopydata[complete.cases(canopydata), ]


# create frequency table of canopy
dffreq <- canopydata %>%
  count(sitetype, site, canopy) %>%
  group_by(site) %>%          
  mutate(prop = prop.table(n))
dffreq <- as.data.frame(dffreq)


# consider only presence
dffreqpres <- filter(dffreq, canopy == "presence")


# freq figure for presence of canopy by siteypte NB. Definitely probably worth separating out East and West sites.
frequencyofcanopy <- ggplot(
  data = dffreqpres, aes(x=reorder(site,-prop), y= prop*100, fill = sitetype)) +   geom_bar(stat = "identity", aes(fill = sitetype), colour = "black", position = position_dodge2(width = 0.9, preserve = "single")) +
  ylim(0, 100) +
  labs(x="Site", y = "Canopy cover %")+
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

frequencyofcanopy

#ggsave(frequencyofcanopy, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/frequencyofcanopy.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# model it
canopybysitetypeglmm <- glmmTMB(prop ~ sitetype, data = dffreqpres, family=beta_family(link="logit"))
summary(canopybysitetypeglmm)


describe(dffreqpres)



### ASSESS VEGETATION STRUCTURE - TREE HEIGHT AND SIZE ----



# bring in site type
heightdata$sitetype <- sitedata$'Site type'[match(heightdata$'Site Code', sitedata$'Site code')]


# rename 'messy' factor names
heightdata <- heightdata %>%
  rename(site = 'Site Name',
         transect = 'Transect Number',
         species = `Species name`,
         height = 'Heights (first 5 of each Sp to 0.5m)')

# identify factors
heightdata$sitetype <- as.factor(heightdata$sitetype)
heightdata$site <- as.factor(heightdata$site)
heightdata$transect <- as.factor(heightdata$transect)
heightdata$species <- as.factor(heightdata$species)


# identify variables
heightdata$height <- as.numeric(heightdata$height)


# summarise
heightdatasummary <- heightdata %>%
  group_by(sitetype, site, species) %>%
  summarise(
    count = n(),
    meanheight = mean(height))

# filter out species for which only 1 plant was surveyed at a site?


# graph raw heights using boxplots?
heightfig <- ggplot(
  data = heightdata, aes(x=species, y= height, fill = sitetype)) +
  geom_boxplot() +
  labs(x= NULL, y = "Height (m)") +
  theme_classic() +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))
heightfig

# there are LOTS of different species... subset?

# graph raw heights using boxplots at sitetype level only?
heightfig2 <- ggplot(
  data = heightdata, aes(x=sitetype, y= height, fill = sitetype)) +
  geom_boxplot() +
  labs(x= NULL, y = "Height (m)") +
  theme_classic() +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90))
heightfig2





### STRATA DATA - ASSESS VEGETATION STRUCTURE - VEGETATION STRUCTURE ----
# load data
sitedata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Site info")
stratadata<- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Strata Heights")

# lets look at the canopy first
# bring in site type into beltdata dataframe
stratadata$sitetype <- sitedata$'Site type'[match(stratadata$'Site Code', sitedata$'Site code')]

# rename 'messy' factor names
stratadata <- stratadata %>%
  rename(site = 'Site Name',
         transect = 'Transect Number',
         beltm = `Belt intercept`,
         height = 'Poll height')


# identify factors
stratadata$sitetype <- as.factor(stratadata$sitetype)
stratadata$site <- as.factor(stratadata$site)
stratadata$transect <- as.factor(stratadata$transect)
stratadata$beltm <- as.factor(stratadata$beltm)
stratadata$height <- as.factor(stratadata$height)
canopydata$canopy <- factor(canopydata$canopy,levels=c("P","A"),labels=c('presence','absemce'))

# identify variables
stratadata$Tree <- as.numeric(stratadata$Tree)
stratadata$Shrub <- as.numeric(stratadata$Shrub)
stratadata$Fern <- as.numeric(stratadata$Fern)
stratadata$Herbaceous <- as.numeric(stratadata$Herbaceous)

# exclude transects rows with NAs
stratadata <- stratadata[complete.cases(stratadata), ] # OR
stratadata <- na.omit(stratadata)

# create count able of different plant types
stratacount <- stratadata %>%
  group_by(sitetype, site, height) %>%          
  summarise(
    count = n(),
    tree = sum(Tree),
    shrub = sum(Shrub),
    fern = sum(Fern),
    herb = sum(Herbaceous))

stratacountworks <- filter(stratacount, sitetype == "Works")


describe(stratacountworks)

# create frequency table of different plant types
stratasum <- stratadata %>%
  group_by(sitetype, height) %>%          
  summarise(
    count = n(),
    tree = sum(Tree)/n(),
    shrub = sum(Shrub)/n(),
    fern = sum(Fern)/n(),
    herb = sum(Herbaceous)/n())


# convert to long form for graphing
stratasumlong <- stratasum %>%
  pivot_longer(cols = tree:herb, names_to = "planttype", values_to = "frequency")


# Figure for frequency of different plant types at different heights by sitetype
# NB. AGAIN, MIGHT BE BEST TO DO THIS WITH EAST V WEST SEPARATED?
# OPTION 1
stratavegtypecomparison1 <- ggplot(
  data = stratasumlong, aes(x=height, y= frequency, fill = sitetype)) +
  geom_bar(stat = "identity", aes(fill = sitetype), colour = "black", position = position_dodge2(width = 0.9, preserve = "single")) +
  ylim(0, 1) +
  labs(x= NULL, y = "Frequency") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~planttype)

stratavegtypecomparison1

#ggsave(stratavegtypecomparison1, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/stratavegtypecomparison1.tiff", width = 16, height = 12, units = "cm", dpi = 600)



# OPTION 2 #### THE ONE I"M USING 
stratasitesypecomparison <- ggplot(
  data = stratasumlong, aes(x=height, y= frequency, fill = planttype)) +
  geom_bar(stat = "identity", aes(fill = planttype), colour = "black", position = position_dodge2(width = 0.9, preserve = "single")) +
  ylim(0, 1) +
  labs(x= NULL, y = "Frequency") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Plant form"))+
  guides(fill=guide_legend(title="Plant form"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~sitetype, ncol = 1)

stratasitesypecomparison

#ggsave(stratasitesypecomparison, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/stratasitesypecomparison.tiff", width = 16, height = 12, units = "cm", dpi = 600)

stratacomparesitetypefig <- ggplot(data =stratasumlong, aes(y=height, x=planttype, fill=frequency, size=frequency, colour = frequency)) +
  geom_point(alpha=0.5, shape=21) +
  scale_size(range = c(1, 30), name="Frequency of hits") +
  facet_wrap(.~sitetype) +
  theme_classic()+
  labs(x= "Plant type", y="Pole Heights")
stratacomparesitetypefig


stratacomparesitetypefigurecircle <- ggplot(data =stratasumlong, aes(y=height, x=planttype, fill=frequency, size=frequency)) +
  geom_point(alpha=1, shape=21, color="black") +
  facet_wrap(.~sitetype) +
  labs(x= "Plant type", y="Pole Heights") +
  scale_fill_continuous(name = "Frequency",  limits=c(0, 1), breaks=seq(0, 1, by=0.2)) + 
  scale_size_continuous(name = "Frequency", range = c(1, 15), limits=c(0, 1), breaks=seq(0, 1, by=0.2)) +
  scale_fill_steps2(
    low = "darkorange", 
    mid = "purple", 
    high = "darkgreen", 
    midpoint = 0.6)+
  guides(fill=guide_colourbar(reverse=TRUE)) + 
  guides(fill=guide_legend(), size = guide_legend()) +
  theme_classic()

stratacomparesitetypefigurecircle

#ggsave(stratacomparesitetypefigurecircle, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/stratacomparesitetypefigurecircle.tiff", width = 16, height = 12, units = "cm", dpi = 600)

#summarise 
stratasumbysite <- stratadata %>%
  group_by(sitetype, site, height) %>%    #  NEED TO ADD SITE AS GROUPING FACTOR HERE     
  summarise(
    count = n(),
    tree = sum(Tree)/n(),
    shrub = sum(Shrub)/n(),
    fern = sum(Fern)/n(),
    herb = sum(Herbaceous)/n())


#model it - tree frequency by height and site type
stratasumbysite$treebeta <- (stratasumbysite$tree+.01)
stratatreehitfrequencyglmm <- glmmTMB(treebeta ~ sitetype * height + (1|site), data = stratasumbysite, family=beta_family(link="logit"))
summary(stratatreehitfrequencyglmm)
Anova(stratatreehitfrequencyglmm)

emmip(stratatreehitfrequencyglmm, height ~ sitetype)

#model it - fern frequency by height and site type
stratasumbysite$fernbeta <- (stratasumbysite$fern+.01)
stratafernhitfrequencyglmm <- glmmTMB(fernbeta ~ sitetype * height + (1|site), data = stratasumbysite, family=beta_family(link="logit"))
summary(stratafernhitfrequencyglmm)
Anova(stratafernhitfrequencyglmm)

emmip(stratafernhitfrequencyglmm, height ~ sitetype)



stratasumbysite$shrubbeta <- (stratasumbysite$shrub+.01)
stratashrubhitfrequencyglmm <- glmmTMB(shrubbeta ~ sitetype * height + (1|site), data = stratasumbysite, family=beta_family(link="logit"))
summary(stratashrubhitfrequencyglmm)
Anova(stratashrubhitfrequencyglmm)

emmip(stratashrubhitfrequencyglmm, height ~ sitetype)

stratasumbysite$herbbeta <- (stratasumbysite$herb+.01)/1.02
strataherbhitfrequencyglmm <- glmmTMB(herbbeta ~ sitetype * height + (1|site), data = stratasumbysite, family=beta_family(link="logit"))
summary(strataherbhitfrequencyglmm)
Anova(strataherbhitfrequencyglmm)

emmip(strataherbhitfrequencyglmm, height ~ sitetype)



#### Q1/2d. RECRUIT ABUNDANCE ----

# bring in site type into beltdata datframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]


#### Calculate the number recruits per site
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation') 


# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)


# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)


# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits))


# filter to consider only native
beltdatasummarynative <- filter(beltdatasummary, origin == "Native")
describeBy(beltdatasummarynative, beltdatasummarynative$sitetype)

# graph native and exotic species richness
recruitnativetreerichness <- ggplot(
  data = beltdatasummarynative, aes(x=reorder(site,-norecruits), y=norecruits, fill = sitetype)) +
  geom_bar(stat="identity", color = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Number of native woody recruits") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

recruitnativetreerichness

#ggsave(recruitnativetreerichness, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/recruitnativetreerichness.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# model it
recruitrichnessbysitetypemodel <- glm(norecruits ~ sitetype, data = beltdatasummarynative, family = poisson(link = "log"))
summary(recruitrichnessbysitetypemodel)
r2(recruitrichnessbysitetypemodel)



describeBy(beltdatasummarynative, beltdatasummarynative$sitetype)













#### Q1/3a. EXOTIC TREE AND SHRUB Richness----


# filter to only consider exotic
beltdatasummaryexotic <- filter(beltdatasummary, origin == "Exotic")
describeBy(beltdatasummaryexotic, beltdatasummaryexotic$sitetype)


exotictreerichness <- ggplot(data = beltdatasummaryexotic, aes(x=reorder(site,-richness), y=richness, fill = sitetype)) +
  geom_bar(stat="identity", color ="black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Richness of exotic trees and shrubs") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

exotictreerichness

#ggsave(exotictreerichness, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/exotictreerichness.tiff", width = 16, height = 12, units = "cm", dpi = 600)

exotictreerichness

#include sites with 0 recorded 

# model it
exoticrichnessbysitetypemodel <- glm(richness ~ sitetype, data = beltdatasummaryexotic, family = poisson(link = "log"))
summary(exoticrichnessbysitetypemodel)
r2(exoticrichnessbysitetypemodel)





#### Q1/3a. EXOTIC TREE AND SHRUB ABUNDANCE ----

exotictreeabundance <- ggplot(
  data =  beltdatasummaryexotic, aes(x=reorder(site,-nostems), y=nostems, fill = sitetype)) +
  geom_bar(stat="identity", color="black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Abundance of exotic trees and shrubs") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

exotictreeabundance

#ggsave(exotictreeabundance, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/exotictreeabundance.tiff", width = 16, height = 12, units = "cm", dpi = 600)


exotictreeabundance

# model it
exoticabundancebysitetypemodel <- glm(nostems ~ sitetype, data = beltdatasummaryexotic, family = poisson(link = "log"))

summary(exoticabundancebysitetypemodel)
r2(exoticabundancebysitetypemodel)

#### Q3. LANDSCAPE CONTEXT - NATIVE WOODY VEGETATION COVERAGE ---

# bring in site type into beltdata dataframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]

#BRING IN TRANSECT LENGTH DATA 

#### Calculate the number native tree and shrub species per site
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation') 


# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)


# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)


# data exploration - belts
glimpse(beltdata)
describe(beltdata)
# describeBy(beltdata, beltdata$site)


# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits))


# filter to consider only native
beltdatasummarynative <- filter(beltdatasummary, origin == "Native")
describeBy(beltdatasummarynative, beltdatasummarynative$sitetype)

# graph native and exotic species richness
nativetreerichness <- ggplot(
  data = beltdatasummarynative, aes(x=reorder(site,-richness), y=richness, fill = sitetype)) +
  geom_bar(stat="identity", color= "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Richness of native trees and shrubs") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())


nativetreerichness

ggsave(nativetreerichness, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativetreerichness.tiff", width = 16, height = 12, units = "cm", dpi = 600)


# model it
nativerichnessbysitetypemodel <- glm(richness ~ sitetype, data = beltdatasummarynative, family = poisson(link = "log"))
summary(richnessbysitetypemodel)


#### Q3 LANDSCAPE CONTEXT - NATIVE WOODY VEGETATION COVERAGE - RECRUIT ABUNDANCE ---- 

beltdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Belt transect counts")

# bring in site type into beltdata dataframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]

#BRING IN TRANSECT LENGTH DATA 

#### Calculate the number native tree and shrub species per site
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation',
         nativevegcoveragecounts = 'native veg coverage counts',
         nativevegcoveragepercentage = 'native veg percentage coverage') 


# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)

# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)
beltdata$nativevegcoveragecounts <- as.numeric(beltdata$nativevegcoveragecounts)
beltdata$nativevegcoveragepercentage <- as.numeric(beltdata$nativevegcoveragepercentage)

# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits),
    nativevegcoveragepercentage = mean(nativevegcoveragepercentage))

# filter to consider only native
beltdatasummarynative <- filter(beltdatasummary, origin == "Native")


beltdatasummarynative <- filter(beltdatasummarynative, sitetype == "Works")

# graph native and exotic species richness
nativewoodyvegcoveragerecruits <- ggplot(data = beltdatasummarynative, aes(x=nativevegcoveragepercentage, y=norecruits)) +
  geom_jitter(size = 2, color = "darkorange3") +
  geom_smooth (method = "glm", method.args = list(family = "poisson"),
               colour = "darkblue") +
  labs(x = 'Percentage of Native Woody Vegetation ', y = "Number of recruits") +
  theme_classic()


nativewoodyvegcoveragerecruits

#ggsave(nativewoodyvegcoveragerecruits, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativewoodyvegcoveragerecruits.tiff", width = 16, height = 12, units = "cm", dpi = 600)



nativevegcovervrecruits <- glm(norecruits ~ nativevegcoveragepercentage, data = beltdatasummarynative,   family = poisson(link = "log"))
summary(nativevegcovervrecruits)
summ(nativevegcovervrecruits)
r2(nativevegcovervrecruits)
describe(beltdatasummary)


#COMPARE TO EXOTIC COVER

groundcovervsnativecoverage <- full_join(quadratdatasummary, beltdatasummary, by='site')

groundcovervsnativecoverageworks <- filter(groundcovervsnativecoverage, sitetype.x == "Works")

describe(groundcovervsnativecoverageworks)

#graph it 
exoticcovervsnativecoveragegraph <- ggplot(data = groundcovervsnativecoverageworks, aes(x=nativevegcoveragepercentage, y=exotic)) +
  geom_jitter(size = 2, color = "darkorange3") +
  
  labs(x = 'Percentage of Native Woody Vegetation', y = "Exotic ground cover (%)") +
  theme_classic()

exoticcovervsnativecoveragegraph

#ggsave(exoticcovervsnativecoveragegraph, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/exoticcovervsnativecoveragegraph.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#model it - need to do beta
groundcovervsnativecoverage$exoticbeta <- groundcovervsnativecoverage$exotic/100

exoticgroundcovervsnativecoverageglm <- glm(exoticbeta ~ nativevegcoveragepercentage, data = groundcovervsnativecoverage,   family = beta_family(link="logit"))
summary(exoticgroundcovervsnativecoverageglm)
summ(exoticgroundcovervsnativecoverageglm)



#COMPARE TO NATIVE COVER


#graph it 
nativecovervsnativecoveragegraph <- ggplot(data = groundcovervsnativecoverageworks, aes(x=nativevegcoveragepercentage, y=native)) +
  geom_jitter(size = 2, color = "darkorange3") +
  
  labs(x = 'Percentage of Native Woody Vegetation', y = "Native ground cover(%)") +
  theme_classic()


nativecovervsnativecoveragegraph

#ggsave(nativecovervsnativecoveragegraph, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nativecovervsnativecoveragegraph.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#model it - need to do beta
groundcovervsnativecoverage$nativebeta <- groundcovervsnativecoverage$native/100

nativecovervsnativecoverageglm <- glm(nativebeta ~ nativevegcoveragepercentage, data = groundcovervsnativecoverage,   family = beta_family())
summary(nativecovervsnativecoverageglm)
summ(nativecovervsnativecoverageglm)


#WHY? maybe because there is more fine woody debris? near native woody vegetation 

finewdcovervsnativecoveragegraph <- ggplot(data = groundcovervsnativecoverageworks, aes(x=nativevegcoveragepercentage, y=finewd)) +
  geom_jitter(size = 2, color = "darkgreen") +
  geom_smooth (method = "lm", se = FALSE, colour = "darkorange2") +
  labs(x = 'Percentage of Native Woody Vegetation', y = "Fine woody debis/leaf litter cover (%)") 

finewdcovervsnativecoveragegraph

#model it - need to do beta
groundcovervsnativecoverage$nativebeta <- groundcovervsnativecoverage$native/100

nativecovervsnativecoverageglm <- glm(nativebeta ~ nativevegcoveragepercentage, data = groundcovervsnativecoverage,   family = beta_family())
summary(nativecovervsnativecoverageglm)
r2(nativecovervsnativecoverageglm)


#### Q3. BROWSER POO FREQUENCY ----

# load data
poodata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Pellet counts")

# bring in site type into poodata dataframe
poodata$sitetype <- sitedata$'Site type'[match(poodata$'Site Code', sitedata$'Site code')]

# rename 'messy' factor names
poodata <- poodata %>%
  rename(site = 'Site Name',
         transect = 'Transect Number',
         belt = `Belt point`)


# identify factors
poodata$sitetype <- as.factor(poodata$sitetype)
poodata$site <- as.factor(poodata$site)
poodata$transect <- as.factor(poodata$transect)
poodata$belt <- as.factor(poodata$belt)


# replace P's and A's with 1s and 0s
poodata <- poodata[!(poodata$Deer == "NA"),] # remove one row with NAs
poodata[poodata  == "P"] <- "1"
poodata[poodata  == "A"] <- "0"


# replace P's and A's with 1s and 0s
poodata <- poodata[!(poodata$Deer == "NA"),] # remove one row with NAs
poodata[poodata  == "P"] <- "1"
poodata[poodata  == "A"] <- "0"
poodata$Kangaroo <- as.numeric(poodata$Kangaroo)
poodata$Wallaby <- as.numeric(poodata$Wallaby)
poodata$Rabbit <- as.numeric(poodata$Rabbit)
poodata$Deer <- as.numeric(poodata$Deer)

# create count able of browser poo counts
poodatacount <- poodata %>%
  group_by(sitetype, site) %>%          
  summarise(
    count = n(),
    kanga = sum(Kangaroo),
    wall = sum(Wallaby),
    rabbit = sum(Rabbit),
    deer = sum(Deer))

# create table of browser poo frequency # NOTE DF NAME SAME AS ABOVE 
poodatacount <- poodata %>%
  group_by(sitetype, site) %>%          
  summarise(
    count = n(),
    kanga = sum(Kangaroo)/count,
    wall = sum(Wallaby)/count,
    rabbit = sum(Rabbit)/count,
    deer = sum(Deer)/count)


# sum across all browser types
poodatacount$total <- poodatacount$kanga + poodatacount$wall + poodatacount$rabbit + poodatacount$deer 


# graph browser frequency 
browsertotal <- ggplot(
  data = poodatacount, aes(x=reorder(site,-total), y=total, fill = sitetype)) +
  geom_bar(stat="identity", colour = "black", position = position_dodge(preserve = "single")) +
  labs(x = 'Site', y = "Frequency of browser pellets") +
  scale_fill_brewer(palette="Dark2")+
  guides(colour=guide_legend(title="Site Type"))+
  guides(fill=guide_legend(title="Site Type"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.ticks.x = element_blank())

browsertotal

#ggsave(browsertotal, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/browsertotale.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# merge poodata and beltdata
beltdatasummarynative$browsers <- poodatacount$total[match(beltdatasummarynative$site, poodatacount$site)]  

beltdatasummarynative <- filter(beltdatasummarynative, sitetype == "Works")

browservrecruit <- ggplot(data = beltdatasummarynative, aes(x=browsers, y=norecruits)) +
  geom_jitter(size = 2, color = "darkorange2") +
  geom_smooth (method = "glm",method.args = list(family = "poisson"),
               colour = "darkblue") +
  labs(x = 'Browser pellet frequency ', y = "Number of native woody recruits") +
  theme_classic()

browservrecruit

#ggsave(browservrecruit, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/browservrecruit.tiff", width = 16, height = 12, units = "cm", dpi = 600)


browservrecruitmodel <- glm(norecruits ~ browsers, data = beltdatasummarynative, family = poisson(link = "log"))
summary(browservrecruitmodel)
r2(browservrecruitmodel)







#### Q3. SOIL ANALYSIS bulk density ----


beltdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Belt transect counts")

# bring in site type into beltdata dataframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]


#### include nutrients into dataset
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation',
         nativevegcoveragecounts = 'native veg coverage counts',
         nativevegcoveragepercentage = 'native veg percentage coverage',
         totalnitrogen = 'total nitrogen',
         totalphosporus = 'total phosporus',
         bulkdensityaverage = 'Bulk density average') 

# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)

# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)
beltdata$nativevegcoveragecounts <- as.numeric(beltdata$nativevegcoveragecounts)
beltdata$nativevegcoveragepercentage <- as.numeric(beltdata$nativevegcoveragepercentage)
beltdata$nativevegcoveragecounts <- as.numeric(beltdata$nativevegcoveragecounts)
beltdata$totalnitrogen <- as.numeric(beltdata$totalnitrogen)
beltdata$totalphosporus <- as.numeric(beltdata$totalphosporus)                                               
beltdata$bulkdensityaverage <- as.numeric(beltdata$bulkdensityaverage) 

# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits),
    nativevegcoveragepercentage = mean(nativevegcoveragepercentage),
    totalnitrogen = mean(totalnitrogen),
    totalphosporus = mean(totalphosporus),
    bulkdensityaverage = mean(bulkdensityaverage))

beltdatasummarynative <- filter(beltdatasummary, origin == "Native")

beltdatasummarynative <- filter(beltdatasummarynative, sitetype == "Works")

describeBy(beltdatasummarynative, beltdatasummarynative$sitetype)

#Graph bulk density to recruits 
bulkdensityvsrecruits <- ggplot(data = beltdatasummarynative, aes(x=bulkdensityaverage, y=norecruits)) +
  geom_jitter(size = 2, color = "darkorange2") +
  
  labs(x = 'Bulk density (g/cm3)', y = "Number of recruits")+
  theme_classic()

bulkdensityvsrecruits

#ggsave(bulkdensityvsrecruits, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/bulkdensityvsrecruitss.tiff", width = 16, height = 12, units = "cm", dpi = 600)



# model it 
bulkdensityvsrecruitsglm <- glm(norecruits ~ bulkdensityaverage, data = beltdatasummarynative,   family = poisson(link = "log"))
summary(bulkdensityvsrecruitsglm)
r2(bulkdensityvsrecruitsglm)

#Graph bulk density to number of stems 
bulkdensityvsnostems <- ggplot(data = beltdatasummarynative, aes(x=bulkdensityaverage, y=nostems)) +
  geom_jitter(size = 2, color = "darkorange2") +
  geom_smooth (method = "glm",method.args = list(family = "poisson"),colour="darkblue")+
  labs(x = 'Bulk density (g/cm3)', y = "Number of trees and shrubs")+
  theme_classic()

bulkdensityvsnostems

#ggsave(bulkdensityvsnostems, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/bulkdensityvsnostems.tiff", width = 16, height = 12, units = "cm", dpi = 600)

# model it 
bulkdensityvsabundanceglm <- glm(nostems ~ bulkdensityaverage, data = beltdatasummarynative,   family = poisson(link = "log"))
summary(bulkdensityvsabundanceglm)
r2(bulkdensityvsabundanceglm)


#### Q3. SOIL ANALYSIS nutrients ----

beltdata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Belt transect counts")

# bring in site type into beltdata dataframe
beltdata$sitetype <- sitedata$'Site type'[match(beltdata$'Site Name', sitedata$'Site Name')]




beltdata$sitetype <- sitedata$'Site type'
#### include nutrients into dataset
beltdata <- beltdata %>%
  rename(site = 'Site Name',
         species = 'Species Name',
         origin = 'Native or Exotic',
         counts = 'Counts',
         recruits = 'Recruits/revegetation',
         nativevegcoveragecounts = 'native veg coverage counts',
         nativevegcoveragepercentage = 'native veg percentage coverage',
         totalnitrogen = 'total nitrogen',
         totalphosporus = 'total phosporus') 

# identify factors
beltdata$site <- as.factor(beltdata$site)
beltdata$species <- as.factor(beltdata$species)
beltdata$origin <- as.factor(beltdata$origin)
beltdata$sitetype <- as.factor(beltdata$sitetype)

# identify variables
beltdata$counts <- as.numeric(beltdata$counts)
beltdata$recruits <-as.numeric(beltdata$recruits)
beltdata$nativevegcoveragecounts <- as.numeric(beltdata$nativevegcoveragecounts)
beltdata$nativevegcoveragepercentage <- as.numeric(beltdata$nativevegcoveragepercentage)
beltdata$nativevegcoveragecounts <- as.numeric(beltdata$nativevegcoveragecounts)
beltdata$totalnitrogen <- as.numeric(beltdata$totalnitrogen)
beltdata$totalphosporus <- as.numeric(beltdata$totalphosporus)                                               

# summarise
beltdatasummary <- beltdata %>%
  group_by(sitetype, site, origin) %>%
  summarise(
    richness = n_distinct(species),
    nostems = sum(counts),
    norecruits = sum(recruits),
    nativevegcoveragepercentage = mean(nativevegcoveragepercentage),
    totalnitrogen = mean(totalnitrogen),
    totalphosporus = mean(totalphosporus))

beltdatasummarynative <- filter(beltdatasummary, origin == "Native")

beltdatasummarynativework <- filter(beltdatasummarynative, sitetype == "Works")                                

#Nitrogen affect on recruits - Graph it
nitrogenvsrecruits <- ggplot(data = beltdatasummarynativework, aes(x=totalnitrogen, y=norecruits)) +
  geom_jitter(size = 2, color = "darkorange2") +
  geom_smooth (method = "glm",method.args = list(family = "poisson"), colour = "darkblue") +
  labs(x = 'Total nitrogen mg/kg ', y = "Number of native woody recruits") +
  theme_classic()

nitrogenvsrecruits

#ggsave(nitrogenvsrecruits, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nitrogenvsrecruits.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#model it
nitrogenvsrecruitsglm <- glm(norecruits ~ totalnitrogen, data = beltdatasummarynativework,   family = poisson(link = "log"))
summary(nitrogenvsrecruitsglm)
r2(nitrogenvsrecruitsglm)
RsqGLM(nitrogenvsrecruitsglm)

#Phosphorus affect on recruits - Graph it 
phosphorusvsrecruits <- ggplot(data = beltdatasummarynativework, aes(x=totalphosporus, y=norecruits)) +
  geom_jitter(size = 2, color = "darkorange3") +
  
  labs(x = 'Total Phosphorus mg/kg ', y = "Number of native woody recruits") +
  theme_classic()

phosphorusvsrecruits

#ggsave(phosphorusvsrecruits, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/phosphorusvsrecruits.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#model it
phosphorusvsrecruitsglm <- glm(norecruits ~ totalphosporus, data = beltdatasummarynative,   family = poisson(link = "log"))
summary(phosphorusvsrecruitsglm)
r2(phosphorusvsrecruitsglm)

# NOW COMPARE SITE TYPE 
nitrogenvsrecruits2 <- ggplot(data = beltdatasummarynative, aes(x=totalnitrogen, y=norecruits)) +
  geom_jitter(size = 2, color = "darkgreen") +
  facet_wrap(.~sitetype)+
  geom_smooth (method = "lm", se = FALSE, colour = "darkorange2") +
  labs(x = 'Total nitrogen mg/kg ', y = "Number of recruits") +
  theme_classic()

nitrogenvsrecruits2

#ggsave(nitrogenvsrecruits2, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nitrogenvsrecruits2.tiff", width = 16, height = 12, units = "cm", dpi = 600)


phosphorusvsrecruits2 <- ggplot(data = beltdatasummarynative, aes(x=totalphosporus, y=norecruits)) +
  geom_jitter(size = 2, color = "darkgreen") +
  facet_wrap(.~sitetype)+
  geom_smooth (method = "lm", se = FALSE, colour = "darkorange2") +
  labs(x = 'Total phosphorus mg/kg ', y = "Number of recruits") +
  theme_classic()

phosphorusvsrecruits2

#ggsave(phosphorusvsrecruits2, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/phosphorusvsrecruits2.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#not sure how to model that?

phosphorusvsrecruits2 <- ggplot(data = beltdatasummarynative, aes(x=totalphosporus, y=norecruits)) +
  geom_jitter(size = 2, color = "darkgreen") +
  facet_wrap(.~sitetype)+
  geom_smooth (method = "lm", se = FALSE, colour = "darkorange2") +
  labs(x = 'Total phosphorus mg/kg ', y = "Number of recruits") +
  theme_classic()

phosphorusvsrecruits2






#### Soil analysis - ground coverage data  ----


soildata <- read_xlsx("Riparian works monitoring data_20220214a.xlsx", sheet = "Soil samples")


soildata$sitetype <- sitedata$'Site type'[match(soildata$'Site Name', sitedata$'Site Name')]


soildata <- soildata %>%
  rename(site = 'Site Name',
         bulkdensityraverage = 'Bulk density average',
         totalnitrogen = 'total nitrogen mg/kg',
         totalphosporus = 'total phosporus mg/kg') 

# identify factors
soildata$site <- as.factor(soildata$site)

# identify variables
soildata$bulkdensityraverage <- as.numeric(soildata$bulkdensityraverage)
soildata$totalnitrogen <- as.numeric(soildata$totalnitrogen)
soildata$totalphosporus <- as.numeric(soildata$totalphosporus)

soildatasummary <- soildata %>%
  group_by(sitetype, site) %>%
  summarise(
    bulkdensityraverage = mean(bulkdensityraverage),
    totalnitrogen = mean(totalnitrogen),
    totalphosporus = mean(totalphosporus))



#merge quadratdata summary and soildata 

covervssoil <- full_join(quadratdatasummary, soildata, by='site')

#compare remant vs work sites
boxplot(totalnitrogen ~ sitetype, data =covervssoil)

boxplot(totalphosporus ~ sitetype, data =covervssoil)

boxplot(bulkdensityraverage ~ sitetype, data =covervssoil)

#restrict analysis to worksites 

covervssoilworks <- filter(covervssoil, sitetype.x == "Works")


#Nitrogen affect on recruits - Graph it
nitrogenvsecoticcover <- ggplot(data = covervssoilworks, aes(x=totalnitrogen, y=exotic)) +
  geom_jitter(size = 2, color = "darkorange2") +
  
  labs(x = 'Total nitrogen (mg/kg) ', y = "Exotic ground cover (%)") +
  theme_classic()


nitrogenvsecoticcover

#ggsave(nitrogenvsecoticcover, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/nitrogenvsecoticcovers.tiff", width = 16, height = 12, units = "cm", dpi = 600)


#model it - need to do beta
covervssoilworks$exoticbeta <- covervssoilworks$exotic/100

nitrogenvsecoticcoverglm <- glm(exoticbeta ~ totalnitrogen, data = covervssoilworks,   family = beta_family())
summary(nitrogenvsecoticcoverglm)


#Phosphorus affect on recruits 
phosphorusvsexoticcover <- ggplot(data = covervssoilworks, aes(x=totalphosporus, y=exotic)) +
  geom_jitter(size = 2, color = "darkorange2") +
  
  labs(x = 'Total phosphorus (mg/kg) ', y = "Exotic ground cover (%)") +
  theme_classic()

phosphorusvsexoticcover

#ggsave(phosphorusvsexoticcover, filename = "C:/Users/Eliza.Foley-Congdon/OneDrive - Water Technology Pty Ltd/Desktop/Eliza Uni/My thesis/phosphorusvsexoticcover.tiff", width = 16, height = 12, units = "cm", dpi = 600)



#model it - need to do beta
covervssoilworks$exoticbeta <- covervssoilworks$exotic/100

phosphorusvsecoticcoverglm <- glm(exoticbeta ~ totalphosporus, data = covervssoilworks,   family = beta_family())
summary(phosphorusvsecoticcoverglm)
r2(phosphorusvsecoticcoverglm)




