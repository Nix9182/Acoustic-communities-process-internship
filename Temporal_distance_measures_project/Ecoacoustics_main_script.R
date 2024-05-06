
## Set-up ----
options(digits = 15, dplyr.summarise.inform = FALSE)
Sys.setlocale("LC_ALL", "C")
#Setwd where all the functions are
setwd("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project") 

#path to where the folders for each site are 
path <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/"
site <- c('BEARAV', 'GRANAM', 'MOIRAM', 'MORTCE', 'ROSSAM', 'VILOAM')
channel <- c('left', 'right')

## Load libraries ----
{
  library(foreach)
  library(iterators) ## icount
  library(reshape2)
  
  library(purrr) ## reduce
  library(dplyr) ## %>% full_join mutate mutate_at recode group_by ungroup summarize anti_join
  library(tidyr) ## drop_na
  
  library(LambertW) ## Gaussianize
  library(heatmaply) ## normalize
  
  library(scales)
  library(ggplot2)
  library(patchwork)
  library(corrplot)
  library(ggpubr)
  library(grid)
  library(gridExtra)
}

## Source functions ----
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_prepare_data.R", echo=TRUE)
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_runAnalysis.R", echo=TRUE)
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_workflow.R", echo=TRUE)

#############################################################################################################
## I. PREPARATION OF DATA
#############################################################################################################

## A. Load traits data ------------------------------------------------------------------
Acous.measures = read.csv("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/mesures_soundtypes_updated.csv", sep=";", dec=",")


Acous.measures <- Acous.measures[1:(length(Acous.measures)-5)] #focus on duration and dom.frequency
names(Acous.measures)[1] <- "sound_type"

Acous.traits <- subset(Acous.measures, select = -c(id) )
Acous.traits <- aggregate(.~sound_type, data=Acous.traits, mean) #get mean trait for each sound type
Acous.traits$sound_type <- lapply(Acous.traits$sound_type, formatC, digits=2,flag="0")

## Get soundtype n° in traits
tr.st_names = unique(Acous.traits$sound_type)
tr.st_no = length(tr.st_names)

## Get trait names
traits_names = c("duration", "dom_freq")

## Prepare traits (log transformation + gaussianize)
# We normalize and scale the traits to be able to compare and perform statistical tests.


Acous.traits = Acous.traits %>%
  mutate_at(traits_names, as.numeric)%>%
  mutate_at(traits_names, log) %>%
  mutate_at(traits_names, as.numeric)
rownames(Acous.traits) = Acous.traits$sound_type

Acous.traits$duration <- scale(Acous.traits$duration)
Acous.traits$dom_freq <- scale(Acous.traits$dom_freq)
#Acous.traits = fun_gaussianizeTraits(Acous.traits, traits_names, setype = "hh")

## Check trait distributions and trait space with a PCA
tr_names = c(duration = "Duration",
             dom_freq ="Dominant frequency")
ggdata = melt(Acous.traits[, names(tr_names)])
ggdata = ggdata %>% mutate(variable = recode(variable, !!!tr_names))
ggplot(ggdata, aes(x = value)) +
  facet_grid(.~variable, scales = "free_x") +
  geom_histogram(alpha = 0.5) +
  theme_bw() +
  xlab("Log-transformed and scaled trait values") +
  labs(fill = "Acoustic traits" , title= "Distribution within soundtype pool")+
  theme(plot.title = element_text(hjust = 0.5))

res = Acous.traits[, names(tr_names)] %>% drop_na()
corrplot(cor(res))

## Calculate distances between sound types for each trait
# We calculate the differences between each soundtype for all traits at once to gain time.
# For each focal and neighbor, we will later call these differences.
for (tr in traits_names){
  df = as.data.frame(Acous.traits[, tr, drop = FALSE]) #df with only one trait (one column) with species names as index (keeps only one trait in Acous.traits)
  dist_tr = outer(df[, 1], df[, 1], "-") #outputs a data frame resulting of the difference between all soundtypes (all combinations possible)
  colnames(dist_tr) = rownames(dist_tr) = rownames(df)
  assign(x = paste0("dist", tr), value = dist_tr) #each distTRAIT for ex: distduration, get assigned the distance df for this trait
}

## Gather all distances
list.acousdist = list(duration = distduration,
                 dom_freq= distdom_freq
                 )

## Build the regional soundtype pool per traits
# We avoid selecting the soundtype with no trait information.
list.acouspool = list(duration = tr.st_names[tr.st_names %in% Acous.traits$sound_type[which(!is.na(Acous.traits$duration))]],
                 dom_freq = tr.st_names[tr.st_names %in% Acous.traits$sound_type[which(!is.na(Acous.traits$dom_freq))]])


## B. Gather surveys data -----------------------------------------------------------------
#Create the releves table (with nobs)
# Columns of interest: 
# channel: left or right hydrophone
# site: site of recording
# date: date of the recording
# hour: hour of the recording (=pinpoint)
# type: soundtype
# nbobs: Abundances
site <- c('BEARAV', 'GRANAM', 'MOIRAM', 'MORTCE', 'ROSSAM', 'VILOAM')
channel <- c('left', 'right')

Acous.info <- data.frame(infoday(path, site, channel))

Acous.releves <- foreach(c=channel, .combine=rbind)%do%{
  foreach(s=site, .combine=rbind)%do%{
    foreach(d=unique(Acous.info$date), .combine=rbind)%do%{
      fun_getUniqueSt(Acous.info, c, s, d)
    }
  }
}

## Get sountype names in releves
ab.st_names = unique(Acous.releves$type)
ab.st_no = length(ab.st_names)


## C. Create random communities per species ---------------------------------------------

## Amount of random communities wanted (SES calculations)
# We would normally need more but increase the numbers depending on your computer.
no.rand = 5 # Before increasing this number to 50 (previously it was 5) let's discuss this during the lecture. 

## Create the test and random soundtypes list at once
# We randomize the observed soundtypes "no.rand" times to create the null neighborhoods.
list.acouscomrand = foreach(i = 1:no.rand, .combine = "cbind") %do% {
    sample(list.acouspool[[tr.dim]], size = ab.st_no, replace = FALSE)
  }
list.acouscomrand = data.frame(Obs = ab.st_names, list.acouscomrand)
colnames(list.acouscomrand) = c("Obs", paste0("Null_", 1:no.rand))
rownames(list.acouscomrand) = list.acouscomrand$Obs


## D. Compute SES for each site ---------------------------------------------

#####DO NOT RUN EXCEPT FOR TESTS (MIGHT TAKE TIME) ######################################################
# test.Neigh <- FUN1_getNeigh(Acous.releves, site, date, no.neigh)
# new.Neigh <- FUN2_getAcousMetrics(test.Neigh, traits, list.acousdist, list.acouspool)
# test.Rand <- fun_getRandAcousComm(new.Neigh, traits, list.acouscomrand, no.rand, list.acouspool)
# test.SES <- FUN_calcSes(test.Rand)
#########################################################################################################

traits <- c("duration","dom_freq")
channel = "left"
site = "BEARAV"
date = "20140620"
no.neigh = -4:4

#test.Neigh <- FUN1_getNeigh(Acous.releves, channel, site, date, no.neigh)
#new.Neigh <- FUN2_getAcousMetrics(test.Neigh, traits, list.acousdist, list.acouscomrand, list.acouspool)
#test.Rand <- fun_getRandAcousComm(new.Neigh, traits, list.acouscomrand, no.rand, list.acouspool)
#test.SES <- FUN_calcSes(test.Rand)

test.final <- FUNCTION_Comm(channel=channel,
                            site=site,
                            releves= Acous.releves,
                            no.neigh = no.neigh, 
                            traits = traits,
                            list.comrand = list.acouscomrand,
                            list.dist = list.acousdist,
                            list.pool = list.acouspool)

#test plot of SES for each level 
df.agreg <- aggregate(SES ~ L, data=test.final[["dom_freq"]], mean)
plot(df.agreg$L,df.agreg$SES)

