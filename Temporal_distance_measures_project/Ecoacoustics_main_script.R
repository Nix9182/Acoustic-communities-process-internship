
## Set-up ----
options(digits = 15, dplyr.summarise.inform = FALSE)
Sys.setlocale("LC_ALL", "C")
setwd("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project") 
path <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Donn�es/files/"
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
  
  library(ggplot2)
  library(patchwork)
  library(corrplot)
}

## Source functions ----
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_prepare_data.R", echo=TRUE)
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_runAnalysis.R", echo=TRUE)
source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/Ecoac_functions_workflow.R", echo=TRUE)

## Create the output folders ----
folders = c("OUTPUTS")
for(i in folders){
  if(!dir.exists(i)){
    dir.create(i)
  }
}

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

## Get soundtype n� in traits
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
Acous.traits = fun_gaussianizeTraits(Acous.traits, traits_names, setype = "hh")

## Check trait distributions and trait space with a PCA
tr_names = c(duration = "Duration",
             dom_freq ="Dominant frequency")
ggdata = melt(Acous.traits[, names(tr_names)])
ggdata = ggdata %>% mutate(variable = recode(variable, !!!tr_names))
ggplot(ggdata, aes(x = value)) +
  facet_grid(.~variable, scales = "free_x") +
  geom_histogram(alpha = 0.5) +
  theme_bw() +
  xlab("Log and gaussian transformed trait values") +
  labs(fill = "Acoustic traits")

res = Acous.traits[, names(tr_names)] %>% drop_na()
corrplot(cor(res))
