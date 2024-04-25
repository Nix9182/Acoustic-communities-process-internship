
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

