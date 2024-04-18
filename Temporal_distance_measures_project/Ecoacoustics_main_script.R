
## Set-up ----
options(digits = 15, dplyr.summarise.inform = FALSE)
Sys.setlocale("LC_ALL", "C")
setwd("C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Code_and_data_Billur/lecture_1") # We will continue working under the lecture_1 directory

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
source("SCRIPT/functions_1_prepareData.R", echo=TRUE)
source("SCRIPT/functions_2a_runAnalysis.R", echo=TRUE)
source("SCRIPT/functions_2b_workflow.R", echo=TRUE)

## Create the output folders ----
folders = c("OUTPUTS/AlpineControl", "OUTPUTS/SubalpineControl", "OUTPUTS/AlpineWarmed")
for(i in folders){
  if(!dir.exists(i)){
    dir.create(i)
  }
}