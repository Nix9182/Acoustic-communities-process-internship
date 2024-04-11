library(tidyverse)

source("C:/Users/cobod/OneDrive/Bureau/Ecoacoustic project internship/Acoustic-communities-process-internship/Temporal_distance_measures_project/temporaldistfunc.R")

site <- c('BEARAV', 'GRANAM', 'MOIRAM', 'MORTCE', 'ROSSAM', 'VILOAM')
channel <- c('left', 'right')
df_tot <- data.frame(matrix(ncol = 0, nrow = 0))

for (i in site){
  for (j in channel){
    setwd(paste("C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/", i, "/", j, sep=""))
    for (k in list.files(pattern="*[0-9]$")){
      df_k <- data.frame(tdist(k)$all.dist)
      df_tot<-bind_rows(df_tot, df_k)
    }
  }
}
