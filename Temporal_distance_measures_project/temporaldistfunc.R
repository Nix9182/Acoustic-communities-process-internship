### Function to measure temporal distances

#file1 <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/BEARAV/left/L_BEARAV_20140620_000000"
#file2 <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/BEARAV/left/L_BEARAV_20140620_020000"

tdist=function(file){## File: file name with time stamps
  library(tuneR)
  sound.types <- read.table(file, dec=',')
  sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
  sound.dist <- data.frame(matrix(ncol = dim(sound.types)[1], nrow = dim(sound.types)[1]))
  event_id <- c()
  for (i in 1:dim(sound.types)[1]){
    Ti_1 <- sound.types[i,1] #start time of referential event 
    Ti_2 <- sound.types[i,2]
    a <- formatC(i, digits=2,flag="0")
    for (j in c(1:dim(sound.types)[1])[-i]){# all events except the referential
      Tj_1 <- sound.types[j,1] #start time of the compared event
      Tj_2 <- sound.types[j,2]
      if (Tj_2<Ti_1){
        sound.dist[i,j] <- Tj_2-Ti_1
      }
      else if (Ti_2<Tj_1){
        sound.dist[i,j] <- Tj_1-Ti_2
      }
      else {
        sound.dist[i,j] <- 0
      }
    }
    event_id <- append(event_id, paste(sound.types[i,3], file, a, sep='_'))
    names(sound.dist)[1:dim(sound.types)[1]] <- paste('dist_event_nb', as.character(1:dim(sound.types)[1]), sep='_')
    #names(sound.dist)[dim(sound.types)[1]+1] <- "event_id"
  }
  sound.dist <- cbind(event_id, sound.dist)
  return(sound.dist)
}

#df <- tdist(file1)
#df2 <- tdist(file2)  
#dftest <- data.frame(matrix(ncol = 0, nrow = 0))
#library(tidyverse)
#df_tot1 <- bind_rows(dftest, df,df2)
