### Function to measure temporal distances
# 
file1 <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/BEARAV/left/L_BEARAV_20140620_000000"
# file2 <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/BEARAV/left/L_BEARAV_20140620_020000"

tdist=function(file){## File: file name with time stamps
  library(tuneR)
  sound.types <- read.table(file, dec=',')
  sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
  sound.dist <- data.frame(matrix(ncol = dim(sound.types)[1], nrow = dim(sound.types)[1]))
  all.dist <- c()
  overlap.perc <- c()
  all.overlap.perc <- c()
  event_id <- c()
  focal_id <- c()
  neighbour_id <- c()
  all_focal <- c()
  all_neighbour <- c()
  for (i in 1:dim(sound.types)[1]){
    Ti_1 <- sound.types[i,1] #start time of referential event 
    Ti_2 <- sound.types[i,2]
    a <- formatC(i, digits=2,flag="0")
    for (j in c(1:dim(sound.types)[1])[-i]){# all events except the referential
      Tj_1 <- sound.types[j,1] #start time of the compared event
      Tj_2 <- sound.types[j,2]
      b <- formatC(j, digits=2,flag="0")
      all_focal <- append(all_focal, paste(sound.types[i,3], file, a, sep='_'))
      all_neighbour <- append(all_neighbour, paste(sound.types[j,3], file, b, sep='_'))
      if (Tj_2<Ti_1){
        sound.dist[i,j] <- Tj_2-Ti_1
        all.dist <- append(all.dist, Tj_2-Ti_1)
        all.overlap.perc <- append(all.overlap.perc, 0)
      }
      else if (Ti_2<Tj_1){
        sound.dist[i,j] <- Tj_1-Ti_2
        all.dist <- append(all.dist, Tj_1-Ti_2)
        all.overlap.perc <- append(all.overlap.perc, 0)
      }
       else if ((Tj_1<=Ti_1) & (Ti_1<=Tj_2) & (Tj_2<=Ti_2)) {#i begins during j and ends after j
         sound.dist[i,j] <- 0
         all.dist <- append(all.dist, 0)
         overlap <- 100*(Tj_2-Ti_1)/(Ti_2-Ti_1)
         overlap.perc <- append(overlap.perc, overlap)
         all.overlap.perc <- append(all.overlap.perc, overlap)
         focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
         neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
       }
       else if ((Ti_1<=Tj_1) & (Tj_1<=Ti_2) & (Ti_2<=Tj_2)){#i begins before j and ends during j
         sound.dist[i,j] <- 0
         all.dist <- append(all.dist, 0)
         overlap <- 100*(Ti_2-Tj_1)/(Ti_2-Ti_1)
         overlap.perc <- append(overlap.perc, overlap)
         all.overlap.perc <- append(all.overlap.perc, overlap)
         focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
         neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
       }
       else if ((Ti_1<=Tj_1) & (Tj_1<=Ti_2) & (Ti_1<=Tj_2) & (Tj_2<=Ti_2)) {#j begins and ends during i
         sound.dist[i,j] <- 0
         all.dist <- append(all.dist, 0)
         overlap <- 100*(Tj_2-Tj_1)/(Ti_2-Ti_1)
         overlap.perc <- append(overlap.perc, overlap)
         all.overlap.perc <- append(all.overlap.perc, overlap)
         focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
         neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
        }
       else if ((Tj_1<=Ti_1) & (Ti_1<=Tj_2) & (Tj_1<=Ti_2) & (Ti_2<=Tj_2)) {#i beings and ends during j
         sound.dist[i,j] <- 0
         all.dist <- append(all.dist, 0)
         overlap <- 100
         overlap.perc <- append(overlap.perc, overlap)
         all.overlap.perc <- append(all.overlap.perc, overlap)
         focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
         neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
       }
    }
    event_id <- append(event_id, paste(sound.types[i,3], file, a, sep='_'))
    names(sound.dist)[1:dim(sound.types)[1]] <- paste('dist_event_nb', as.character(1:dim(sound.types)[1]), sep='_')
    #names(sound.dist)[dim(sound.types)[1]+1] <- "event_id"
  }
  sound.dist <- data.frame(event_id, sound.dist)
  all.dist <- data.frame(all_focal,all_neighbour,all.dist,all.overlap.perc)
  overlap.perc <- data.frame(focal_id,neighbour_id,overlap.perc)
  #return(sound.dist)
  return(list(sound.dist = sound.dist, overlap.perc = overlap.perc, all.dist = all.dist))
}

splitid <- strsplit(df_tot$all_focal, split='_')
lapply(seq(splitid), function(i) splitid[[i]][6])
       
# df1_dist <- data.frame(tdist(file1)$sound.dist)
# df1_overlap <- data.frame(tdist(file1)$overlap.perc)
# df1_all <- data.frame(tdist(file1)$all.dist)
# df2_dist <- tdist(file2)$sound.dist
# df2_overlap <- data.frame(tdist(file2)$overlap.perc)
# library(tidyverse)
# df_dist_tot1 <- bind_rows(df1_dist,df2_dist)
# df_overlap_tot1 <- bind_rows(df1_overlap,df2_overlap)


###-----------------------------------------------------------------------------------------------------
sound.types <- read.table(file1, dec=',')
sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
sound.types[,4] <- formatC(seq(sound.types[,3]), digits=2,flag="0")
names(sound.types) <- c("t1","t2","st","nb")

sound.comb <- expand.grid(sound.types[,4],sound.types[,4])[,c(2,1)]
names(sound.comb) <- c("fnb","nnb")


sound.info= foreach(fnb=sound.comb$fnb, .combine=rbind)%do%{
  fst <- sound.types[sound.types$nb==fnb,]$st
  fnb_t1 <- sound.types[sound.types$nb==fnb,]$t1
  fnb_t2 <- sound.types[sound.types$nb==fnb,]$t2
  
  foreach(nnb=sound.comb$nnb, .combine=rbind)%do%{
    nst <- sound.types[sound.types$nb==nnb,]$st
    nnb_t1 <- sound.types[sound.types$nb==fnb,]$t1
    nnb_t2 <- sound.types[sound.types$nb==fnb,]$t2
    
    if (nnb_t2<fnb_t1){
      sound.dist <- nnb_t2-fnb_t1
      overlap <- 0
      return(c(sound.dist,overlap))
    }
    
    else if (fnb_t2<nnb_t1){
      sound.dist <- nnb_t1-fnb_t2
      overlap <- 0
      return(c(sound.dist,overlap))
    }
    
    else if ((nnb_t1<=fnb_t1) & (fnb_t1<=nnb_t2) & (nnb_t2<=fnb_t2)) {#i begins during j and ends after j
      sound.dist <- 0
      overlap <- 100*(nnb_t2-fnb_t1)/(fnb_t2-fnb_t1)
      return(c(sound.dist,overlap))
    }
    
    else if ((fnb_t1<=nnb_t1) & (nnb_t1<=fnb_t2) & (fnb_t2<=nnb_t2)){#i begins before j and ends during j
      sound.dist <- 0
      overlap <- 100*(fnb_t2-nnb_t1)/(fnb_t2-fnb_t1)
      return(c(sound.dist,overlap))
    }
    
    else if ((fnb_t1<=nnb_t1) & (nnb_t1<=fnb_t2) & (fnb_t1<=nnb_t2) & (nnb_t2<=fnb_t2)) {#j begins and ends during i
      sound.dist <- 0
      overlap <- 100*(nnb_t2-nnb_t1)/(fnb_t2-fnb_t1)
      return(c(sound.dist,overlap))
    }
    
    else if ((nnb_t1<=fnb_t1) & (fnb_t1<=nnb_t2) & (nnb_t1<=fnb_t2) & (fnb_t2<=nnb_t2)) {#i beings and ends during j
      sound.dist <- 0
      overlap <- 100
      return(c(sound.dist,overlap))
    }
  }
}


test_seq <- sapply(seq(sound.types[,3]), function(i) formatC(i, digits=2,flag="0"))
expand.grid(test_seq, test_seq)



tdist2=function(file){## File: file name with time stamps
  library(tuneR)
  sound.types <- read.table(file, dec=',')
  sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
  sound.dist <- data.frame(matrix(ncol = dim(sound.types)[1], nrow = dim(sound.types)[1]))
  all.dist <- c()
  overlap.perc <- c()
  all.overlap.perc <- c()
  event_id <- c()
  focal_id <- c()
  neighbour_id <- c()
  all_focal <- c()
  all_neighbour <- c()
  
  df_comb <- expand
  
  
  for (i in 1:dim(sound.types)[1]){
    Ti_1 <- sound.types[i,1] #start time of referential event 
    Ti_2 <- sound.types[i,2]
    a <- formatC(i, digits=2,flag="0")
    for (j in c(1:dim(sound.types)[1])[-i]){# all events except the referential
      Tj_1 <- sound.types[j,1] #start time of the compared event
      Tj_2 <- sound.types[j,2]
      b <- formatC(j, digits=2,flag="0")
      all_focal <- append(all_focal, paste(sound.types[i,3], file, a, sep='_'))
      all_neighbour <- append(all_neighbour, paste(sound.types[j,3], file, b, sep='_'))
      if (Tj_2<Ti_1){
        sound.dist[i,j] <- Tj_2-Ti_1
        all.dist <- append(all.dist, Tj_2-Ti_1)
        all.overlap.perc <- append(all.overlap.perc, 0)
      }
      else if (Ti_2<Tj_1){
        sound.dist[i,j] <- Tj_1-Ti_2
        all.dist <- append(all.dist, Tj_1-Ti_2)
        all.overlap.perc <- append(all.overlap.perc, 0)
      }
      else if ((Tj_1<=Ti_1) & (Ti_1<=Tj_2) & (Tj_2<=Ti_2)) {#i begins during j and ends after j
        sound.dist[i,j] <- 0
        all.dist <- append(all.dist, 0)
        overlap <- 100*(Tj_2-Ti_1)/(Ti_2-Ti_1)
        overlap.perc <- append(overlap.perc, overlap)
        all.overlap.perc <- append(all.overlap.perc, overlap)
        focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
        neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
      }
      else if ((Ti_1<=Tj_1) & (Tj_1<=Ti_2) & (Ti_2<=Tj_2)){#i begins before j and ends during j
        sound.dist[i,j] <- 0
        all.dist <- append(all.dist, 0)
        overlap <- 100*(Ti_2-Tj_1)/(Ti_2-Ti_1)
        overlap.perc <- append(overlap.perc, overlap)
        all.overlap.perc <- append(all.overlap.perc, overlap)
        focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
        neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
      }
      else if ((Ti_1<=Tj_1) & (Tj_1<=Ti_2) & (Ti_1<=Tj_2) & (Tj_2<=Ti_2)) {#j begins and ends during i
        sound.dist[i,j] <- 0
        all.dist <- append(all.dist, 0)
        overlap <- 100*(Tj_2-Tj_1)/(Ti_2-Ti_1)
        overlap.perc <- append(overlap.perc, overlap)
        all.overlap.perc <- append(all.overlap.perc, overlap)
        focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
        neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
      }
      else if ((Tj_1<=Ti_1) & (Ti_1<=Tj_2) & (Tj_1<=Ti_2) & (Ti_2<=Tj_2)) {#i beings and ends during j
        sound.dist[i,j] <- 0
        all.dist <- append(all.dist, 0)
        overlap <- 100
        overlap.perc <- append(overlap.perc, overlap)
        all.overlap.perc <- append(all.overlap.perc, overlap)
        focal_id <- append(focal_id, paste(sound.types[i,3], file, a, sep='_'))
        neighbour_id <- append(neighbour_id, paste(sound.types[j,3], file, b, sep='_'))
      }
    }
    event_id <- append(event_id, paste(sound.types[i,3], file, a, sep='_'))
    names(sound.dist)[1:dim(sound.types)[1]] <- paste('dist_event_nb', as.character(1:dim(sound.types)[1]), sep='_')
    #names(sound.dist)[dim(sound.types)[1]+1] <- "event_id"
  }
  sound.dist <- data.frame(event_id, sound.dist)
  all.dist <- data.frame(all_focal,all_neighbour,all.dist,all.overlap.perc)
  overlap.perc <- data.frame(focal_id,neighbour_id,overlap.perc)
  #return(sound.dist)
  return(list(sound.dist = sound.dist, overlap.perc = overlap.perc, all.dist = all.dist))
}

