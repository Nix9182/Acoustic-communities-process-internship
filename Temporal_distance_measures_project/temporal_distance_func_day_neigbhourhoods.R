
path <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/"
site <- c('BEARAV', 'GRANAM', 'MOIRAM', 'MORTCE', 'ROSSAM', 'VILOAM')
channel <- c('left', 'right')

#Function to get meta info on all sounds
infoday = function(path, site, channel){
  recording_w_types <- c()
  site_list <- c()
  channel_list <- c()
  date_list <- c()
  hour_list <-c()
  sound.info <- data.frame(matrix(ncol = 0, nrow = 0))
  for (i in site){
    for (j in channel){
      setwd(paste(path,"/", i, "/", j, sep=""))
      for (k in list.files(pattern="*[0-9]$")){
        sound.types <- read.table(k, dec=',')
        sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
        date <-strsplit(k, "_")[[1]][3]
        hour <-substr(strsplit(k, "_")[[1]][4], 1, 2)
        for (l in 1:dim(sound.types)[1]){
          a <- formatC(l, digits=2,flag="0")
          type <- sound.types[l,3]
          type_id <- paste(type, k, a, sep='_')
          test_list <- c(type_id, type, i, j, date, hour, a)
          sound.info <- rbind(sound.info, test_list)
          
        }
      }
    }
    names(sound.info) <- c("id", "type", "site", "channel", "date", "hour", "number")
    sound.info$hour <- as.numeric(sound.info$hour)
  }
  return(sound.info)
}

df_info <- data.frame(infoday(path, site, channel))

#Function to get distance between 2 records
tdistday = function(data_info, id_focal, id_neigh){
  index_focal <- which(data_info$id== id_focal)
  index_neigh <- which(data_info$id== id_neigh)
  time_focal <- data_info$hour[index_focal]
  time_neigh <- data_info$hour[index_neigh]
  distance <- time_neigh-time_focal
  
  return(distance)
}
