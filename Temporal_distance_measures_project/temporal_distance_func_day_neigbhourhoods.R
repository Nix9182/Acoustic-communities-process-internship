
path <- "C:/Users/cobod/OneDrive/Bureau/Master BEE MNHN/Stage M1 Ecoacoustique/Données/files/"
site <- c('BEARAV', 'GRANAM', 'MOIRAM', 'MORTCE', 'ROSSAM', 'VILOAM')
channel <- c('left', 'right')

#Function to get meta info on all sounds

infoday = function(path, site, channel){
  sound.info <- data.frame(matrix(ncol = 0, nrow = 0))
  
  for (i in site){
    for (j in channel){
      setwd(paste(path,"/", i, "/", j, sep=""))
      
      for (k in list.files(pattern="*[0-9]$")){
        sound.types <- read.table(k, dec=',')
        sound.types[,3] <- formatC(sound.types[,3], digits=2,flag="0")
        info_split <- strsplit(k, "_")[[1]]
        date <- info_split[3]
        hour <-substr(info_split[4], 1, 2)
        
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


Acous.info <- data.frame(infoday(path, site, channel))


#############################################################################################################
#Function to get list of unique soundtypes in each recording (pinpoint observation) 
fun_getUniqueSt = function(data_info, channel, site, date, hour){
  df_set <- data_info[data_info$channel == channel 
                      & data_info$site == site 
                      & data_info$date == date,]
  
  unique_st = foreach(h= 0:(length(unique(df_set$hour))-1), .combine = rbind)%do%{
    {
      table_set <-table(df_set[df_set$hour == h,]$type)
      df_table <- as.data.frame(table_set)
      df_table <- cbind(channel = rep(channel,length(df_table[,1])),
                        site = rep(site,length(df_table[,1])),
                        date = rep(date,length(df_table[,1])),
                        hour=rep(h,length(df_table[,1])), 
                        df_table)
      return(df_table)
    }
  
  }
  colnames(unique_st)[c(5,6)] <- c("type", "nb_of_obs")
    
  if(missing(hour)){
    return(unique_st)
  }
  
  else {
    return(unique_st[unique_st$hour == hour, ])
  }
}

