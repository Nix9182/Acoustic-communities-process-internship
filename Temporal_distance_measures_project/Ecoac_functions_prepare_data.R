## ----------------------------------------------------------------------------
fun_gaussianizeTraits = function(traitdata, traits, setype)
{
  traitdata_G = foreach(tr = traits) %do%
    {
      tab = traitdata[, tr, drop = FALSE] %>% drop_na()
      res = Gaussianize(tab, return.u = TRUE, type = setype)
      colnames(res) = tr
      res[["sound_type"]] = rownames(res)
      return(res)
    }
  traitdata_G = traitdata_G %>% reduce(full_join, by = "sound_type")
  
  st.names = traitdata[, "sound_type", drop = FALSE]
  RES = merge(st.names, traitdata_G, by = "sound_type", all = TRUE)
  rownames(RES) = RES[, "sound_type"]
  return(RES)
}

#################################################################################################################
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