
#############################################################################################################
## YES Function to get the focal individual and the neighborhood for each pinpoint
FUN1_getNeigh = function(releves, site, date, no.neigh)
{
 day.rel = releves[releves$site== site 
                    & releves$date == date, ][,c("channel","hour","type")]
 
 pin.neigh = foreach (chan=unique(day.rel$channel), .combine=rbind)%do%{
   df_chan = day.rel[day.rel$channel==chan,]
   foreach (fh=unique(df_chan$hour), .combine=rbind)%do%{
     focal.types = df_chan[df_chan$hour == fh,]$type
     
     foreach(nh=unique(df_chan$hour),.combine=rbind)%do%{
       neigh.types = df_chan[df_chan$hour == nh,]$type
       L <- nh-fh
       RES <- expand.grid(nst= neigh.types, fst= focal.types)
       RES <- cbind(channel=rep(chan, length(RES$fst)), fh=rep(fh, length(RES$fst)),nh=rep(nh, length(RES$fst)), RES, L=rep(L, length(RES$fst)))
    }
  }
 }
return(pin.neigh[pin.neigh$L %in% c(no.neigh), c("channel",'fh','nh','fst','nst','L') ]) #to keep levels wanted and intervert place of fst and nst for more readability
 
}

#############################################################################################################
## YES Function to get sub-neighborhoods, null communities and SES calculations for a given focal individual
FUN2_getAcousMetrics = function(df.neigh, traits, list.acousdist, list.acouspool)
{
  ## Get the observed acoustic distances between focal and neighbors
  
  df.neigh.dist <- df.neigh
  for(tr.dim in traits){
    list.neigh.dist <- c()
    for (i in 1:length(rownames(df.neigh))){
      fst = as.character(df.neigh$fst[i])
      nst = as.character(df.neigh$nst[i])
      list.neigh.dist <- c(list.neigh.dist,list.acousdist[[tr.dim]][fst,nst]) 
    }
    df.neigh.dist <- cbind(df.neigh.dist,list.neigh.dist)
    colnames(df.neigh.dist)[ncol(df.neigh.dist)] <- paste0("Obs_dist_", tr.dim)
  }
  return(df.neigh.dist)
}


##########################################################################################################
## YES Function to create the random neighborhoods for each focal indvidual given the list.comrand and get differences and means
fun_getRandAcousComm = function(neighdf, traits, list.acouscomrand, no.rand, list.acouspool)
{
  acouscomrand = list.acouscomrand #randomized pool 
  rownames(acouscomrand) <- acouscomrand[, "Obs"]
  ## Get OBSERVED focal and neighbor soundtype in the initial releve
  Null_comMeans <- foreach(tr.dim= traits)%do%
    {
      Null_comMetrics = NULL
      tr.df = neighdf[neighdf$fst!=neighdf$nst ,c("channel","fh","nh","L","fst","nst", paste0("Obs_dist_", tr.dim))]
      colnames(tr.df)[colnames(tr.df)==paste0("Obs_dist_", tr.dim)] <- "Obs_dist"
      list.names <-c(colnames(tr.df), 'Null_mean_all', 'Null_sd_all')
      
      ## Get 'random' focal species for each null community associated to OBSERVED focal species
      Null_means = foreach(h= 1:no.rand, .combine=cbind) %do% 
        {
          name = paste0("Null_nst_", h)
          name.dist = paste0("Null_dist_", h)
          name.mean = paste0("Null_mean_", h)
          list.names <- c(list.names, name, name.dist, name.mean)
          
          
          #--------------------------------------------------------------------------------- 
          rand_nst.list = foreach(nst= tr.df$nst, .combine=rbind)  %do% 
            {
              rand_nst = acouscomrand[nst, paste0("Null_", h)] 
            }
          #---------------------------------------------------------------------------------
          rand.dist = foreach(i=1:length(tr.df$fst), .combine=rbind) %do% 
            {
              fst = as.character(tr.df$fst[i])
              nst = as.character(rand_nst.list[i])
              neigh.dist <- list.acousdist[[tr.dim]][fst,nst]
            }
          
          #---------------------------------------------------------------------------------
          transit.dfx = cbind(tr.df[,c("fh","L","fst")], rand_nst.list, rand.dist)
          
          all.means = foreach(fh=unique(transit.dfx$fh), .combine=rbind) %do%
            {
              sub.df.fh = transit.dfx[transit.dfx$fh == fh,]
              
              L.metrics = foreach(L=unique(sub.df.fh$L), .combine=rbind) %do%
                {
                  sub.df.L = sub.df.fh[sub.df.fh$L==L,]
                  
                  fst.metrics = foreach(fst=sub.df.L$fst, .combine=rbind) %do% 
                    {
                      sub.df.fst = sub.df.L[sub.df.L$fst==fst,]
                      mean.dist <- mean(abs(sub.df.fst[,"rand.dist"]))
                    }
                }
            }
          #----------------------------------------------------------------------------------
          Null_comMetrics <- cbind(Null_comMetrics, rand_nst.list, rand.dist, all.means)
          return(all.means)
          
        }
      
      Null_comTrait<- as.data.frame(cbind(tr.df,rowMeans(Null_means), apply(Null_means, 1, sd), Null_comMetrics))
      names(Null_comTrait) <- list.names
      return(Null_comTrait)
    }
  
  names(Null_comMeans) <- traits
  return(Null_comMeans)
}

#############################################################################################################
## YES Function to calculate the acoustic distances between the focal individual and neighborhoods.
FUN_calcSes = function(rand.Metrics)
{
  res= foreach(tr.dim=names(rand.Metrics))%do%{
    tr.df = rand.Metrics[[tr.dim]][,c("channel","fh","nh","L","fst","nst", "Obs_dist", 'Null_mean_all', 'Null_sd_all')]
    ## Get distances between neighbor species and the focal species for each trait required
    tr.df$SES = (tr.df$Obs_dist-tr.df$Null_mean_all)/tr.df$Null_sd_all
    return(tr.df)
  }
  names(res)<-names(rand.Metrics)
  
  return(res)
}

#############################################################################################################
## GLOBAL function 
#############################################################################################################

#takes ~3:40 for one site one channel all days


FUNCTION_Comm = function(site, releves
                         , list.comrand, no.neigh
                         , traits, list.dist, list.pool)
{
  dates <- unique(releves[releves$site==site,]$date)
  
  df_SES_dates = foreach(d=dates)%do%{
    ## Get grid numbers for day window
    df_winNum = FUN1_getNeigh(releves, site, date=d, no.neigh)
    
    ## Get observed distance for each trait 
    df_obs_dist = FUN2_getAcousMetrics(df_winNum, traits, list.acousdist, list.acouspool)
    
    ## Get null communities, mean distances and sd
    df_rand_comm = fun_getRandAcousComm(df_obs_dist, traits, list.acouscomrand, no.rand, list.acouspool)
    
    ## Get SES calculations
    df_SES = FUN_calcSes(df_rand_comm) 
    
  }
  names(df_SES_dates) <- dates
  ## --------------------------------------------------------------------------------------
  
  df_organized = foreach(tr.dim=traits)%do%{
    df_trait = foreach(d=dates, .combine=rbind)%do%{
      df_SES_dates[[d]][[tr.dim]]
    
    }
    df_trait$site = rep(site,length(df_trait[,1]))
    return(df_trait)
  }
  
  names(df_organized) <- traits
  
  return(df_organized)
}