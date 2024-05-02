
#############################################################################################################
## YES Function to get the focal individual and the neighborhood for each pinpoint
FUN1_getNeigh = function(releves, channel, site, date, no.neigh)
{
 day.rel = releves[releves$channel == channel 
                     & releves$site== site 
                     & releves$date == date, ][,c("hour","type")]
 
 pin.neigh <- data.frame(matrix(ncol = 0, nrow = 0))
 
 for (fh in unique(day.rel$hour)){
   pin.test <- data.frame(matrix(ncol = 0, nrow = 0))
   for (nh in unique(day.rel$hour)){
     focal.types = day.rel[day.rel$hour == fh,]$type
     neigh.types = day.rel[day.rel$hour == nh,]$type
     L <- nh-fh
     RES <- expand.grid(nst= neigh.types, fst= focal.types)
     RES <- cbind(fh=rep(fh, length(RES$fst)),nh=rep(nh, length(RES$fst)), RES, L=rep(L, length(RES$fst)))
     pin.test <- rbind(pin.test,RES)
   }
   pin.neigh <- rbind(pin.neigh, pin.test)
 }
 return(pin.neigh[pin.neigh$L %in% c(no.neigh), c('fh','nh','fst','nst','L') ]) #to keep levels wanted and intervert place of fst and nst for more readability
 
}

channel = "left"
site = "BEARAV"
date = "20140620"
no.neigh = -4:4

test.Neigh = FUN1_getNeigh(Acous.releves, channel, site, date, no.neigh)

#############################################################################################################
## NO Function to get sub-neighborhoods, null communities and SES calculations for a given focal individual
FUN2_getAcousMetrics = function(df.neigh, traits, list.acousdist, list.acouscomrand, list.acouspool)
{
  ## Get all null communities 
  # randNull = fun_getRandComm(abundx = abundf, focalsp = focalsp, list.comrand = list.comrand, no.rand = no.rand, list.pool = list.pool)
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

#############################################################################################################
## GLOBAL function 
#############################################################################################################

#takes ~3:40 for one site all days


FUNCTION_Comm = function(channel, site, releves
                         , list.comrand, no.neigh
                         , traits, list.dist, list.pool, focus)
{
  if (focus=="day"){
    dates <- unique(releves[releves$site==site,]$date)
    
    df_SES_dates = foreach(d=dates)%do%{
      ## Get grid numbers for day window
      df_winNum = FUN1_getNeigh(releves, channel, site, date=d, no.neigh)
      
      ## Get observed distance for each trait 
      df_obs_dist = FUN2_getAcousMetrics(df_winNum, traits, list.acousdist, list.acouscomrand, list.acouspool)
      
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
    }
  }
  
  else if (focus=="recording"){
    
  }
  
  names(df_organized) <- traits
  
  return(df_organized)
}