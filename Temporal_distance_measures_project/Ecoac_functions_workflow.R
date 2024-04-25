
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
     RES <- expand.grid(fst= focal.types, nst= neigh.types)
     RES <- cbind(fh=rep(fh, length(RES$fst)),nh=rep(nh, length(RES$fst)), RES, L=rep(L, length(RES$fst)))
     pin.test <- rbind(pin.test,RES)
   }
   pin.neigh <- rbind(pin.neigh, pin.test)
 }
 return(pin.neigh[pin.neigh$L %in% c(no.neigh), ])
 
}

channel = "left"
site = "BEARAV"
date = "20140620"
no.neigh = -4:4

test.Neigh = FUN1_getNeigh(Acous.releves, channel, site, date, no.neigh)

#############################################################################################################
## NO Function to get sub-neighborhoods, null communities and SES calculations for a given focal individual
FUN3_getAcousMetrics = function(df.neigh, traits, list.acousdist, list.acouscomrand, list.acouspool)
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

###Test
traits <- c("duration", "dom_freq")
new.Neigh <- FUN3_getAcousMetrics(test.Neigh, traits, list.acousdist, list.acouscomrand, list.acouspool)
