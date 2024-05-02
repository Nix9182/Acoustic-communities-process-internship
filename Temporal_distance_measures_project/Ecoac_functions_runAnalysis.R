## YES Function to create the random neighborhoods for each focal indvidual given the list.comrand and get differences and means
fun_getRandAcousComm = function(neighdf, traits, list.acouscomrand, no.rand, list.acouspool)
{
  
  ## Get OBSERVED focal and neighbor soundtype in the initial releve
  Null_comMeans <- foreach(tr.dim= traits)%do%
    {
      Null_comMetrics = NULL
      acouscomrand = list.acouscomrand[[tr.dim]] #list of randomized pool for one trait
      rownames(acouscomrand) <- acouscomrand[, "Obs"]
      tr.df = neighdf[neighdf$fst!=neighdf$nst ,c("fh","nh","L","fst","nst", paste0("Obs_dist_", tr.dim))]
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
              fst = as.character(fst_init[i])
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

}

traits <- c("duration","dom_freq")
test.Rand <- fun_getRandAcousComm(new.Neigh, traits, list.acouscomrand, no.rand, list.acouspool)
