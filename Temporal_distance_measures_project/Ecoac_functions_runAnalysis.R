## IN PROGRESS Function to create the random neighborhoods for each focal indvidual given the list.comrand.
fun_getRandAcousComm = function(neighdf, traits, list.acouscomrand, no.rand, list.acouspool)
{
  
  ## Get OBSERVED focal and neighbor soundtype in the initial releve
  fst_init = neighdf$fst
  nst_init = neighdf$nst #soundtypes in the neighborhoods
  L_init = neighdf$L
  
  #I want to cbind the random focal and neigh st lists to the original df with the observed st
  #Null_comAll = list()
  Null_comAll <- foreach(tr.dim= traits)%do%
    {
      acouscomrand = list.acouscomrand[[tr.dim]] #list of randomized pool for one trait
      rownames(acouscomrand) <- acouscomrand[, "Obs"]
      tr.df = neighdf[,c("fh","nh","L","fst","nst", paste0("Obs_dist_", tr.dim))]
      list.names <-c()
      ## Get 'random' focal species for each null community associated to OBSERVED focal species
      Null_com = foreach(h= 1:no.rand, .combine=cbind) %do% 
      {
        name = paste0("Null_nst_", h)
        name.dist = paste0("Null_dist_", h)
        
        rand_nst.list = foreach(nst= nst_init, .combine=rbind)  %do% 
        {
          rand_nst = acouscomrand[nst, paste0("Null_", h)] 
        }
        
        rand.dist <- foreach(i=1:length(fst_init)) %do% 
        {
          fst = as.character(fst_init[i])
          nst = as.character(rand_nst.list[i])
          neigh.dist <- list.acousdist[[tr.dim]][fst,nst]
        }
        
        
        list.names <- c(list.names, name, name.dist)
        Null_dfx <- cbind(rand_nst.list, rand.dist)
        
        return(Null_dfx)
        
      }
      # tr.df <- cbind(tr.df, rand_fst = rand_fst.list, rand_nst = rand_nst.list)
      # names(tr.df)[names(tr.df) == "rand_fst"] <- paste0("Null_fst_", h)
      # names(tr.df)[names(tr.df) == "rand_nst"] <- paste0("Null_nst_", h)
      colnames(Null_com) <- list.names
      
      Null_comTrait<- cbind(tr.df,Null_com)
    }
      #return(list(obs = nst_obs, null = nst_null.list))
   #randNeigh <- append(randNeigh,tr.df) 
  names(Null_comAll) <- c("duration", "dom_freq")
  #return(Null_comTrait)
  return(Null_comAll)
  # return(list(fst = focalst, com.rel = com.rel$nbobs, com.init = nst_init, com.null = nst_RAND))
}

traits <- c("duration", "dom_freq")
test.Rand <- fun_getRandAcousComm(new.Neigh, traits, list.acouscomrand, no.rand, list.acouspool)
