
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

