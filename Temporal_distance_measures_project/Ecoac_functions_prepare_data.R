## ----------------------------------------------------------------------------
fun_gaussianizeTraits = function(traitdata, traits, setype)
{
  traitdata_G = foreach(tr = traits) %do%
    {
      tab = traitdata[, tr, drop = FALSE] %>% drop_na()
      res = Gaussianize(tab, return.u = TRUE, type = setype)
      colnames(res) = tr
      res[["refNamePin"]] = rownames(res)
      return(res)
    }
  traitdata_G = traitdata_G %>% reduce(full_join, by = "refNamePin")
  
  sp.names = traitdata[, "refNamePin", drop = FALSE]
  RES = merge(sp.names, traitdata_G, by = "refNamePin", all = TRUE)
  rownames(RES) = RES[, "refNamePin"]
  return(RES)
}