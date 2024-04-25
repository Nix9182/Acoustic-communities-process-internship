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