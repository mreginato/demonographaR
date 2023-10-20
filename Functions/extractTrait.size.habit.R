extractTrait.size.habit <- function(descriptions, final.unit = "m") {
  
  possible.units = c("mm", "cm", "dm", "m")
  paste(" ", possible.units, " ", sep="") -> possible.units
  
  trait <- data.frame(matrix(ncol=5, nrow=length(descriptions)))
  colnames(trait) <- c("species", "extracted_value", "unit", "description", "reference")
  trait$species <- unlist(lapply(descriptions, FUN=attr, "species"))
  trait$reference <- unlist(lapply(descriptions, FUN=attr, "reference"))
  descriptions -> txt
  
  for (i in 1:length(txt)) {
    txt[i][[1]] -> x
    tolower(x) -> x
    strsplit(x[[1]], ". ", fixed = T)[[1]] -> x
    x[1] -> t0
    trimws(t0, which="both") -> t0
    gsub('(?<=\\))(?=[a-z])', " ", t0, perl=T) -> t0 -> t0
    gsub('(?<=[0-9])(?=[a-z])', " ", t0, perl=T) -> t0
    sub("[^0-9]+", "", t0, ignore.case = FALSE, perl = T) -> t1
    sub("[^0-9]*$", "", t1, ignore.case = FALSE, perl = T) -> v0
    gsub('(?<=[0-9]),(?=[0-9])', ".", v0, perl=T) -> v0
    v0 -> trait[i,2]
    gsub(";", " ", gsub(",", " ", t0)) -> t2
    names(unlist(sapply(possible.units, grep, t2))) -> u0
    paste(u0, collapse="|") -> u0
    sub(" ", "", u0) -> trait[i,3]
    t0 -> trait[i,4]
  }
  
  #################################################
  ### Split range
  #################################################
  
  c(1000, 100, 10, 1) -> units.scale
  names(units.scale) <- trimws(possible.units)
  units.scale[final.unit]/units.scale -> units.t
  
  trait$extracted_value -> values
  trait$unit -> units
  ranges <- vector("list", length=length(values))
  
  for (i in 1:length(values)) {
    values[i] -> v0
    trimws(units[i]) -> u0
    strsplit(v0, "x")[[1]] -> v0
    gsub("(", "-", v0, fixed=T) -> v0
    gsub(")", "-", v0, fixed=T) -> v0
    gsub("–", "-", v0, fixed=T) -> v0
    strsplit(v0, "-") -> v0
    
    if (length(v0) > 0) {
      for (k in 1:length(v0)) {
        v0[[k]] -> v1
        trimws(v1) -> v1
        for (j in 1:length(possible.units)) {
          gsub(trimws(possible.units)[j], "", v1, fixed=T) -> v1
        }
        sub("[^0-9.]+", " ", v1, ignore.case = FALSE, perl = T) -> v1
        sub("[^0-9.]*$", " ", v1, ignore.case = FALSE, perl = T) -> v1
        unlist(strsplit(v1, " ")) -> v1
        suppressWarnings(as.numeric(v1)) -> v1
        na.omit(v1) -> v1
        match(u0, names(units.t)) -> m0
        if (is.na(m0)==F) {
          v1*units.t[m0] -> v1
        }
        if (length(v1)==1){
          rep(v1,2) -> v1
        }
        range(v1) -> v1
        v1 -> v0[[k]]
      }
    }
    m0 <- matrix(ncol=2, nrow=1)
    colnames(m0) <- c("height_min", "height_max")
    if (length(v0)==1) {
      v0[[1]][1] -> m0[1,1]
      v0[[1]][2] -> m0[1,2]
    } 
    m0 -> ranges[[i]]
  }
  
  do.call(rbind, ranges) -> ranges
  
  data.frame(species=trait[,1], ranges, trait[,-1]) -> trait
  paste(colnames(trait)[2:3], final.unit, sep="_") -> colnames(trait)[2:3]
  
  return(trait) 
}