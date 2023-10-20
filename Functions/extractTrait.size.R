extractTrait.size <- function(descriptions, target=NULL, non.target=NULL, final.unit = "cm", length.only=FALSE) {
  
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
    strsplit(x[[1]], ". ", fixed = T)[[1]][-1] -> x
    ### non-target
    if (is.null(non.target)==F) {
      unlist(sapply(non.target, grep, x)) -> nt
      if (length(nt) > 0) {
        x[-nt] -> x
      }
    }
    ### target
    unlist(sapply(target, grep, x)) -> g0
    if (length(g0) > 1) {
      g0[1] -> g0
      sub("1", "", names(g0)) -> names(g0)
    }
    x[g0] -> x
    names(g0) -> target.f
    if (length(x) == 0) {x <- NA}
    if (is.na(x) == F) {
      strsplit(x[[1]], target.f)[[1]][2] -> x
      if (is.na(x)) {x <- ""}
      paste(target.f,x, sep="") -> x
      strsplit(x[[1]], "; ", fixed = T)[[1]] -> x
      x[grep(target.f, x)[1]] -> x
      trimws(x, which="both") -> t0
      t0 -> d0
      gsub('(?<=\\))(?=[a-z])', " ", t0, perl=T) -> t0 
      gsub('(?<=[0-9])(?=[a-z])', " ", t0, perl=T) -> t0
      gsub('(?<=[0-9]),(?=[0-9])', ".", t0, perl=T) -> t1
      gsub("[0-9],", "", t1, perl = T) -> t1
      sub("[^0-9]+", "", t1, ignore.case = FALSE, perl = T) -> t1
      ### check long/wide
      grep("long", t1) -> w0
      c(w0,grep("wide", t1)) -> w0
      if (sum(w0) == 2) {
        paste(strsplit(t1, ",")[[1]][1:2], collapse=" x ") -> t1
        sub("long", "", t1) -> t1
        sub("wide", "", t1) -> t1
      } else {
        strsplit(t1, ",")[[1]][1] -> t1
      }
      ### check other structures ","
      strsplit(t1, ", ", fixed = T)[[1]] -> t1
      gsub("[^0-9]*$", "", t1, ignore.case = FALSE, perl = T) -> v0
      trimws(v0) -> v0
      gsub("  ", " ", v0) -> v0
      ### clean v0
      gsub(" mm", "", v0) -> v0
      gsub("ca", "", v0) -> v0
      gsub("about", "", v0) -> v0
      gsub("mostly", "", v0) -> v0
      gsub("on lateral view ", "", v0) -> v0
      gsub("and", "", v0) -> v0
      gsub("long", "x", v0) -> v0
      gsub("de largo y de", "x", v0) -> v0
      gsub("by", "x", v0) -> v0
      gsub("×", "x", v0) -> v0
      gsub("de largo con un ala", "x", v0) -> v0
      gsub("rnm de largo n un ala", "x", v0) -> v0
      gsub(" mm wide", "", v0) -> v0
      gsub("–", "-", v0) -> v0
      gsub(" - ", "-", v0) -> v0
      gsub("- ", "-", v0) -> v0
      gsub(" -", "-", v0) -> v0
      gsub("—", "-", v0) -> v0
      for (j in 1:length(possible.units)) {
        gsub(possible.units[j], "", v0) -> v0
      }
      
      gsub("[", "(", v0, fixed = T) -> v0
      gsub("]", ")", v0, fixed = T) -> v0
      
      ###
      v0 -> trait[i,2]
      gsub(";", " ", gsub(",", " ", t0)) -> t2
      paste(t2, " ", sep="") -> t2
      names(unlist(sapply(possible.units, grep, t2))) -> u0
      paste(u0, collapse="|") -> u0
      sub(" ", "", u0) -> trait[i,3]
      d0 -> trait[i,4]
    }
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
  
  if (length.only == T) {
    
    for (i in 1:length(values)) {
      values[i] -> v0
      trimws(units[i]) -> u0
      strsplit(v0, "x")[[1]][1] -> v0
      trimws(v0)
      gsub("(", "", v0, fixed=T) -> v0
      gsub(")", "", v0, fixed=T) -> v0
      strsplit(v0, "-") -> v0
      
      for (k in 1:length(v0)) {
        v0[[k]] -> v1
        trimws(v1) -> v1
        sub("[^0-9.]+", "", v1, ignore.case = FALSE, perl = T) -> v1
        sub("[^0-9.]*$", "", v1, ignore.case = FALSE, perl = T) -> v1
        suppressWarnings(as.numeric(v1)) -> v1
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
      m0 <- matrix(ncol=2, nrow=1)
      colnames(m0) <- c("length_min", "length_max")
      if (length(v0)==1) {
        v0[[1]][1] -> m0[1,1]
        v0[[1]][2] -> m0[1,2]
      } 
      m0 -> ranges[[i]]
    }
    
    do.call(rbind, ranges) -> ranges
    
    #rowMeans(ranges[,1:2]) -> length.m
    #data.frame(species=trait[,1], length_mean=length.m, ranges, trait[,-1]) -> trait
    #paste(colnames(trait)[2:5], final.unit, sep="_") -> colnames(trait)[2:5]
    
    data.frame(species=trait[,1], ranges, trait[,-1]) -> trait
    paste(colnames(trait)[2:3], final.unit, sep="_") -> colnames(trait)[2:3]
    
  } else {
    
    for (i in 1:length(values)) {
      values[i] -> v0
      trimws(units[i]) -> u0
      strsplit(v0, "x")[[1]] -> v0
      gsub("(", "", v0, fixed=T) -> v0
      gsub(")", "", v0, fixed=T) -> v0
      gsub("–", "-", v0, fixed=T) -> v0
      strsplit(v0, "-") -> v0
      
      for (k in 1:length(v0)) {
        v0[[k]] -> v1
        trimws(v1) -> v1
        sub("[^0-9.]+", "", v1, ignore.case = FALSE, perl = T) -> v1
        sub("[^0-9.]*$", "", v1, ignore.case = FALSE, perl = T) -> v1
        suppressWarnings(as.numeric(v1)) -> v1
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
      m0 <- matrix(ncol=4, nrow=1)
      colnames(m0) <- c("length_min", "length_max", "width_min", "width_max")
      if (length(v0)==1) {
        v0[[1]][1] -> m0[1,1]
        v0[[1]][2] -> m0[1,2]
      } else {
        v0[[1]][1] -> m0[1,1]
        v0[[1]][2] -> m0[1,2]
        v0[[2]][1] -> m0[1,3]
        v0[[2]][2] -> m0[1,4]
      }
      m0 -> ranges[[i]]
    }
    
    do.call(rbind, ranges) -> ranges
    
    #rowMeans(ranges[,1:2]) -> length.m
    #rowMeans(ranges[,3:4]) -> width.m
    #data.frame(species=trait[,1], length_mean=length.m, width_mean=width.m, ranges, trait[,-1]) -> trait
    #paste(colnames(trait)[2:7], final.unit, sep="_") -> colnames(trait)[2:7]
    
    data.frame(species=trait[,1], ranges, trait[,-1]) -> trait
    paste(colnames(trait)[2:5], final.unit, sep="_") -> colnames(trait)[2:5]
    
  }
  
  
  return(trait) 
}