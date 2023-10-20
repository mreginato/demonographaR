flagSuspicious <- function(trait) {
  
  get.out <- function(x) {
    quantile(x,na.rm=T)[2]-(1.5*IQR(x,na.rm = T)) -> min.out
    quantile(x,na.rm=T)[4]+(1.5*IQR(x,na.rm=T)) -> max.out
    which(x < min.out) -> out.1
    which(x > max.out) -> out.2
    c(out.1,out.2) -> out
    return(out)
  }
  
  target = c("_min_", "_max_", "count_min", "count_max")
  colnames(trait) -> cols
  unlist(sapply(target, grep, cols)) -> cols.n
  if (length(cols.n) == 0) {
    message("Nothing to flag, no minimum or maximum values")
    outliers <- NULL
  } else {
    vector("list", length=nrow(trait)) -> outliers
    outliers[] <- ""
    ### outliers - all
    trait[,cols.n] -> t0
    trait$species -> spp
    unlist(lapply(strsplit(spp, " "), "[", 1)) -> genera
    for (i in 1:ncol(t0)) {
      as.numeric(t0[,i]) -> t0[,i]
      ### All
      get.out(t0[,i]) -> out0
      outliers[out0] <- paste(outliers[out0], "Outlier_all")
      ### Genera
      aggregate(t0[,i], by=list(genera), FUN=get.out) -> out.g
      for (k in 1:nrow(out.g)) {
        out.g[k,] -> o0
        o0[,2][[1]] -> o1
        if (length(o1) > 0) {
          which(genera == o0[,1])[o1] -> out0
          outliers[out0] <- paste(outliers[out0], "Outlier_genus")
        }
      }
      ### Species
      aggregate(t0[,i], by=list(spp), FUN=get.out) -> out.g
      for (k in 1:nrow(out.g)) {
        out.g[k,] -> o0
        o0[,2][[1]] -> o1
        if (length(o1) > 0) {
          which(spp == o0[,1])[o1] -> out0
          outliers[out0] <- paste(outliers[out0], "Outlier_species")
        }
      }
    }
    
    ### Text in extract value
    trait$extracted_value -> v0
    gsub(" x ", " ", v0) -> v0
    grep("[a-z]", v0, ignore.case=T) -> out0
    outliers[out0] <- paste(outliers[out0], "Text_in_extval")
    
    ### Min > Max
    
    grep("_min", colnames(trait)) -> min.g
    
    for (i in min.g) {
      as.numeric(trait[,i]) -> t.min
      as.numeric(trait[,i+1]) -> t.max
      which(t.min > t.max) -> out0
      outliers[out0] <- paste(outliers[out0], "Min_greater_Max")
    }
    
    ### clean up
    for (k in 1:length(outliers)) {
      outliers[[k]] -> out0
      if (out0 != "") {
        sort(unique(strsplit(out0, " ")[[1]])) -> out0
        out0[which(out0 != "")] -> out0
        paste(out0, collapse="|") -> outliers[[k]]
      }
      
    }
    unlist(outliers) -> outliers
  }
  return(outliers)
}