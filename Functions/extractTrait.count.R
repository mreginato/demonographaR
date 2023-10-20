extractTrait.count <- function(descriptions, target=NULL, non.target=NULL) {
  
  possible.units = c("mm", "cm", "dm", "m", "mm)", "cm)", "cm,", "mm,")
  paste(" ", possible.units, " ", sep="") -> possible.units
  
  trait <- data.frame(matrix(ncol=6, nrow=length(descriptions)))
  colnames(trait) <- c("species", "count_min", "count_max", "extracted_value", "description", "reference")
  trait$species <- unlist(lapply(descriptions, FUN=attr, "species"))
  trait$reference <- unlist(lapply(descriptions, FUN=attr, "reference"))
  descriptions -> txt
  
  for (i in 1:length(txt)) {
    txt[i][[1]] -> x
    tolower(x) -> x
    gsub("-", "-", x, fixed=T) -> x
    gsub("-", "-", x, fixed=T) -> x
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
      #strsplit(x[[1]], target.f)[[1]][1] -> x
      strsplit(x[[1]], "; ", fixed = T)[[1]] -> x
      unlist(strsplit(x[[1]], ", ", fixed = T)) -> x
      
      unique(unlist(sapply(target, grep, x))) -> g0
      x[g0] -> x
      
      ### non-target (units)
      unique(unlist(sapply(possible.units, grep, x))) -> g0
      if (length(g0) > 0) {
        x[c(1:length(x))[-g0]] -> x
      }
      paste(x, collapse=", ") -> x
      trimws(x, which="both") -> t0
      gsub('(?<=\\))(?=[a-z])', " ", t0, perl=T) -> t0 
      gsub('(?<=[0-9])(?=[a-z])', " ", t0, perl=T) -> t0
      gsub('(?<=[0-9]),(?=[0-9])', ".", t0, perl=T) -> t1
      t0 -> trait[i,5]
      ### isolate number
      
      grep('[0-9]', x) -> n0
      if (length(n0) > 0) {
        gsub("[a-z]", "", x) -> x
        gsub('[^a-zA-Z0-9 -]', "", x) -> x
        gsub(" ", "", x) -> x
        strsplit(x, "-")[[1]] -> x
        x[x!=""] -> x
        paste(x, collapse = "-") -> trait[i,4]
        range(as.numeric(x)) -> v0
        v0[1] -> trait[i,2]
        v0[2] -> trait[i,3]
      }
    }
  }
  
  return(trait) 
}