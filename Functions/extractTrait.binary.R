extractTrait.binary <- function(descriptions, target=NULL, non.target=NULL) {
  
  trait <- data.frame(matrix(ncol=4, nrow=length(descriptions)))
  colnames(trait) <- c("species", "extracted_value", "description", "reference")
  trait$species <- unlist(lapply(descriptions, FUN=attr, "species"))
  trait$reference <- unlist(lapply(descriptions, FUN=attr, "reference"))
  descriptions -> txt
  
  
  for (i in 1:length(txt)) {
    txt[i][[1]] -> x
    tolower(x) -> x
    strsplit(x[[1]], ". ", fixed = T)[[1]][-1] -> x
    
    ### target
    unlist(sapply(targets, grep, x)) -> g0
    x[g0] -> x
    paste(x, collapse=" | ") -> x
    
    if (x == "") {x <- NA}
    
    if (is.na(x) == F) {
      trimws(x) -> x
      x -> trait[i,3]
      ### non-target (absent)
      unlist(sapply(non.target, grep, x, simplify=F)) -> nt
      if (length(nt) > 0) {
        trait[i,2] <- "absent"
      } else {
        trait[i,2] <- "present"
      }
    }
  }
  
  trait$extracted_value[which(is.na(trait$extracted_value))] <- "absent"
  return(trait)
  
}