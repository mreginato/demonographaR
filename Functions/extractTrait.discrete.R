extractTrait.discrete <- function(trait, classification=NULL) {
  if (is.null(classification)) {
    stop("A classification (data.frame) should be provided...")
  }
  classification -> class.t
  colnames(class.t) <- c("word", "category")
  unique(class.t$category) -> types
  
  trait.d <- matrix(ncol=length(types), nrow=nrow(trait))
  rownames(trait.d) <- trait$species
  colnames(trait.d) <- types
  trait.d[] <- 0
  
  for (i in 1:nrow(class.t)) {
    class.t[i,] -> h0
    paste(" ", h0$word, sep="") -> h1
    paste(" ", trait$description) -> d0
    grep(h1, d0) -> x
    trait.d[x,h0$category] <- 1
  }
  
  vector(length=nrow(trait.d)) -> d.out
  d.out[] <- NA
  for (i in 1:nrow(trait.d)) {
    trait.d[i,] -> h0
    if (sum(h0) > 0) {
      names(which(h0 == 1)) -> n0
      paste(sort(n0), collapse = "|") -> d.out[i]
    }
  }
  return(d.out)
}