exportDescriptions <- function(descriptions, file="Descriptions.txt") {
  
  cat("Exporting file to: ", getwd(), "/", file, sep="", fill=T)
  
  ### Check duplicates
  
  names(descriptions) -> x
  lab1 <- unlist(lapply(strsplit(x, " "), FUN=function(x)(paste(x[1],x[2], sep="_"))))
  lab2 <- unlist(lapply(strsplit(x, " {", fixed=T), "[", 2))
  lab2 <- unlist(lapply(strsplit(lab2, " |", fixed=T), "[", 1))
  paste(lab1, "_", lab2, ".txt", sep="") -> labs
  length(labs) -> l0
  length(unique(labs)) -> l1
  
  if (l1 < l0) {
    
    table(labs) -> labs.t
    which(labs.t > 1) -> labs.to.fix
    
    for (i in 1:length(labs.to.fix)) {
      labs.to.fix[i] -> l0
      labs.t[l0] -> n
      which(labs == names(l0)) -> x
      labs[x] -> l1
      sub(".txt$", "", l1) -> l1
      paste(paste(l1, 1:n, sep="_"), ".txt", sep="") -> l1
      labs[x] <- l1
    }
  }
  
  ### Export 
  
  cat("", file=file)
  
  for (i in 1:length(descriptions)) {
    descriptions[[i]] -> d0
    names(descriptions)[i] -> lab0
    f0 = labs[i]
    write.table(rbind(paste("#", lab0), d0), file = file, quote=F, col.names=F, row.names=F, fileEncoding = "UTF-8", append=T)
    cat("\r","Saving description:", i, "     ", sep="")
  }
  
}