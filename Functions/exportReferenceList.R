exportReferenceList <- function(descriptions, file=NULL) {
  names(descriptions) -> refs
  unique(unlist(lapply(strsplit(refs, " {", fixed=T), "[", 2))) -> refs
  unique(unlist(lapply(strsplit(refs, " [", fixed=T), "[", 1))) -> refs
  do.call(rbind, strsplit(refs, " | ", fixed=T)) -> refs
  colnames(refs) <- c("file", "reference")
  paste(refs[,1], ".txt", sep="") -> refs[,1]
  sub("}", "", refs[,2], fixed=T) -> refs[,2]
  if (is.null(file) == F) {
    write.xlsx2(refs, file=file, row.names = F)
    cat("\n", "Reference list saved in:", paste(getwd(), "/", file, sep=""))
  }
  return(refs)
}