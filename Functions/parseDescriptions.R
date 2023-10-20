parseDescriptions <- function(files) {
  vector("list", length=length(files)) -> all.descs
  for (k in 1:length(files)) {
    files[k] -> file
    cat("\nReading", file, "...")
    scan(file, what="", sep="\n", quiet=T, encoding = "UTF-8") -> f0
    grep("#", f0) -> seps
    f0[grep("@", f0)[1]:(seps[1]-1)][1] -> ref0
    trimws(sub("@", "", ref0)) -> ref0
    f0[seps] -> labs
    sub("#", "", sub("# ", "", labs)) -> labs
    trimws(labs) -> labs
    paste(labs, " {", sub(".txt$", "", file), " | ",ref0, "}", sep="") -> labs
    c(seps[-1]-1, length(f0)) -> ends
    seps+1 -> starts
    data.frame(starts, ends) -> splits
    descs <- vector("list", length=nrow(splits))
    names(descs) <- labs
    for (i in 1:nrow(splits)) {
      f0[splits[i,1]:splits[i,2]] -> f1
      paste(f1, collapse=" ") -> descs[[i]]
    }
    descs -> all.descs[[k]]
  }
  unlist(all.descs, recursive = F) -> all.descs
  ### check breaks (<U+FEFF>)
  
  names(all.descs) -> x
  write.csv(x, file="zzzzz.csv", row.names = F)
  read.csv(file="zzzzz.csv", stringsAsFactors = F) -> x
  unlink("zzzzz.csv")
  gsub("<U+FEFF> ", "", x$x, fixed=T) -> x
  gsub("<U+653C>", "", x, fixed=T) -> x
  gsub("<U+3E62>", "", x, fixed=T) -> x
  gsub("<U+FB02>", "", x, fixed=T) -> x
  gsub("<U+3E31>", "", x, fixed=T) -> x
  gsub("<U+633C>", "", x, fixed=T) -> x
  gsub("<U+3E39>", "", x, fixed=T) -> x
  gsub("<U+653C>", "", x, fixed=T) -> x
  gsub("<U+643C><U+3E30>", "-", x, fixed=T) -> x
  gsub("<U+383C><U+3E65>", "e", x, fixed=T) -> x
  x -> names(all.descs)
  return(all.descs)
}