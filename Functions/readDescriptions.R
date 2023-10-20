readDescriptions <- function(file) {
  cat("\nReading", file, "...")
  scan(file, what="", sep="\n", quiet = T, encoding = "UTF-8") -> all.lines
  
  ### split descriptions
  
  all.lines[grep("#", all.lines)] -> titles
  all.lines[grep("#", all.lines)+1] -> descs
  vector("list", length=length(titles)) -> all.descs
  
  for (i in 1:length(titles)) {
    titles[i] -> title
    strsplit(title, " {", fixed=T)[[1]] -> title
    sub("# ", "", title[1]) -> sp0
    trimws(sp0, which="both") -> sp0
    sub("}", "", title[2], fixed=T) -> ref0
    descs[i] -> desc
    names(desc) <- sp0
    attr(desc, "reference") <- ref0
    attr(desc, "species") <- sp0
    desc -> all.descs[[i]]
  }
  return(all.descs)
}