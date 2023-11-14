cleanDescriptions <- function(descriptions) {
  
  for (i in 1:length(descriptions)) {
    descriptions[[i]] -> t0
    write.table(t0, file = "zzzzz.txt", quote=F, col.names=F, row.names=F, fileEncoding = "UTF-8", append=F)
    #suppressWarnings(as.character(read.table("zzzzz.txt", fileEncoding = "UTF-8", sep="#"))) -> t0
    read.table("zzzzz.txt", fileEncoding = "UTF-8") -> t0
    unlink("zzzzz.txt")
    paste(t0[1,], collapse=" ") -> t0
    gsub("ca.", "ca", t0, fixed=T) -> t0
    gsub("alt.", "alt", t0, fixed=T) -> t0
    gsub("<u+00 ac>", "", t0, fixed=T) -> t0
    gsub("<U+00 ac>", "", t0, fixed=T) -> t0
    gsub("<U+00 AC>", "", t0, fixed=T) -> t0
    gsub("<U+00AC>", "", t0, fixed=T) -> t0
    gsub("<u+00 b1>", "", t0, fixed=T) -> t0
    gsub("<U+00 b1>", "", t0, fixed=T) -> t0
    gsub("<U+00 B1>", "", t0, fixed=T) -> t0
    gsub("<U+00B1>", "", t0, fixed=T) -> t0
    gsub("<u+00 a5>", "", t0, fixed=T) -> t0
    gsub("<u+00a5>", "", t0, fixed=T) -> t0
    gsub("<u+0096>", "-", t0, fixed=T) -> t0
    gsub("<U+0096>", "-", t0, fixed=T) -> t0
    gsub("<u+03 c7>", "x", t0, fixed=T) -> t0
    gsub("<U+03 c7>", "x", t0, fixed=T) -> t0
    gsub("<U+03 C7>", "x", t0, fixed=T) -> t0
    gsub("<U+03C7>", "x", t0, fixed=T) -> t0
    gsub("<d7>", "x", t0, fixed=T) -> t0
    gsub("<D7>", "x", t0, fixed=T) -> t0
    gsub("|", "x", t0, fixed=T) -> t0
    gsub("<U+FB02>", "fl", t0, fixed = T) -> t0
    gsub("<U+FB01>", "fi", t0, fixed = T) -> t0
    gsub("; ", ". ", t0, fixed = T) -> t0 
    gsub("<U+F0B4>", "?", t0, fixed = T) -> t0
    gsub("<U+2153>", "1/3", t0, fixed = T) -> t0
    gsub("<U+2154>", "2/3", t0, fixed = T) -> t0
    gsub("<U+25CF>", "", t0, fixed = T) -> t0 
    gsub("<U+25B2>", "", t0, fixed = T) -> t0 
    gsub("<U+2126>", "omega-shape", t0, fixed = T) -> t0 
    gsub("<U+E033>", "?", t0, fixed = T) -> t0
    gsub("<U+0097>", " ", t0, fixed = T) -> t0
    gsub(" c.", " ca", t0, fixed = T) -> t0
    gsub(" .cm ", " cm ", t0, fixed=T) -> t0	
    gsub("-", "-", t0, fixed=T) -> t0
    gsub("-", "-", t0, fixed=T) -> t0
    gsub("?", "-", t0, fixed=T) -> t0
    gsub("?", " ", t0, fixed=T) -> t0
    t0 -> descriptions[[i]]
    cat("\rCleaning description",i,"...")
  }
  
  return(descriptions)
}
