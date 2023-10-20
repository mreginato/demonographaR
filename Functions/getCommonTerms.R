getCommonTerms <- function(description, exclude=NULL) {
  
  description -> descs
  
  str_extract(descs, "[^0-9]+") -> descs
  trimws(descs) -> descs
  
  if (is.null(exclude) == F) {
    for (i in 1:length(exclude)) {
      gsub(exclude[i], "", descs) -> descs
    }
  }
  
  corp <- VCorpus(VectorSource(descs))
  pdfs.tdm <- TermDocumentMatrix(corp, control = list(
    removePunctuation = TRUE,
    stopwords = TRUE,
    tolower = TRUE,
    stemming = TRUE,
    removeNumbers = TRUE,
    bounds = list(global = c(3, Inf)))) 
  ft <- findFreqTerms(pdfs.tdm, 
                      lowfreq = 2, 
                      highfreq = Inf)
  ft.tdm <- as.matrix(pdfs.tdm[ft,])
  ft.count <- sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
  d <- data.frame(word = names(ft.count),freq=ft.count)
  
  ### wordcloud
  
  par(mar=c(2,2,2,2))
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=nrow(d), random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  return(d)
}