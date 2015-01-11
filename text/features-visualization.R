
library(wordcloud)

showWordCloud  <- function(matrix, min.freq = 3) {
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)
  grayLevels <- gray((wordFreq + 10)  / (max(wordFreq) + 10))
  wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=min.freq, random.order=F, colors=grayLevels)
}

showClusters <- function(tdm, sparseThreshhold = 0.7) {
  # Remove sparse terms that have % of empty elements over the threshold
  tdm2 <- removeSparseTerms(tdm, sparse=sparseThreshhold)
  m2 <- as.matrix(tdm2)
  
  # The distances between terms can also be calculated using the dist() function
  distMatrix <- dist(scale(m2))
  
  # Find clusters of words with hierarchical clustering
  fit <- hclust(distMatrix, method="ward.D")
  plot(fit)
}
