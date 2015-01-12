
library(plyr)

addSentWordData <- function(ann) {  
  sent_ann = subset(ann, type == "sentence")
  word_ann = subset(ann, type == "word")
  tag_ann = word_ann
  
  sent_ann = as.data.frame(sent_ann)
  word_ann = as.data.frame(word_ann)
  
  sentCount = nrow(sent_ann)
  sentLen = mean(sent_ann$end - sent_ann$start)
  wordCount = nrow(word_ann)
  wordLen = mean(word_ann$end - word_ann$start + 1)
  
  tagCount = table(unlist(tag_ann$features))
  tagCountDF = as.vector(tagCount)
  names(tagCountDF) = names(tagCount)
  
  vec = c(SentCount = sentCount, SentLen = sentLen, WordCount = wordCount, WordLen = wordLen)
  # convert to row of a data frame
  as.data.frame(t(c(vec, tagCountDF)))
}

prepareTextFeatures <- function(annotations, corpus) {
  docData = data.frame();
  
  for(i in 1:length(annotations)) {
    print(i)
    
    doc = corpus[[i]]
    tfreq = termFreq(doc)
    mostUsed = names(which.max(tfreq))
    
    rowData = data.frame(MostUsed = mostUsed)
    
    ann = annotations[[i]]
    rowData = cbind(rowData, addSentWordData(ann))
    
    #print(rowData)
    
    # rbind even if columns are different
    docData = rbind.fill(docData, rowData)
  }
  
  docData
}

getWordStats <- function(dtm) {
  distinctCount = rowSums(as.matrix(dtm > 0))
  totalWeight = rowSums(as.matrix(dtm))
  
  
  frequent = removeSparseTerms(dtm, sparse = 0.8)
  rare = dtm[,!(colnames(dtm) %in% colnames(frequent))]
  
  # attributes based on rare words
  rareCount = rowSums(as.matrix(rare > 0))
  rareWeight = rowSums(as.matrix(rare))
  
  data.frame(DistinctCount = distinctCount, TotalWeight = totalWeight, RareCount = rareCount, RareWeight = rareWeight)
}
