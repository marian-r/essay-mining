
library(plyr)

annotateSent <- function(string) {
  # Generate an annotator which computes sentence annotations 
  sent_ann <- Maxent_Sent_Token_Annotator()
  annotate(string, sent_ann)
}

annotateWord <- function(string, sentAnnotation) {
  word_ann = Maxent_Word_Token_Annotator()
  annotate(string, word_ann, sentAnnotation)
}

annotatePOS <- function(string, wordAnnotation) {
  pos_ann <- Maxent_POS_Tag_Annotator()
  annotate(string, pos_ann, wordAnnotation)
}


addSentWordData <- function(doc) {
  sent_ann = annotateSent(doc)
  word_ann = annotateWord(doc, sent_ann)
  tag_ann = annotatePOS(doc, word_ann)
  
  word_ann = subset(word_ann, type == "word")
  tag_ann = subset(tag_ann, type == "word")
  
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

prepareTextFeatures <- function(rawCorpus, corpus) {
  docData = data.frame();
  
  for(i in 1:length(rawCorpus)) {
    print(i)
    
    doc = corpus[[i]]
    tfreq = termFreq(doc)
    mostUsed = names(which.max(tfreq))
    
    rowData = data.frame(MostUsed = mostUsed)
    
    doc = rawCorpus[[i]]
    rowData = cbind(rowData, addSentWordData(doc))
    
    #print(rowData)
    
    # rbind even if columns are different
    docData = rbind.fill(docData, rowData)
  }
  
  docData
}
