
annotateSent <- function(string) {
  # Generate an annotator which computes sentence annotations 
  sent_ann <- Maxent_Sent_Token_Annotator()
  annotate(string, sent_ann)
}

annotateWord <- function(string, sentAnnotation) {
  word_ann = Maxent_Word_Token_Annotator()
  annotate(string, word_ann, sentAnnotation)
}


addSentWordData <- function(docData, i, doc) {
  sent_ann = annotateSent(doc)
  word_ann = annotateWord(doc, sent_ann)
  word_ann = subset(word_ann, type == "word")
  
  sent_ann = as.data.frame(sent_ann)
  word_ann = as.data.frame(word_ann)
  
  sentCount = nrow(sent_ann)
  sentLen = mean(sent_ann$end - sent_ann$start)
  wordCount = nrow(word_ann)
  wordLen = mean(word_ann$end - word_ann$start + 1)
  
  data.frame(SentCount = sentCount, SentLen = sentLen, WordCount = wordCount, WordLen = wordLen)
}

prepareTextFeatures <- function(rawCorpus) {
  docData = data.frame();
  
  for(i in 1:length(rawCorpus)) {
    print(i)
    doc = rawCorpus[[i]]
    
    rowData = addSentWordData(docData, i, doc)
    
    
    docData = rbind(docData, rowData)
  }
  
  docData
}
