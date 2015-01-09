
corpus_foreach <- function(corpus, func) {
  for(i in 1:length(corpus)) {
    print(i)
    doc = corpus[[i]]
    
    func(doc)
       
  }
}

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
  
  sent_ann = as.data.frame(sent_ann)
  word_ann = as.data.frame(word_ann)
  
  docData[i,"SentCount"] = nrow(sent_ann)
  docData[i,"SentLen"] = mean(sent_ann$end - sent_ann$start)
  docData[i,"WordCount"] = nrow(word_ann)
  docData[i,"WordLen"] = mean(word_ann$end - word_ann$start + 1)
}

prepareTextFeatures <- function(rawCorpus) {
  docData = data.frame();
  i = 1
  
  corpus_foreach(rawCorpus, function(doc) {
    addSentWordData(docData, i, doc)
    
    
    
    i = i + 1
  })
  
  
  docData
}
