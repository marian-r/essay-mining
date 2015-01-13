
printTime = function(title) {
  print (title)
  print(Sys.time())
}

predictWord <- function(sent, position) {
  s = as.String(sent)  
  ann = annotateString(s)
  sent_ann = subset(ann, type == "sentence")
  word_ann = subset(ann, type == "word") 
  
  print(sent)
  print(paste("Finding:", s[word_ann[[position]]]))
  
  sent = sent_ann[[1]]  
  wordData = processSentence(s, ann, sent)
  
  #predict(model, wordData[position,], type = "probability")
  predict(model, wordData[position,], type = "class")
}

processText <- function(s, ann) {
  sent_ann = subset(ann, type == "sentence")
  
  predData = data.frame()
  
  for (j in 500:600) { #length(sent_ann)) {
    print(j)
    
    sent = sent_ann[j]
    wordData = processSentence(s, ann, sent)
    
    #print(wordData)
    
    predData = rbind(predData, wordData)
  }
  
  predData
}

processSentence <- function(s, ann, sent) {
  len = sent$end - sent$start
  
  wordPositions = sent$features[[1]][[1]]
  words = ann[wordPositions]
  wordCount = length(wordPositions)
  
  tagCount = table(unlist(words$features))
  
  nouns = sum(c(tagCount["NN"], tagCount["NNP"], tagCount["NNS"]), na.rm = TRUE)
  verbs = sum(c(tagCount["VB"], tagCount["VBD"], tagCount["VBG"], tagCount["VBN"], tagCount["VBP"], tagCount["VBZ"]), na.rm = TRUE)
  adjectives = sum(c(tagCount["JJ"], tagCount["JJR"], tagCount["JJS"]), na.rm = TRUE)
  
  sentData = data.frame(SentLen = len, WordCount = wordCount, Nouns = nouns, Verbs = verbs, Adj = adjectives)
  
  #print("Sentence data")
  #print(sentData)
  
  wordData = data.frame()
  
  for(i in 1:length(words)) {
    word = s[words[[i]]]
    
    before2 = NA
    before1 = NA
    after1 = NA
    after2 = NA
    before2tag = NA
    before1tag = NA
    after1tag = NA
    after2tag = NA
    
    if (i > 1) {
      wordItem = words[[i-1]]
      before1 = s[wordItem]
      before1tag = unlist(wordItem$features)
    }
    if (i > 2) {
      wordItem = words[[i-2]]
      before2 = s[wordItem]
      before2tag = unlist(wordItem$features)
    }
    
    if (i < length(words)) {
      wordItem = words[[i+1]]
      after1 = s[wordItem]
      after1tag = unlist(wordItem$features)
    }
    if (i+1 < length(words)) {
      wordItem = words[[i+2]]
      after2 = s[wordItem]
      after2tag = unlist(wordItem$features)
    }
    
    wordDataRow = data.frame(Word = as.character(word),
                             Before2 = as.character(before2), Before1 = as.character(before1),
                             After1 = as.character(after1), After2 = as.character(after2),
                             Before2Tag = as.character(before2tag), Before1Tag = as.character(before1tag),
                             After1Tag = as.character(after1tag), After2Tag = as.character(after2tag))
    
    wordDataRow = cbind(wordDataRow, sentData)
    
    #print("====== Word row ======")
    #print(wordDataRow)
    
    wordData = rbind(wordData, wordDataRow)
  }
  
  wordData
}

# get Hamlet
guttenbergCorpus = Corpus(URISource("http://www.gutenberg.org/cache/epub/1524/pg1524.txt"))
s = as.String(content(guttenbergCorpus[[1]]))
printTime("Before annotation")
ann = annotateString(s)
printTime("After annotation")

predData = processText(s, ann)
model = CoreModel(Word ~ ., data = predData, model = "knn")

sent = "A broken voice, and his whole function suiting With forms to his conceit?"
predictWord(sent, 5)
