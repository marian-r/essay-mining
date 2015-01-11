
prepareSentFeatures <- function(annotations) {
  stat = data.frame()
  
  for(i in 1:length(annotations)) {
    print(i)
    ann = annotations[[i]]
    
    sent_ann = subset(ann, type == "sentence")
    
    tagCounts = sapply(sent_ann, function(sent) {
      wordPositions = sent$features[[1]][[1]]
      
      tagCount = table(unlist(ann[wordPositions]$features))
      
      nouns = sum(c(tagCount["NN"], tagCount["NNP"], tagCount["NNS"]), na.rm = TRUE)
      verbs = sum(c(tagCount["VB"], tagCount["VBD"], tagCount["VBG"], tagCount["VBN"], tagCount["VBP"], tagCount["VBZ"]), na.rm = TRUE)
      adjectives = sum(c(tagCount["JJ"], tagCount["JJR"], tagCount["JJS"]), na.rm = TRUE)
      
      c(Nouns = nouns, Verbs = verbs, Adj = adjectives)
    })
    
    averages = rowSums(tagCounts) / ncol(tagCounts)
    #print(averages)
    
    stat = rbind(stat, averages)
  }
  
  names(stat) = c("Nouns", "Verbs", "Adj")
  stat
}
