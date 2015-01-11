
annotateCorpus <- function(corpus) {
  sent_ann = Maxent_Sent_Token_Annotator()
  word_ann = Maxent_Word_Token_Annotator()
  tag_ann = Maxent_POS_Tag_Annotator()
  
  result = list()
  
  for(i in 1:length(corpus)) {
    print(i)
    
    doc = corpus[[i]]
    result[[i]] = annotate(doc, list(sent_ann, word_ann, tag_ann))
  }
  
  result
}

annotateString <- function(string) {
  sent_ann = Maxent_Sent_Token_Annotator()
  word_ann = Maxent_Word_Token_Annotator() 
  tag_ann = Maxent_POS_Tag_Annotator() 
  annotate(string, list(sent_ann, word_ann, tag_ann))
}
