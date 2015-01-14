
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

# Part of Speech labels
#
# NN - Noun
# NNP - Proper noun
# NNS - Noun, plural
# VB - Verb
# VBD - Verb, past tense
# VBG - Verb, gerund or present participle
# VBN - Verb, past participle
# VBP - Verb, non-3rd person singular present
# VBZ - Verb, 3rd person singular present 
# DT - Determiner
# JJ - Adjective
# JJR - Adjective, comparative
# JJS - Adjective, superlative
# IN - Preposition or subordinating conjunction
# PRP - Personal pronoun 
# RB - Adverb
# RBR - Adverb, comparative
# RBS - Adverb, superlative 
# CC - Conjunction
# CD - Cardinal number
# ...
