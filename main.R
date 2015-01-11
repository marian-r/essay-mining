
library(gtools)
library(tm)
library(NLP)
library(openNLP)
#library(openNLPmodels.en)

source("corpus-prepare.R")
source("text/annotations.R")
source("text-features-visualization.R")
source("text/features.R")
source("text/sentence-structure.R")


fileNames = mixedsort(list.files("data/essay", full.names = TRUE))
rawCorpus <- Corpus(URISource(fileNames))

annotations = annotateCorpus(rawCorpus)
docData = prepareTextFeatures(annotations, corpus)
write.table(docData, "data/essay-features.txt", na = "0")
docData = read.table("data/essay-features.txt")

sentStructure = prepareSentFeatures(annotations)
write.table(sentStructure, "data/essay-structure.txt")
sentStructure = read.table("data/essay-structure.txt")

