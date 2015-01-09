
library(tm)
library(NLP)
library(openNLP)
#library(openNLPmodels.en)

source("corpus-prepare.R")
source("text-features-visualization.R")
source("text-features.R")

rawCorpus <- Corpus(DirSource("data/essay"))

docData = prepareTextFeatures(rawCorpus)


