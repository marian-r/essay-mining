
library(gtools)
library(tm)
library(NLP)
library(openNLP)
#library(openNLPmodels.en)

source("corpus-prepare.R")
source("text/annotations.R")
source("text/features.R")
source("text/features-visualization.R")
source("text/sentence-structure.R")


fileNames = mixedsort(list.files("data/essay", full.names = TRUE))
rawCorpus <- Corpus(URISource(fileNames))

annotations = annotateCorpus(rawCorpus)
docData = prepareTextFeatures(annotations, corpus)
write.table(docData, "data/essay-features.txt", na = "0")
docData = read.table("data/essay-features.txt")

# show errors in sentence annotation
barplot(docData$SentLen)
boxplot(docData$SentLen)
#rawCorpus[docData$SentLen > 250]
#content(rawCorpus[docData$SentLen > 250])
#content(rawCorpus[docData$WordCount / docData$SentLen > 50])
#rawCorpus[docData$SentLen > 200 & docData$WordCount / docData$SentCount > 50]
#content(rawCorpus[docData$SentLen > 200 & docData$WordCount / docData$SentCount > 50])

#barplot(docData$WordCount / docData$SentLen)
#barplot(docData$WordCount / docData$SentCount)

# distribution of sentence count
hist(docData$SentCount, labels = TRUE)

# word count
boxplot(docData$WordCount)


tdm = TermDocumentMatrix(corpus)

showWordCloud(as.matrix(tdm), 150)
showClusters(tdm, 0.7)

# Find terms associated with "laugh" with correlation no less than 0.3 (ordered by their correlation with "laugh")
assoc = findAssocs(tdm, "laugh", 0.3)
pie(table(assoc), labels = rownames(assoc), col = c("#EDF393", "#F5E665", "#FFC472", "#FFA891", "#89BABE"))


sentStructure = prepareSentFeatures(annotations)
write.table(sentStructure, "data/essay-structure.txt")
sentStructure = read.table("data/essay-structure.txt")


dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
wordStats = getWordStats(dtm)
