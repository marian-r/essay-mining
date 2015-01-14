
# we can optionally install models for English language 
#install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")

library(gtools)
library(tm)
library(NLP)
library(openNLP)
#library(openNLPmodels.en)

source("text/annotations.R")
source("text/features.R")
source("text/features-visualization.R")
source("text/sentence-structure.R")

# get files in numerical order
fileNames = mixedsort(list.files("data/essay", full.names = TRUE))
rawCorpus <- Corpus(URISource(fileNames))

# annotate - sentences, words, part-of-speech
annotations = annotateCorpus(rawCorpus)

# process corpus
source("corpus-prepare.R")

# text features
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



# sentence structure
sentStructure = prepareSentFeatures(annotations)
write.table(sentStructure, "data/essay-structure.txt")
sentStructure = read.table("data/essay-structure.txt")


#
# prepare for ML
#

source("ML/prepare.R")

dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
wordStats = getWordStats(dtm)

mlDf = prepareForVocabularyML(docData, wordStats)
mlDf = prepareForStructureML(docData, wordStats, sentStructure)


#
# ML
#

library(CORElearn)
library(rpart)
library(ipred)

sort(attrEval(Grade ~ ., mlDf, "Relief"), decreasing = TRUE)
sort(attrEval(Grade ~ ., mlDf, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(Grade ~ ., mlDf, "MDL"), decreasing = TRUE)

source("ML//wrapper.R")
wrapper(mlDf, className="Grade", classModel="tree", folds=10)
#Grade ~ SentCount + RareCount 26.23
#Grade ~ DT + RareCount + NN + JJ + IN + DistinctCount + SentCount = 26.52
#Grade ~ SentCount + RareWeight + TotalWeight + NN + IN + RareCount = 26.66
#Grade ~ SentCount + RareWeight = 26.66
#Grade ~ SentCount + TotalWeight + RareWeight = 26.81
# Sentence stucture
#Grade ~ IN + Verbs + Adj + DT + SentCount + JJ = 34.35
#Grade ~ WordCount + Verbs + IN = 34.78
#Grade ~ IN + Verbs + SentCount = 34.20
wrapper(mlDf, className="Grade", classModel="knn", folds=5)
#Grade ~ DistinctCount + WordCount + TotalWeight 25.65
#Grade ~ WordCount + RareCount + DistinctCount + NN + DT = 26.23
#Grade ~ DistinctCount + DT + RareCount = 25.80
#Grade ~ RareCount + JJ + DistinctCount + IN = 25.94
#Grade ~ DistinctCount + TotalWeight + WordCount + DT = 26.23
# Sentence stucture
#Grade ~ DistinctCount + DT + WordCount + RareCount + IN + TotalWeight + JJ + Adj + SentCount + NN = 33.91
#Grade ~ DistinctCount + SentCount + Adj + WordCount + RareCount + DT = 34.49
#Grade ~ IN + WordCount + RareCount + SentCount + Adj + DT + TotalWeight + NN + Verbs + JJ = 33.48
#Grade ~ SentCount + RareCount + IN + TotalWeight + Adj + WordCount + DT = 33.62
wrapper(mlDf, className="Grade", classModel="rf", folds=5)
#Grade ~ RareCount + DistinctCount + WordCount = 25.51
# Sentence stucture
#Grade ~ DistinctCount + WordCount + SentCount + RareCount = 34.78

# force the predict function to return class labels only (and not the class probabilities...)
mypredict <- function(object, newdata) { predict(object, newdata, type = "class") }
# force the CoreModel function to train a model of a given type (specified by the parameter "target.model")
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
# force the predict function to return class labels only and also destroy the internal representation of a given model
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

#
# decision trees (rpart)
#

# 10-fold cross-validation of decision trees for the complete dataset
# errorest(formula, data, model (function), predict (function))
errorest(Grade ~ ., data=mlDf, model = rpart, predict = mypredict)

model = CoreModel(Grade ~ SentCount + RareCount, data = mlDf, model = "tree")
plot(model, mlDf)

#
# CORElearn - decision trees, k-NN, random forests
#

# 10-fold cross validation 
errorest(Grade ~ SentCount + TotalWeight + RareWeight, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
errorest(Grade ~ DistinctCount + DT + RareCount, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "knn")
errorest(Grade ~ RareCount + DistinctCount + WordCount, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "rf")

errorest(Grade ~ ., data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
errorest(Grade ~ ., data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "bayes")
errorest(Grade ~ ., data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "knn")
errorest(Grade ~ ., data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "rf")


#
# grading based on sentence structure
#

errorest(Grade ~ IN + Verbs + SentCount, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
errorest(Grade ~ SentCount + RareCount + IN + TotalWeight + Adj + WordCount + DT, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "knn")
errorest(Grade ~ DistinctCount + WordCount + SentCount + RareCount, data = mlDf, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "rf")

