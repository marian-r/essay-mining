
prepareForVocabularyML <- function(docData, wordStats) {
  mlDf = getGrades("data//vocabulary.txt")
  
  mlDf = cbind(mlDf, docData[, c("SentCount", "WordCount", "DT", "IN", "NN", "JJ")])
  #names(mlDf) = paste("a", names(mlDf), sep = "")
  
  mlDf = cbind(mlDf, wordStats)
  
  # get rid of invalidly annotated essays
  mlDf = mlDf[docData$SentLen < 200,]
  
  mlDf
}

prepareForStructureML <- function(docData, wordStats, sentStructure) {
  mlDf = getGrades("data//structure.txt")
  
  mlDf = cbind(mlDf, docData[, c("SentCount", "WordCount", "DT", "IN", "NN", "JJ")])
  mlDf = cbind(mlDf, wordStats)
  mlDf = cbind(mlDf, sentStructure)
  
  # get rid of invalidly annotated essays
  mlDf = mlDf[docData$SentLen < 200,]
  
  mlDf
}

getGrades <- function(filePath) {
  grades = read.table(filePath)
  
  names(grades) = "Grade"
  grades$Grade = as.factor(grades$Grade)
  
  grades
}
