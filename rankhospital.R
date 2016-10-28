rankhospital <- function(state, outcome, num="best")
{
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeTypes <- c("heart attack", "heart failure", "pneumonia")
  stateList <- unique(outcomes[,7])
  outcomeColumn <- -1
  lowestRate <- -1
  
  if (is.na(match(state,stateList)))
    stop("invalid state")
  
  if (is.na(match(outcome,outcomeTypes)))
    stop("invalid outcome")
  else if (outcome == "heart attack")
    outcomeColumn <- 11
  else if (outcome == "heart failure")
    outcomeColumn <- 17
  else
    outcomeColumn <- 23

  outcomesSubSet <- outcomes[c(2,7,outcomeColumn)]
  outcomesSubSet <- outcomesSubSet[outcomesSubSet$State == state,]
  outcomesSubSet[,3] <- as.numeric(outcomesSubSet[,3])
  good <- complete.cases(outcomesSubSet)
  
  dataSet <- outcomesSubSet[good,]
  
  dataSet <- dataSet[with(dataSet,order(dataSet[,3])),]
  
  dataSet <- transform(dataSet,Rank=rank(dataSet[,3],ties.method="first"))
  
  dataSet[dataSet$Rank==num,1]
}