library(tidyverse)

myseed <- 1635863582
houseprices <- read.csv("~/Desktop/Glasgow Project/housing.csv")
str(houseprices)

# bath and parking is factor
houseprices$bath <- factor(houseprices$bath)
houseprices$parking <- as.factor(houseprices$parking)

# extreme outlier removal
houseprices <- houseprices[c(-348),]


houseprices$logp <- log(houseprices$price)
houseprices$sqrtp <- sqrt(houseprices$price)

set.seed(myseed)
trainingIndices <- sample(1:499, 299)
validationIndices <- sample((1:499)[-trainingIndices], 150)
testIndices <- (1:499)[c(-trainingIndices, -validationIndices)]

trainingSet <- houseprices[trainingIndices,]
validationSet <- houseprices[validationIndices,]
testSet <- houseprices[testIndices,]


### TEST::: CROSS-VALIDATION ###
calculateMSPECV <- function(variableVector, dataset, responseVar, splits) {
  # Calculation of average MSPE via K-fold cross-validation
  
  dataSetLength <- nrow(dataset)
  k <- length(splits)
  if(k <= 1) stop("Data set splits must be more than 1.")
  
  MSPEs <- numeric(k)
  for(i in 1:k) {
    validSet <- dataset[splits[[i]], ]
    trainSet <- dataset[ (1:dataSetLength)[ -splits[[i]] ], ]
    
    model <- lm( paste(responseVar, joinVariables( variableVector ), sep=" ~ "), data=trainSet)
    MSPEs[i] <- calculateMSPE(model, validSet, responseVar) 
  }
  
  print(sum(MSPEs)/k)
  return(sum(MSPEs) / k)
}


selectVariablesCV <- function(variableVector, trainSet, responseVar, splits) {
  # Variable Selection via backward elimination using K-fold CV to calculate the MSPE
  
  if(length(splits) <= 1) stop("The number of splits in Cross-validation must be greater than 1.")
  
  ### RESULTS ###
  removedVariables <- c() # contains removed variables in order
  MSPEs <- c() # contains MSPE values in order
  
  
  ### Full model average MSPE ###
  minMSPE <- calculateMSPECV(variableVector, trainSet, responseVar, splits)
  
  # remove each variable one at a time and calculate MSPE
  removedVar <- 0
  bestFound <- FALSE
  while(!bestFound & length(variableVector) > 1) {
    bestFound <- TRUE # assume the best model is the one found from the previous iteration
    
    for(i in seq_along( variableVector )) {
      currentMSPE <- calculateMSPECV(variableVector[-i], trainSet, responseVar, splits)
      
      if(minMSPE >= currentMSPE) {
        bestFound <- FALSE # if something better has been found, best model has not been found yet
        minMSPE <- currentMSPE
        removedVar <- i
      }
    }
    
    if(!bestFound) {
      # some variable has indeed been removed
      removedVariables <- c(removedVariables, variableVector[removedVar])
      MSPEs <- c(MSPEs, minMSPE)
      variableVector <- variableVector[-removedVar]
    }
  }
  
  return(list(removedVariables = removedVariables, MSPEs = MSPEs, bestModel = variableVector))
}

teIndices <- sample(1:499, 50)
trIndices <- (1:499)[-teIndices]

teSet <- houseprices[teIndices,]
trSet <- houseprices[trIndices,]


set.seed(myseed)
k <- 10
trainSetLength <- length(trSet[[1]])
splitSize <- floor( (trainSetLength / k) + .5)

splits <- list()
splits[[1]] <- sample(1:trainSetLength, splitSize)
usedIndices <- splits[[1]]
for(i in 2:(k-1)) {
  splits[[i]] <- sample( ( 1:trainSetLength )[-usedIndices], splitSize )
  
  usedIndices <- c(usedIndices, splits[[i]])
}
splits[[k]] <- (1:trainSetLength)[-usedIndices]


selectVariablesCV(model_variables, trSet, "price", splits)

### TEST::: CROSS-VALIDATION ###




# without log: mention dist_am3 and probably parking
model <- lm(logp ~ dist_am3, data=houseprices)
summary(model)

model <- lm(logp ~ precip*bath + dist_am1*parking + sqft+ dist_am3, data = houseprices)
summary(model)

fullModel <- lm( paste("logp", joinVariables( model_variables ), sep=" ~ "), data=trainingSet)
selectVariables(model_variables, trainingSet, validationSet, "price")
ggplot(mapping=aes(x=model$residuals)) + 
  geom_histogram(bins = 40)



ggplot(data=houseprices, mapping = aes(y=log(price), fill=bath)) +
  geom_boxplot()

ggplot(data=houseprices, mapping = aes(x=log(price), fill=bath)) +
  geom_histogram(bins = 100)



model_variables <- c("bath", "parking", "precip*bath", "dist_am1*parking", "dist_am2", "dist_am3", "sqft", "elevation")

joinVariables <- function(variableVector) {
  if(length(variableVector) <= 0) stop("Variable vector must contain one or more elements.");
  
  if(length(variableVector) == 1) return(variableVector[1])
  
  result <- variableVector[1]
  for(var in variableVector[c(-1,-length(variableVector))] ) {
    result <- paste(result, var, sep=" + ")
  }
  
  return( paste(result, variableVector[length(variableVector)], sep=" + ") )
}
joinVariables(model_variables)

calculateMSPE <- function(model, dataset, responseVar) {
  return(sum( (dataset[[responseVar]] - predict(model, dataset))^2 )/nrow(dataset))
}


model_variables <- c("bath", "parking", "precip*bath", "dist_am1*parking", "dist_am2", "dist_am3", "sqft", "elevation*bath")

fullModel <- lm( paste("log(price)", joinVariables( model_variables), sep=" ~ "), data=houseprices)
plot(fullModel)
summary(fullModel)

selectVariables(model_variables, trainingSet, validationSet, "price")

model <- lm(log(price) ~ dist_am1*parking, data=houseprices)
summary(model)

cor(houseprices$price[houseprices$bath=="1"], houseprices$elevation[houseprices$bath=="1"])


plot(y=log(houseprices$price), houseprices$dist_am2)
plot(y=(houseprices$price), houseprices$dist_am3)

houseprices %>%
  ggplot(mapping = aes(y = log(price), x = precip)) +
  geom_point(size = 1.5, shape = 1) +
  geom_smooth(method = "lm", se = FALSE)

fullModel
