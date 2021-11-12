# DONE: elevation, sqft, precip, dist_am1, dist_am2, dist_am3

library(tidyverse)
library(ggridges)

houseprices <- read.csv("~/Desktop/Glasgow Project/housing.csv")
str(houseprices)

# bath and parking is factor
houseprices$bath <- factor(houseprices$bath)
houseprices$parking <- as.factor(houseprices$parking)

# extreme outlier removal
houseprices <- houseprices[-348,]



# test: Parking Open or something else
houseprices$parkingBinaryOpen <- as.character(houseprices$parking)
houseprices$parkingBinaryOpen[houseprices$parkingBinaryOpen != "Open"] <- "Not Open"
houseprices$parkingBinaryOpen <- as.factor(houseprices$parkingBinaryOpen)
levels(houseprices$parkingBinaryOpen)

# test: Parking NoParking or something else
houseprices$parkingBinaryNoPa <- as.character(houseprices$parking)
houseprices$parkingBinaryNoPa[houseprices$parkingBinaryNoPa != "No Parking"] <- "Some Parking or Not Provided"
houseprices$parkingBinaryNoPa <- as.factor(houseprices$parkingBinaryNoPa)
levels(houseprices$parkingBinaryNoPa)

# test: Parking Covered/NotProvided or something else
houseprices$parkingBinaryCNotPr <- as.character(houseprices$parking)
houseprices$parkingBinaryCNotPr[houseprices$parkingBinaryCNotPr == "Covered" | houseprices$parkingBinaryCNotPr == "Not Provided"] <- "Covered or Not Provided"
houseprices$parkingBinaryCNotPr <- as.factor(houseprices$parkingBinaryCNotPr)
levels(houseprices$parkingBinaryCNotPr)

# test: Parking Covered/NotProvided or something else
houseprices$parkingBinaryt <- as.character(houseprices$parking)
houseprices$parkingBinaryt[houseprices$parkingBinaryt == "Covered" | houseprices$parkingBinaryt == "Not Provided"] <- "Covered or Not Provided"
houseprices$parkingBinaryt[houseprices$parkingBinaryt == "Open" | houseprices$parkingBinaryt == "No Parking"] <- "Open or No"
houseprices$parkingBinaryt <- as.factor(houseprices$parkingBinaryt)
levels(houseprices$parkingBinaryt)

# test: bath 1 or more binary 
houseprices$bathBinary1 <- as.character(houseprices$bath)
houseprices$bathBinary1[houseprices$bathBinary1 != "1"] <- "More than 1"
houseprices$bathBinary1 <- as.factor(houseprices$bathBinary1)
levels(houseprices$bathBinary1)

# test: bath 4 or less binary 
houseprices$bathBinary4 <- as.character(houseprices$bath)
houseprices$bathBinary4[houseprices$bathBinary4 != "1"] <- "Less than 4"
houseprices$bathBinary4 <- as.factor(houseprices$bathBinary4)
levels(houseprices$bathBinary4)


set.seed(1635863590)
trainingIndices <- sample(1:499, 299)
validationIndices <- sample((1:499)[-trainingIndices], 150)
testIndices <- (1:499)[c(-trainingIndices, -validationIndices)]

trainingSet <- houseprices[trainingIndices,]
validationSet <- houseprices[validationIndices,]
testSet <- houseprices[testIndices,]



### EDA ###
houseprices %>%
  ggplot( mapping = aes( y = price, x = dist_am3, colour = bath, fill = parking) ) + 
  geom_point(shape = 1, size = 2.5) + 
  geom_smooth(method = "lm", se = FALSE)


cor(houseprices$price[houseprices$bath=="1" & houseprices$parking=="No Parking"], houseprices$dist_am2[houseprices$bath=="1" & houseprices$parking=="No Parking"] )


md <- lm(price ~ bath + precip:bathBinary1 + dist_am1:parkingBinaryCNotPr, data = trainingSet)
summary(md)
step(md, direction = "backward")

md1 <- lm(price ~ bath + parking, data = trainingSet)
summary(md1)

step(md, direction = "backward")


fullModel <- lm(price ~ bath + parking + precip*bathBinary1 - bathBinary1 + dist_am1*parkingBinaryCNotPr - parkingBinaryCNotPr + dist_am2 + dist_am3 + sqft + elevation, data = trainingSet)

### MSPE ###
sum( (validationSet$price - predict(fullModel, validationSet))^2 )/nrow(validationSet)


### MAPE ###
sum( abs( validationSet$price - predict(md, validationSet) ) )/nrow(validationSet)




### models that are better than price ~ bath + parking ###
#1 - BEST
md <- lm(price ~ bath + precip*bathBinary1 - bathBinary1, data = trainingSet)
summary(md)

# PROOF of preci:Bath
houseprices[houseprices$bath != "1",] %>%
  ggplot( mapping = aes( y = price, x = precip, colour = bath, fill = parking) ) + 
  geom_point(shape = 1, size = 2.5) + 
  geom_smooth(method = "lm", se = FALSE)

#irrespective of the parking category, when bath=1 precip has a significant negative effect on price
houseprices[houseprices$bath == "1",] %>%
  ggplot( mapping = aes( y = price, x = precip, colour = parking) ) + 
  geom_point(shape = 1, size = 2.5) + 
  geom_smooth(method = "lm", se = FALSE)


#2
md <- lm(price ~ bath + precip*bathBinary1 - bathBinary1 + dist_am1*parkingBinaryCNotPr - parkingBinaryCNotPr, data = trainingSet)
# PROOF
houseprices %>%
  ggplot( mapping = aes( y = price, x = dist_am1, colour = parking) ) + 
  geom_point(shape = 1, size = 2.5) + 
  geom_smooth(method = "lm", se = FALSE)

houseprices %>%
  ggplot( mapping = aes( y = price, x = dist_am1, colour = parkingBinaryCNotPr) ) + 
  geom_point(shape = 1, size = 2.5) + 
  geom_smooth(method = "lm", se = FALSE)



