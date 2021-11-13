library(tidyverse)
library(ggridges)

houseprices <- read.csv("~/Desktop/Glasgow Project/housing.csv")
str(houseprices)

# bath and parking is factor
houseprices$bath <- factor(houseprices$bath)
houseprices$parking <- as.factor(houseprices$parking)

# extreme outlier removal
houseprices <- houseprices[-348,]


# test: Parking Binary for dist_am1 dist_am3
houseprices$parkingBinary <- as.character(houseprices$parking)
houseprices$parkingBinary[houseprices$parkingBinary != "No Parking"] <- "Some Parking"
houseprices$parkingBinary <- as.factor(houseprices$parkingBinary)
levels(houseprices$parkingBinary)

# test: bath 1 or more binary for precip
houseprices$bathBinary <- as.character(houseprices$bath)
houseprices$bathBinary[houseprices$bathBinary != "1"] <- "More than 1"
houseprices$bathBinary <- as.factor(houseprices$bathBinary)
levels(houseprices$bathBinary)


houseprices %>%
  ggplot(mapping = aes(x = dist_am2, y = price, colour = parkingBinary, fill = bathBinary)) + 
  geom_point(size = 2, alpha=.6, shape = 1) + 
  geom_smooth(method = "lm", se = FALSE, lwd=.7)

cor(houseprices$price[houseprices$parking == "No Parking"],
    houseprices$dist_am3[houseprices$parking == "No Parking"])


mdddd <- lm(price ~ bath + parking, data = houseprices)
summary(mdddd)

crazymodel <- lm(price ~ bath + parking + (dist_am2:parkingBinary):bathBinary, data=houseprices)
summary(crazymodel)
step(crazymodel, direction = "backward")


md <- lm(price ~ bath, data = houseprices)
summary(md)
plot(md)

houseprices %>%
  ggplot(mapping = aes(x = dist_am2, y = price, colour = bathBinary, fill = parkingBinary )) + 
  geom_point(size = 2, alpha=.6, shape = 1) + 
  geom_smooth(method = "lm", se = FALSE, lwd=.7)

cor(houseprices$price[houseprices$parking == "Open" & houseprices$bath == "1"],
    houseprices$sqft[houseprices$parking == "Open" & houseprices$bath == "1"])


houseprices[houseprices$parking == "No Parking",] %>%
  ggplot(mapping = aes(x = dist_am3, y = price)) + 
  geom_point(size = 2, alpha=.6, shape = 1) + 
  geom_smooth(method = "lm", se = FALSE, lwd=.7)



cor(houseprices$precip[houseprices$bath == 1], houseprices$price[houseprices$bath == 1])





houseprices %>%
  select(-bath, -parking, -price) %>%
  gather(key="varname", value = "value") %>%
  ggplot(mapping = aes(x = varname, y = value)) +
  geom_boxplot() +
  xlab("Numerical Variables") + ylab("Values") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




ggplot(houseprices, aes(y = price, fill = bath, colour = parking)) +
  geom_boxplot()

# histogram of price coloured by bath category
ggplot(data=houseprices, mapping = aes(x=price, fill=bath)) +
  geom_histogram(bins=100)



#model fitting
md <- lm(price ~ ., data = houseprices)

library(moderndive)
gpoints <- get_regression_points(md)
colnames(gpoints)
gpoints <- data.frame(residual=md$residuals, fitted=md$fitted.values, bath=houseprices$bath, parking = houseprices$parking)
ggplot(gpoints, aes(x = fitted, y = residual)) +
  geom_point() +
  labs(x = "age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ bath+parking)

hist(resid(md))
plot(density(resid(md)))

plot(md)
qqnorm(resid(md))
summary(md)


plot(houseprices$parking, houseprices$price)
step(md, direction = "backward", trace=TRUE ) 


plot(as.factor(houseprices$sqft), md$fitted.values)
AIC(md)
BIC(md)




tt <- lm(Petal.Length ~ Species, data = iris)
plot(tt)


plot(houseprices$sqft, houseprices$price)
plot(houseprices$dist_am1,resid(md))



boxplot(houseprices$bath, houseprices$price)

train_ind <- sample(1:499, 499*0.8)
trainset <- houseprices[train_ind,]
validset <- houseprices[(1:499)[-train_ind],]

md <- lm(price ~ bath + log(sqft) , data = trainset)
plot(log(trainset$sqft),trainset$price)
ggplot(mapping = aes(x= trainset$precip, y= trainset$price, col=trainset$bath)) +
  geom_point() +
  geom_smooth(se = FALSE, method="lm")
plot(md)
validerr <- mean(abs(predict(md, validset) - validset$price))
validerr


sum(resid(md)^2)
hist(houseprices$price[houseprices$bath == 4], breaks = 8)


length(unique(houseprices)[[1]])


test <- lm(price ~ . -bathBinary  -parkingBinary, houseprices)
summary(test)



model_variables <- c("bath", "parking", "precip*bathBinary1 - bathBinary1", "dist_am1*parkingCNoP - parkingCNoP", "dist_am2", "dist_am3", "sqft", "elevation")

joinVariables <- function(variableVector) {
  if(length(variableVector) <= 0) return();
  
  result <- variableVector[1]
  for(i in 2:(length(variableVector)-1) ) {
    result <- paste(result, variableVector[i], sep=" + ")
  }
  
  return( paste(result, variableVector[length(variableVector)], sep=" + ") )
}
joinVariables(model_variables)

calculateMSPE <- function(model, dataset, responseVar) {
  return(sum( (dataset[[responseVar]] - predict(model, dataset))^2 )/nrow(dataset))
}


calculateMSPE()