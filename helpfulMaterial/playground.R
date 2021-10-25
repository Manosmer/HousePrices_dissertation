houseprices <- read.csv("~/Desktop/Glasgow Project/housing.csv")
str(houseprices)

houseprices %>%
  select(-bath, -parking, -price) %>%
  gather(key="varname", value = "value") %>%
  ggplot(mapping = aes(x = varname, y = value)) +
  geom_boxplot() +
  xlab("Numerical Variables") + ylab("Values") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# bath and parking is factor
houseprices$bath <- factor(houseprices$bath)
houseprices$parking <- as.factor(houseprices$parking)

# extreme outlier removal
houseprices <- houseprices[-348,]


ggplot(houseprices, aes(y = price, fill = bath, colour = parking)) +
  geom_boxplot()

# histogram of price coloured by bath category
library(tidyverse)
library(ggridges)
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
