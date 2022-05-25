library(naivebayes)

library(dplyr)


library(ggplot2)
library(psych)



 data <- read.csv("weather1.csv", header = T)
 head(data)

 xtabs(~ï..outlook+play, data = data)

 str(data)

 set.seed(1234)
 ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
 train <- data[ind == 1,]
 test <- data[ind == 2,]
 model <- naive_bayes(ï..outlook~ ., data = train, usekernel = T)

 plot(model)
 p <- predict(model, train, type = 'prob')

 head(cbind(p, train))


 p1 <- predict(model, train)

 (tab1 <- table(p1, train$ï..outlook))


1 - sum(diag(tab1)) / sum(tab1)

p2 <- predict(model, test)

(tab2 <- table(p2, test$ï..outlook))


1 - sum(diag(tab2)) / sum(tab2)
