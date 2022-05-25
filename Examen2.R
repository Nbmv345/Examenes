#EJERCICIO 1
car.df <- read.csv("ToyotaCorolla.csv")
car.df <- car.df[1:1000, ]
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
View(car.df)

set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
car.lm <- lm(Price ~ ., data = train.df)
options(scipen = 999)

summary(car.lm)

library(forecast)

car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)


car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1000)])/400


set.seed(123)
train.rows <- sample(rownames(car.df), dim(car.df)[1]*0.5)
set.seed(123)
valid.rows <- sample(setdiff(rownames(car.df), train.rows),dim(car.df)[1]*0.3)
set.seed(123)
test.rows <- setdiff(rownames(car.df), union(train.rows, valid.rows))

train.data <- car.df[train.rows, ]
valid.data <- car.df[valid.rows, ]
test.data <- car.df[test.rows, ]


car.df <- lm(Price ~ ., data = test.data)
summary(car.df)

#---------------------------------------------------------------------------------


#EJERCICIO 2

library(dummies)
library(GGally)
library(gplots)

toyota.df<- read.csv("ToyotaCorolla.csv")

str(toyota.df)

#b)
#PASAMOS A FACTORES 

toyota.df$Mfg_Year <- factor(toyota.df$Mfg_Year)


toyotadummy.df <- dummy.data.frame(toyota.df[,-c(1,2,5,6,11,15)])
toyotadummy.df


#Matriz de correlacion
Cor_Matrix <- round(cor(toyotadummy.df),2)
Cor_Matrix

#c)


Toyota_a <- toyotadummy.df[,-c(8,9,15,16,29,30,31,32,33,34,35)]
Toyota_a

#Heatmap to analyse all data
heatmap.2(cor(Toyota_a), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_a),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

Toyota_Co.df <- toyotadummy.df[,c(1,2,3,7,10,11,12,13,14,17)]
Toyota_Co.df



heatmap.2(cor(Toyota_Co.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Co.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#d)



Toyota_Drop.df <- Toyota_Co.df[,-c(5,6,7,10)]
Toyota_Drop.df




heatmap.2(cor(Toyota_Drop.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(Toyota_Drop.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


#e)
#grafica
y<-Toyota_Drop.df
y

Cor_Matrix <- round(cor(y),2)
Cor_Matrix

ggpairs(Toyota_Drop.df)


