library(ISLR)
str(iris)
head(iris)
summary(iris$Species)
any(is.na(iris))
var(iris[,1])
var(iris[,2])
var(iris[,3])
var(iris[,4])
standard.features <- scale(iris[1:4])
head(standard.features)
final.data <- cbind(standard.features,iris[5])

set.seed(101)
library(caTools)
sample <- sample.split(final.data$Species,SplitRatio=0.7)


train <- subset(final.data,sample ==T)  
test <- subset(final.data,sample ==F)

library(class)
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
print(mean(test$Species != predicted.species))


predicted.species <- NULL
error.rate <- NULL
for (i in 1:10) {
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}



library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl <- pl + geom_line(lty='dotted',color='red',size=2)

print(pl)