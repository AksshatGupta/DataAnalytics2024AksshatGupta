###### Exercise 1 ######

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab3")

library(e1071)

abalone_data <- read.csv("abalone/abalone.data", header = FALSE)
colnames(abalone_data) <- c("Sex", "Length", "Diameter", "Height", 
                            "Whole_Weight", "Shucked_Weight", 
                            "Viscera_Weight", "Shell_Weight", "Rings")

abalone_data$age.group <- cut(abalone_data$Rings, br=c(0,8,11,35), 
                              labels = c("young", 'adult', 'old')) 

subset1 <- abalone_data[, c("Length", "Diameter", "Height", "age.group")]
subset2 <- abalone_data[, c("Whole_Weight", "Shucked_Weight", "Shell_Weight", 
                            "age.group")]
subset3 <- abalone_data[, c("Viscera_Weight", "Length", "Diameter", 
                            "age.group")]

model_subset1 <- naiveBayes(age.group ~ ., data = subset1)
model_subset2 <- naiveBayes(age.group ~ ., data = subset2)
model_subset3 <- naiveBayes(age.group ~ ., data = subset3)

pred_subset1 <- predict(model_subset1, subset1)
table(predicted = pred_subset1, actual = subset1$age.group)

pred_subset2 <- predict(model_subset2, subset2)
table(predicted = pred_subset2, actual = subset2$age.group)

pred_subset3 <- predict(model_subset3, subset3)
table(predicted = pred_subset3, actual = subset3$age.group)

plot(function(x) dnorm(x, mean(abalone_data$Length), sd(abalone_data$Length)), 
     0, max(abalone_data$Length), col="red", xlab="Length", ylab="age.group", 
     main="age.group for 3 different features")
curve(dnorm(x, mean(abalone_data$Diameter), sd(abalone_data$Diameter)), 
      add=TRUE, col="blue")
curve(dnorm(x, mean(abalone_data$Height), sd(abalone_data$Height)), 
      add=TRUE, col="green")

plot(function(x) dnorm(x, mean(abalone_data$Whole_Weight), 
                       sd(abalone_data$Whole_Weight)), 0, 
     max(abalone_data$Whole_Weight), col="red", xlab="Whole Weight", 
     ylab="age.group", main="age.group for 3 different features")
curve(dnorm(x, mean(abalone_data$Shucked_Weight), 
            sd(abalone_data$Shucked_Weight)), add=TRUE, col="blue")
curve(dnorm(x, mean(abalone_data$Shell_Weight), sd(abalone_data$Shell_Weight)), 
      add=TRUE, col="green")

plot(function(x) dnorm(x, mean(abalone_data$Viscera_Weight), 
                       sd(abalone_data$Viscera_Weight)), 0, 
     max(abalone_data$Viscera_Weight), col="red", xlab="Viscera Weight", 
     ylab="age.group", main="age.group for 3 different features")
curve(dnorm(x, mean(abalone_data$Length), sd(abalone_data$Length)), add=TRUE, 
      col="blue")
curve(dnorm(x, mean(abalone_data$Diameter), sd(abalone_data$Diameter)), 
      add=TRUE, col="green")



###### Exercise 2 ######

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab3")

library(class)

iris_data <- read.csv("iris.csv")
iris_data <- iris_data[,-1]

iris_data.norm <- iris_data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
iris_data.norm[1:4] <- as.data.frame(lapply(iris_data.norm[1:4], normalize))

set.seed(123)
s_iris <- sample(1:nrow(iris_data.norm), 0.7 * nrow(iris_data.norm))

iris_train <- iris_data.norm[s_iris,]
iris_test <- iris_data.norm[-s_iris,]
iris_train_labels <- iris_data[s_iris, 5]
iris_test_labels <- iris_data[-s_iris, 5]

train_subset1 <- iris_train[, 1:2]
test_subset1 <- iris_test[, 1:2]

train_subset2 <- iris_train[, 3:4]
test_subset2 <- iris_test[, 3:4]

ks <- c(3, 5, 7, 9, 11, 13, 15)

accuracy_subset1 <- c()
accuracy_subset2 <- c()

for (k in ks) {
  KNNpred_subset1 <- knn(train = train_subset1, test = test_subset1, 
                         cl = iris_train_labels, k = k)
  cm_subset1 <- table(KNNpred_subset1, iris_test_labels)
  accuracy_subset1 <- c(accuracy_subset1, 
                        sum(diag(cm_subset1)) / length(iris_test_labels))
  KNNpred_subset2 <- knn(train = train_subset2, test = test_subset2,
                         cl = iris_train_labels, k = k)
  cm_subset2 <- as.matrix(table(KNNpred_subset2, iris_test_labels))
  accuracy_subset2 <- c(accuracy_subset2,
                        sum(diag(cm_subset2)) / length(iris_test_labels))
  
  print(cm_subset2)
  print(accuracy_subset2)
}

plot(ks, accuracy_subset1, type = "b", col = "blue", ylim = c(0.4, 1), 
     xlab = "k", ylab = "Accuracy", 
     main = "Accuracy for Two Different Subsets")
lines(ks, accuracy_subset2, type = "b", col = "red")
legend("bottomright", legend = c("Sepal Length & Width", 
                                 "Petal Length & Width"), 
       col = c("blue", "red"), lty = 1)

KNNpred_best_subset1 <- knn(train = train_subset1, 
                            test = test_subset1, cl = iris_train_labels, k = 11)
contingency_table_subset1 <- table(KNNpred_best_subset1, iris_test_labels)
print(contingency_table_subset1)

KNNpred_best_subset2 <- knn(train = train_subset2, 
                            test = test_subset2, cl = iris_train_labels, k = 5)
contingency_table_subset2 <- table(KNNpred_best_subset2, iris_test_labels)
print(contingency_table_subset2)



###### Exercise 3 ######

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab3")

library(ggplot2)

### iris

iris_data <- read.csv("iris.csv")
iris_data <- iris_data[,-1]

ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

set.seed(123)

iris_data.km <- kmeans(iris_data[,-5], centers = 3)
assigned.clusters <- as.factor(iris_data.km$cluster)
ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5)

for (k in ks) {
  iris_data.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris_data.km$tot.withinss)
}

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)

labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"

table(labeled.clusters, iris[,5])


### abalone

abalone_data <- read.csv("abalone/abalone.data", header = FALSE)
colnames(abalone_data) <- c("Sex", "Length", "Diameter", "Height", 
                            "Whole_Weight", "Shucked_Weight", 
                            "Viscera_Weight", "Shell_Weight", "Rings")

abalone_data <- abalone_data[-1]

abalone_data$age.group <- cut(abalone_data$Rings, br=c(0,8,11,35), 
                              labels = c("young", 'adult', 'old')) 

abalone_data <- abalone_data[-8]

ggplot(abalone_data, aes(x = Length, y = Height, colour = age.group)) +
  geom_point()

set.seed(123)

abalone_data.km <- kmeans(abalone_data[,-8], centers = 3)
assigned.clusters <- as.factor(abalone_data.km$cluster)
ggplot(abalone_data, aes(x = Length, y = Height, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5)

for (k in ks) {
  abalone_data.km <- kmeans(abalone_data[,-8], centers = k)
  wss <- c(wss,abalone_data.km$tot.withinss)
}

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)

labeled.clusters[labeled.clusters==1] <- "young"
labeled.clusters[labeled.clusters==2] <- "adult"
labeled.clusters[labeled.clusters==3] <- "old"

table(labeled.clusters, abalone_data[,8])
