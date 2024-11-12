rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab4")

library(ggfortify)
library(e1071)
library(class)
library(psych)


#### Load Data ####

wine <- read.table("wine.data", sep = ",", header = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

wine$Type <- as.factor(wine$Type)
wine.panels <- wine[,-c(4,5,10)]

pairs.panels(wine.panels[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine.panels$Type],pch=21)


#### Compute the PCs and plot the dataset using the 1st and 2nd PC ####

wine.pca <- prcomp(wine[,-1], center = TRUE, scale. = TRUE)
autoplot(wine.pca, data = wine, colour = 'Type', loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

wine.pca$rotation


#### Identify the variables that contribute the most to the 1st PC ####

pc1_contributions <- abs(wine.pca$rotation[,1])
top_contributors <- names(sort(pc1_contributions, decreasing = TRUE)[1:5])
top_contributors


#### Train a classifier model to predict wine type using the 13 attributes ####

set.seed(42)
train_idx <- sample(1:nrow(wine), size = 0.7 * nrow(wine))
train_data <- wine[train_idx, ]
test_data <- wine[-train_idx, ]

evaluate_model <- function(predictions, actual) {
  cm <- table(Predicted = predictions, Actual = actual)
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(precision, recall, f1)
}

knn_pred_all <- knn(train = train_data[,-1], test = test_data[,-1], cl = train_data$Type, k = 3)
cm_all <- table(Predicted = knn_pred_all, Actual = test_data$Type)
metrics_all <- evaluate_model(knn_pred_all, test_data$Type)


#### Train a classifier model to predict wine type using the data projected into the first 3 PCs ####

train_pca <- predict(wine.pca, train_data[,-1])
test_pca <- predict(wine.pca, test_data[,-1])

knn_pred_pca <- knn(train = train_pca[,1:3], test = test_pca[,1:3], cl = train_data$Type, k = 3)
cm_pca <- table(Predicted = knn_pred_pca, Actual = test_data$Type)
metrics_pca <- evaluate_model(knn_pred_pca, test_data$Type)

#### Drop the variables least contributing to the 1st PC and rerun PCA ####

least_contributors <- names(sort(pc1_contributions, decreasing = FALSE)[1:2])
wine_reduced <- wine[ , !(names(wine) %in% least_contributors)]

wine_reduced.pca <- prcomp(wine_reduced[,-1], center = TRUE, scale. = TRUE)


#### Train a classifier model to predict wine type using the data projected into the first 3 PCs after rerunning PCA ####

train_reduced_pca <- predict(wine_reduced.pca, train_data[ , !(names(train_data) %in% least_contributors)])
test_reduced_pca <- predict(wine_reduced.pca, test_data[ , !(names(test_data) %in% least_contributors)])

knn_pred_reduced_pca <- knn(train = train_reduced_pca[,1:3], test = test_reduced_pca[,1:3], cl = train_data$Type, k = 3)
cm_reduced_pca <- table(Predicted = knn_pred_reduced_pca, Actual = test_data$Type)
metrics_reduced_pca <- evaluate_model(knn_pred_reduced_pca, test_data$Type)

#### Compare the 3 classification models using contingency tables and prevision/recall/f1 metrics ####

print("Confusion Matrix and Metrics for Model 1 (All Attributes):")
cm_all
metrics_all

print("Confusion Matrix and Metrics for Model 2 (First 3 PCs):")
cm_pca
metrics_pca

print("Confusion Matrix and Metrics for Model 3 (Reduced First 3 PCs):")
cm_reduced_pca
metrics_reduced_pca
