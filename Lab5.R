library(ggfortify)
library(e1071)
library(class)
library(caret)
library(psych)

#### SVMC ####

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab5")

wine <- read.table("wine.data", sep = ",", header = FALSE)
names(wine) <- c("Type", "Alcohol", "Malic acid","Ash", "Alcalinity of ash",
                 "Magnesium", "Total phenols", "Flavanoids", 
                 "Nonflavanoid Phenols","Proanthocyanins","Color Intensity", 
                 "Hue", "Od280/od315 of diluted wines", "Proline")

wine$Type <- as.factor(wine$Type)

selected_features <- c("Flavanoids", "Total phenols", "Proanthocyanins",
                       "Od280/od315 of diluted wines", "Nonflavanoid Phenols")

evaluate_model <- function(predictions, actual) {
  cm <- table(Predicted = predictions, Actual = actual)
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(precision, recall, f1)
}

set.seed(42)
train_idx <- sample(1:nrow(wine), size = 0.7 * nrow(wine))
train_data <- wine[train_idx, ]
test_data <- wine[-train_idx, ]

train_x <- train_data[, selected_features]
train_y <- train_data$Type
test_x <- test_data[, selected_features]
test_y <- test_data$Type

linear_svm <- tune.svm(Type ~ ., data = train_data[, c(selected_features, "Type")], 
                       kernel = "linear", 
                       cost = 10^(-1:2))
linear_pred <- predict(linear_svm$best.model, test_x)
linear_svm_cm <- table(Predicted = linear_pred, Actual = test_y)
linear_svm_metrics <- evaluate_model(linear_pred, test_y)

radial_svm <- tune.svm(Type ~ ., data = train_data[, c(selected_features, "Type")], 
                       kernel = "radial", 
                       cost = 10^(-1:2), gamma = 10^(-2:1))
radial_pred <- predict(radial_svm$best.model, test_x)
radial_svm_cm <- table(Predicted = radial_pred, Actual = test_y)
radial_svm_metrics <- evaluate_model(radial_pred, test_y)

knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)
knn_cm <- table(Predicted = knn_pred, Actual = test_y)
knn_metrics <- evaluate_model(knn_pred, test_y)

linear_svm_cm
radial_svm_cm
knn_cm

linear_svm_metrics
radial_svm_metrics
knn_metrics

#### SVMR ####

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab5")

house  <- read.csv("NY-House-Dataset.csv")

house$LOGPRICE <- log(as.numeric(house$PRICE))
house$LOGPROPERTYSQFT <- log(as.numeric(house$PROPERTYSQFT))

svm_model <- svm(LOGPRICE ~ LOGPROPERTYSQFT, data = house, type = "eps-regression")
lm_model <- lm(LOGPRICE ~ LOGPROPERTYSQFT, data = house)

house$SVM <- predict(svm_model, house)
house$LM <- predict(lm_model, house)

ggplot(house, aes(x = LOGPROPERTYSQFT)) +
  geom_point(aes(y = LOGPRICE), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SVM), color = "red") +
  geom_line(aes(y = LM), color = "green") +
  labs(title = "SVM vs Linear Regression",
       x = "Log Square Footage",
       y = "Log Price") +
  theme_minimal()
