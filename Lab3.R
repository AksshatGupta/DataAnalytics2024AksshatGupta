rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Homeworks")

library(ggplot2)
library(class)

epi2024results <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)
epi <- epi2024results


#### 1. Derive 2 subsets each for a particular region ####

subset_global_west <- subset(epi, region == "Global West")
subset_asia_pacific <- subset(epi, region == "Asia-Pacific")

### 1.1. Plot histograms for a variable of your choice for both regions with density lines overlayed

ggplot(subset_global_west, aes(x=ECO)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=5, fill="lightblue", color="black") +
  geom_density(color="red", linewidth=1) +
  ggtitle("Histogram with Density Curve: Global West (ECO)") +
  xlab("ECO") +
  ylab("Density")

ggplot(subset_asia_pacific, aes(x=ECO)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=5, fill="lightblue", color="black") +
  geom_density(color="red", linewidth=1) +
  ggtitle("Histogram with Density Curve: Asia-Pacific (ECO)") +
  xlab("ECO") +
  ylab("Density")

### 1.2. Plot QQ plots for both variables compared to known probability distributions.

qqnorm(subset_global_west$ECO, main="QQ Plot: Global West (ECO)")
qqline(subset_global_west$ECO, col="red")

qqnorm(subset_asia_pacific$ECO, main="QQ Plot: Asia-Pacific (ECO)")
qqline(subset_asia_pacific$ECO, col="red")



#### 2. Fit linear models as follows: ####
# 2.1. Choose a subset of 5 variables (excluding EPI) and using the formula 
# EPI~VAR1+VAR2+VAR3+VAR4+VAR5, fit a linear model and identify which variable 
# most significantly influences EPI. Plot that variable with another and overlay the fitted line.

model1 <- lm(EPI ~ ECO + BDH + TBN + TKP + AGR, data = epi)
summary(model1)

ggplot(epi, aes(x=ECO, y=BDH)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Scatter Plot of ECO vs BDH with Fitted Line") +
  xlab("ECO") +
  ylab("BDH")

# 2.2. Repeat the previous model with a subset of 1 region and in 1-2 sentences 
# explain which model is a better fit and why you think that is the case.

model2 <- lm(EPI ~ ECO + BDH + TBN + TKP + AGR, data = subset_global_west)
summary(model2)

ggplot(subset_global_west, aes(x=ECO, y=BDH)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  ggtitle("Scatter Plot of ECO vs BDH with Fitted Line") +
  xlab("ECO") +
  ylab("BDH")

# Model 1 Adjusted R-squared = 0.8205 while Model 2 Adjusted R-squared = 0.7504.
# This means model 1 captures more of the variance in EPI.



#### 3. Train 2 kNN models using ”region” as the class label as follows: ####
# 3.1. Choose a subset of 5 variables and filter the subset by region keeping 3 
# regions out of 8 (representing 3 classes), then train a kNN model to predict 
# the region based on these variables. Evaluate the model using a contingency 
# matrix and calculate the accuracy of correct classifications.

region_subset1 <- subset(epi, region %in% c("Global West", "Asia-Pacific", "Eastern Europe"))
region_subset1 <- region_subset1[, c("region", "ECO", "BDH", "TBN", "TKP", "AGR")]

accuracy_region_subset1 <- numeric(9)
ks <- seq(2, 10, 1)

for (k in ks) {
  n <- nrow(region_subset1)
  train_indexes <- sample(n, n * 0.7)
  
  train_data1 <- region_subset1[train_indexes, ]
  test_data1 <- region_subset1[-train_indexes, ]
  
  train_x1 <- train_data1[, -1]
  train_y1 <- train_data1$region
  test_x1 <- test_data1[, -1]
  test_y1 <- test_data1$region
  
  KNNpred1 <- knn(train = train_x1, test = test_x1, cl = train_y1, k = 5)
  contingency_table1 <- table(Predicted = KNNpred1, Actual = test_y1)
  
  contingency_matrix1 <- as.matrix(contingency_table1)
  accuracy_region_subset1[k-1] <- sum(diag(contingency_matrix1)) / length(test_y1)
}

average_accuracy_region_subset1 <- mean(accuracy_region_subset1)
print(paste("Average Accuracy of kNN Model 1:", average_accuracy_region_subset1))

# 3.2. Repeat the previous model with the same variables for another set of 3
# other regions and evaluate. In 1-2 sentences explain which model is better 
# and why you think that is the case.

region_subset2 <- subset(epi, region %in% c("Greater Middle East", "Latin America & Caribbean", "Sub-Saharan Africa"))
region_subset2 <- region_subset2[, c("region", "ECO", "BDH", "TBN", "TKP", "AGR")]

accuracy_region_subset2 <- numeric(9)
ks <- seq(2, 10, 1)

for (k in ks) {
  n <- nrow(region_subset2)
  train_indexes <- sample(n, n * 0.7)
  
  train_data2 <- region_subset2[train_indexes, ]
  test_data2 <- region_subset2[-train_indexes, ]
  
  train_x2 <- train_data2[, -1]
  train_y2 <- train_data2$region
  test_x2 <- test_data2[, -1]
  test_y2 <- test_data2$region
  
  KNNpred2 <- knn(train = train_x2, test = test_x2, cl = train_y2, k)
  contingency_table2 <- table(Predicted = KNNpred2, Actual = test_y2)

  contingency_matrix2 <- as.matrix(contingency_table2)
  accuracy_region_subset2[k-1] <- sum(diag(contingency_matrix1)) / length(test_y1)
}

average_accuracy_region_subset2 <- mean(accuracy_region_subset2)
print(paste("Average Accuracy of kNN Model 2:", average_accuracy_region_subset2))

# Model 1 is better as it has an average accuracy of 0.66, where as model 2 
# has an average accuracy of 0.5.



#### 4. Fit a k-means model for a subset of 5 variables for 2 different groups of regions (3 each) #### 

region_subset1 <- subset(epi, region %in% c("Global West", "Asia-Pacific", "Eastern Europe"))
region_subset1_data <- region_subset1[, c("ECO", "BDH", "TBN", "TKP", "AGR")]
kmeans_subset1 <- kmeans(region_subset1_data, centers = 3)

region_subset2 <- subset(epi, region %in% c("Greater Middle East", "Latin America & Caribbean", "Sub-Saharan Africa"))
region_subset2_data <- region_subset2[, c("ECO", "BDH", "TBN", "TKP", "AGR")]
kmeans_subset2 <- kmeans(region_subset2_data, centers = 3)

# 4.1. Compare the performance of the models using their within cluster sum of squares.

assigned_clusters_subset1 <- as.factor(kmeans_subset1$cluster)
ggplot(region_subset1, aes(x = ECO, y = BDH, colour = assigned_clusters_subset1)) +
  geom_point() +
  labs(title = "K-Means Clustering for Subset 1 (Global West, Asia-Pacific, Eastern Europe)", x = "ECO", y = "BDH")

assigned_clusters_subset2 <- as.factor(kmeans_subset2$cluster)
ggplot(region_subset2, aes(x = ECO, y = BDH, colour = assigned_clusters_subset2)) +
  geom_point() +
  labs(title = "K-Means Clustering for Subset 2 (Greater Middle East, Latin America & Caribbean, Sub-Saharan Africa", x = "ECO", y = "BDH")

# 4.2. In a loop fit kmeans models for both subsets using multiple values of k. 
# Plot WCSS across k values. In 1-2 sentences explain which model is better and 
# why you think that is the case.

wcss_subset1 <- c()
wcss_subset2 <- c()
ks <- seq(2, 10, 1)

for (k in ks) {
  kmeans_subset1 <- kmeans(region_subset1_data, centers = k)
  wcss_subset1 <- c(wcss_subset1, kmeans_subset1$tot.withinss)
  
  kmeans_subset2 <- kmeans(region_subset2_data, centers = k)
  wcss_subset2 <- c(wcss_subset2, kmeans_subset2$tot.withinss)
}

plot(ks, wcss_subset1, type = "b", col = "blue", ylim = c(min(c(wcss_subset1, wcss_subset2)), max(c(wcss_subset1, wcss_subset2))),
     xlab = "Number of Clusters (k)", ylab = "WCSS", main = "WCSS vs k for subset 1 and subset 2")
lines(ks, wcss_subset2, type = "b", col = "red")
legend("topright", legend = c("subset 1", "subset 2"), col = c("blue", "red"), lty = 1)


# Since subset 1 has a lower WCSS, meaning that clusters in subset 1 less 
# variation and are more compact, it is better than subset 2.

