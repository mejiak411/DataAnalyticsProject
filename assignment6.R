#Data Analytics
#Assignment 6
#Kaily Mejia

library(DescTools)
library(ggplot2)
library(nortest)
library(stats)
library(dplyr)
library(readxl)
library(stringr)
library(gbm)
library(foreign)
library(rpart)
library(class)
library(caret)
library(corrplot)
library(cluster)
library(TSA)

namedata <- read.csv("C:/Users/Kaily Mejia/Dropbox/Graduate Courses/Data Analytics/Project/Datasets/Final Datasets/merged_name_combined_data.csv")

#fixing formatting
namedata$last_week <- as.numeric(namedata$last_week) #formatting as a number
namedata$last_week[is.na(namedata$last_week)] <- -1 #fixing NAs

namedata$chart_week.x <-as.Date(namedatat$chart_week.x, '%m/%d/%Y') #formatting as a date
namedata$chart_week.y <-as.Date(namedatat$chart_week.y, '%m/%d/%Y') #formatting as a date

#summary statistics - not very insightful for this data analysis
summary(namedata$current_week)
summary(namedata$last_week)
summary(namedata$peak_pos)
summary(namedata$wks_on_chart)
summary(namedata$rank)
summary(namedata$peak_rank)
summary(namedata$previous_rank)
summary(namedata$weeks_on_chart)
summary(namedata$streams)

#namedata test-train set
set.seed(123)
ndtrain_index <- sample(1:nrow(namedata), nrow(namedata)*0.8)
ndtrain_set <- namedata[ndtrain_index, ]
ndtest_set <- namedata[-ndtrain_index, ]

#namedata linear regression model
ndlinreg <- lm(namedata$current_week ~ namedata$rank, data = ndtrain_set)
ndlinreg
plot(ndlinreg)

#linear regression predictions
ndlinregpredict <- predict(ndlinreg, newdata = ndtest_set)

#error calculations
ndlinregMAE <- mean(abs(ndlinregpredict - ndtest_set$current_week))
ndlinregMAE # MAE = 54.53782

ndlinregRSMSE <- sqrt(mean((ndlinregpredict - ndtest_set$current_week)^2))
ndlinregRSMSE # RSME = 66.57448

#namedata correlation model (multiple linear regression based on correlation)
namedata_numeric <- select(namedata, current_week, last_week,peak_pos, wks_on_chart, rank, peak_rank, previous_rank, weeks_on_chart, streams) #only chosing numeric values for the correlation matrix

ndcormatrix <- cor(namedata_numeric, use = "pairwise.complete.obs")
ndcormatrix 
corrplot(ndcormatrix)

ndcordf <- as.data.frame(ndcormatrix)
features <- names(ndcordf)[(ndcordf['current_week']) > 0.5] #positively correlated features
features

ndcormod <- lm(ndtrain_set$current_week ~ ., data = ndtrain_set[ ,features])
ndcormod
plot(ndcormod)
ndcorpred <- predict(ndcormod, ndtest_set[ ,features])

#error calculations
ndcorMSE <- mean(abs(ndcorpred - ndtest_set$current_week))
ndcorMSE # MSE = 17.53078 

ndcorRSME <- sqrt(mean((ndcorpred - ndtest_set$current_week)^2))
ndcorRSME # RSME = 25.05107
 
#knn clustering model
b <- namedata$current_week #billboard rank
s <- namedata$rank #spotify rank

standardb <- scale(b)
standards <- scale(s)
set.seed(456)
bsdf <- data.frame(standardb, standards)
k <- 20 #picked an arbitrary k
clusters <- kmeans(bsdf, k)
namedatac <-namedata
namedatac$cluster <- clusters$cluster

ggplot(namedatac, aes(x = s, y = b)) +
  geom_point(color = namedatac$cluster) +
  labs(title = "Clusters of Music Rankings", x = "Spotify Rank", y = "Billboard Rank")

#accuracy calculations
evaluate_knn <- function(k_value) {
  # Train KNN model
  knn_model <- knn(train = matrix(ndtrain_set$rank), test = matrix(ndtest_set$rank),
                   cl = ndtrain_set$current_week, k = k_value)
  # Evaluate accuracy
  accuracy <- sum(knn_model == ndtest_set$current_week) / length(ndtest_set$current_week)
  return(accuracy)
}

k_values <- seq(1, 20, by = 1)
set.seed(789)
accuracy_values <- sapply(k_values, evaluate_knn) #accuracy varied, seed set at the highest accuracy 
accuracy_values
plot(k_values, accuracy_values, type = "b", pch = 19, col = "#009966",
     xlab = "k (Number of Neighbors)", ylab = "Accuracy",
     main = "KNN Model Evaluation")

#time - series model
namedatat <- namedata

timecor <- ccf(namedatat$rank, namedatat$current_week, lag.max = 27)
plot(timecor, main = "Cross-Correlation between Billboard and Spotify Rankings", xlab = "Lag", ylab = 'Correlation')
abline(h = 0, col = "red")

#significance testing
ttest <- t.test(timecor$acf, alternative = "two.sided")
ttest #p- value = 1.766e-06, null hypothesis supported
