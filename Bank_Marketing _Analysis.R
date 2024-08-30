# importing required packages 
library(readr)
library(dplyr)
library(ggplot2)

# importing bank marketing data set
bank_data <- read.csv("C:/Users/User/Downloads/archive/bank-additional-full.csv", sep = ";")

# view the first few rows
head(bank_data)

# checking for null values
sum(is.na(bank_data))
colSums(is.na(bank_data))

# converting character columns to factors  
bank_data <- bank_data %>%
  mutate(across(where(is.character), as.factor))

# view the data
str(bank_data)

# checking duplicate values
duplicated(bank_data)
bank_data <- bank_data[!duplicated(bank_data), ]

# installing "caret" packeges for train and test splitting
install.packages("caret")
library(caret)

# splitting train and test data
set.seed(123)
trainIndex <- createDataPartition(bank_data$y, p = .7, list = FALSE)
trainData <- bank_data[trainIndex, ]
testData <- bank_data[-trainIndex, ]

# summary of the dataset
summary(bank_data)

# distribution of plot of the target variables
ggplot(bank_data, aes(x = y)) +
  geom_bar(fill = "cyan") +
  ggtitle("Distribution of Target Variable (y)") +
  xlab("Subscribed to Term Deposit (y)") +
  ylab("Count")

# visualizing relation between variables
ggplot(bank_data, aes(x = age, fill = y)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ y) +
  ggtitle("Age Distribution by Subscription Status") +
  xlab("Age") +
  ylab("Count")


# generating bar plot between job description by subscription status
ggplot(bank_data, aes(x = job, fill = y)) +
  geom_bar(position = "fill", alpha = 0.7) +
  ggtitle("Job Distribution by Subscription Status") +
  xlab("Job") +
  ylab("Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# generating line plot between "Average Call Duration" and "Previous Contact Outcome"
previous_contact_duration <- bank_data %>%
  group_by(poutcome, y) %>%
  summarize(mean_duration = mean(duration, na.rm = TRUE))

ggplot(previous_contact_duration, aes(x = poutcome, y = mean_duration, color = y, group = y)) +
  geom_line(size = 1) +
  ggtitle("Average Call Duration by Previous Contact Outcome and Subscription Status") +
  xlab("Previous Contact Outcome") +
  ylab("Average Call Duration (seconds)") +
  theme_minimal() +
  scale_x_discrete(limits = c("nonexistent", "failure", "success"))

# generating pie chart of jop distribution
job_distribution <- bank_data %>%
  count(job) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(job_distribution, aes(x = "", y = percentage, fill = job)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Pie Chart of Job Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# generating the descriptive chart
prop.table(table(bank_data$job))

# importing require packages and library for "decision tree" model
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# training the decision tree model
decision_tree_model <- rpart(y ~ ., data = trainData, method = "class")

# visualization of the decision model
rpart.plot(decision_tree_model)

# model prediction on test data
predictions <- predict(decision_tree_model, testData, type = "class")

# generating confusion matrix
confusion <- confusionMatrix(predictions, testData$y)
print(confusion)

# adjusting for cost sensitive learning matrix
cost_matrix <- matrix(c(0, 5, 1, 0), nrow = 2)

decision_tree_model_cost <- rpart(y ~ ., data = trainData, method = "class",
                                  parms = list(loss = cost_matrix))

predictions_cost <- predict(decision_tree_model_cost, testData, type = "class")
confusion_cost <- confusionMatrix(predictions_cost, testData$y)
print(confusion_cost)

# installing packages and library for "random forest" model
install.packages("randomForest")
library(randomForest)

#training the data of the random forest model
rf_model <- randomForest(y ~ ., data = trainData)

#making prediction on the test data
predictions_rf <- predict(rf_model, testData)

# Generating confusion matrix for random forest model
confusion_rf <- confusionMatrix(predictions_rf, testData$y)
print(confusion_rf)
  