library(tidyverse)
library(caret)
library(ipred)

# Set working directory and load data
setwd("C:/Users/Abrar Shah/Desktop/archive (1)")
train_data <- read.csv("customer_churn_dataset-training-master.csv")
test_data <- read.csv("customer_churn_dataset-testing-master.csv")

# Preprocessing
train_data <- na.omit(train_data)

train_data$Gender <- factor(train_data$Gender,
                            levels = c("Male", "Female"),
                            labels = c(1, 2))

train_data$Subscription.Type <- factor(train_data$Subscription.Type,
                                       levels = c("Basic", "Standard", "Premium"),
                                       labels = c(1, 2, 3))

train_data$Contract.Length <- factor(train_data$Contract.Length,
                                     levels = c("Monthly", "Quarterly", "Annual"),
                                     labels = c(1, 2, 3))

test_data$Gender <- factor(test_data$Gender,
                           levels = c("Male", "Female"),
                           labels = c(1, 2))

test_data$Subscription.Type <- factor(test_data$Subscription.Type,
                                      levels = c("Basic", "Standard", "Premium"),
                                      labels = c(1, 2, 3))

test_data$Contract.Length <- factor(test_data$Contract.Length,
                                    levels = c("Monthly", "Quarterly", "Annual"),
                                    labels = c(1, 2, 3))

train_data[, -c(3, 8, 9, 12)] <- scale(train_data[, -c(3, 8, 9, 12)])
test_data[, -c(3, 8, 9, 12)] <- scale(test_data[, -c(3, 8, 9, 12)])

# Define the tuning control
ctrl <- trainControl(
  method = "repeatedcv",
  number = 7,
  repeats = 2
)

# Specify values to tune
nbagg_values <- c(50, 70, 150)
cp_values <- c(0.01, 0.05, 0.1)
minsplit_values <- c(2, 5, 10)

# Train models for each combination of nbagg, cp, and minsplit
models <- list()
for (nbagg in nbagg_values) {
  for (cp in cp_values) {
    for (minsplit in minsplit_values) {
      # Train the model
      set.seed(123)
      model <- train(
        Churn ~ .,
        data = train_data,
        method = "treebag",
        trControl = ctrl,
        nbagg = nbagg,
        cp = cp,
        minsplit = minsplit
      )
      # Store the model
      models <- append(models, list(model))
    }
  }
}

# Output the models
print(models)
