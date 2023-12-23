
library(readr)
library(kknn)
library(caret)

getwd()

life_data <- read_csv("expect.csv") 

names(life_data) <- make.names(names(life_data), unique = TRUE)

sample_size <- floor(0.8 * nrow(life_data))
set.seed(123) 
train_indices <- sample(seq_len(nrow(life_data)), size = sample_size)
train_set <- life_data[train_indices, ]
test_set <- life_data[-train_indices, ]


train_label <- train_set$TotalLifeExpectancy
train_features <- train_set[, !names(train_set) %in% c('State', 'TotalLifeExpectancy')]

test_label <- test_set$TotalLifeExpectancy
test_features <- test_set[, !names(test_set) %in% c('State', 'TotalLifeExpectancy')]


train_features_scaled <- as.data.frame(scale(train_features))
test_features_scaled <- as.data.frame(scale(test_features))


train_label <- as.numeric(train_label)
test_label <- as.numeric(test_label)

# Defining and training the k-NN model
k <- 5 
knn_model <- kknn(train_label ~ ., train = train_features_scaled, test = test_features_scaled, k = k)
predicted_labels <- predict(knn_model)

# Evaluating the model
MAE <- mean(abs(predicted_labels - test_label), na.rm = TRUE)

# evaluation metrics
print(paste("Mean Absolute Error:", MAE))

