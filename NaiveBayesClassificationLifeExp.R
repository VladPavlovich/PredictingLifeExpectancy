data <- read.csv("/Users/VladPavlovich/Desktop/Datafinalproject - Sheet1.csv")

feature_names <- c("White.16.", "Hispanic.16.", "Black.16.", "Asian.16.", "American.Indianand.Alaska.Native.16.", "LifeExpectancyMale", "LifeExpectancyFemale")
data$State <- as.factor(data$State)
#all features are numeric
data[, feature_names] <- lapply(data[, feature_names], as.numeric)

#Create 5 bins to store age groups because classifier can't predict exact age # but instead classes.
data$LifeExpectancyGroup <- cut(data$TotalLifeExpectancy, breaks=5, labels=1:5)

# Calculate the bin intervals for the 'TotalLifeExpectancy' to inform the user about the group age ranges later
bin_intervals <- levels(cut(data$TotalLifeExpectancy, breaks=5))

#probability tables generated
generate_prob_tables <- function(training_data, feature_names, class_name) {
  prob_tables <- list()
  for (feature in feature_names) {
    # This will calculate P(feature|class)
    prob_tables[[feature]] <- table(training_data[[feature]], training_data[[class_name]]) / rowSums(table(training_data[[feature]], training_data[[class_name]]))
  }
  return(prob_tables)
}

# 
calculate_priors <- function(training_data, class_name) {
  priors <- table(training_data[[class_name]]) / nrow(training_data)
  return(priors)
}

# Naive Bayes prediction function
naive_bayes_predict <- function(new_data, prob_tables, priors, feature_names, class_levels) {
  #  vector to store predictions
  predictions <- character(nrow(new_data))
  
  # Loop over each row in new_data to make predictions
  for (i in 1:nrow(new_data)) {
    # Extract the current observation
    observation <- new_data[i, feature_names, drop = FALSE]
    
    # posterior probability for each class
    posterior_probs <- sapply(class_levels, function(class_level) {
      prior <- priors[class_level]
      likelihoods <- sapply(feature_names, function(feature) {
        prob_table <- prob_tables[[feature]]
        feature_value <- as.character(observation[[feature]])
        # Check if feature value is in the levels of the probability table
        if (!(feature_value %in% rownames(prob_table))) {
        
          # Laplace smoothing if the feature value was not seen in the training set
          # This handles the problem of zero probability as well.
          return(1 / (sum(prob_table[, class_level]) + length(prob_table[, class_level])))
          
        } 
        
    else {
          return(prob_table[feature_value, class_level])
        }
      })
      prior * prod(likelihoods)
    })
    
   #The predicted class will have the highest posterior probability
    predictions[i] <- as.character(class_levels[which.max(posterior_probs)])
  }
  
  return(predictions)
}

# Leave-One-Out Cross-Validation calculate accuracy
accuracy <- sum(sapply(1:nrow(data), function(i) {
  # uses one row as the test set and the rest as the training set because data set is not large
  test_set <- data[i, , drop = FALSE]
  training_set <- data[-i, , drop = FALSE]
  
  # generates probability tables and priors using the training set
  prob_tables <- generate_prob_tables(training_set, feature_names, "LifeExpectancyGroup")
  priors <- calculate_priors(training_set, "LifeExpectancyGroup")
  
  # predicts the life expectancy group for instance test
  predicted_group <- naive_bayes_predict(test_set, prob_tables, priors, feature_names, levels(training_set$LifeExpectancyGroup))
  
  # this will return 1 if prediction is correct 0 if not
  as.integer(predicted_group == test_set$LifeExpectancyGroup)
})) / nrow(data)

print(paste("LOOCV Accuracy: ", accuracy))





#User created Objects
new_individual_state <- "California"
new_individual_race <- "Black.16."  # Use the correct column name for Black individuals
new_individual_gender <- "LifeExpectancyFemale"  # Use the correct column name for Female life expectancy


# using neutral values for all values that aren't predicted so the classifier now what characteristics to predict
neutral_value <- mean(data$TotalLifeExpectancy, na.rm = TRUE)  
new_individual <- data.frame(
  State = factor(new_individual_state, levels = levels(data$State)),
  White.16. = neutral_value,
  Hispanic.16. = neutral_value,
  Black.16. = neutral_value,  
  Asian.16. = neutral_value,
  American.Indianand.Alaska.Native.16. = neutral_value,
  LifeExpectancyMale = neutral_value,
  LifeExpectancyFemale = neutral_value  
)

new_individual$Black.16. <- median(data[data$State == new_individual_state, "Black.16."], na.rm = TRUE)
new_individual$LifeExpectancyFemale <- median(data[data$State == new_individual_state, "LifeExpectancyFemale"], na.rm = TRUE)

new_individual$Black.16. <- cut(new_individual$Black.16., breaks = 5, labels = 1:5)
new_individual$LifeExpectancyFemale <- cut(new_individual$LifeExpectancyFemale, breaks = 5, labels = 1:5)

#other featers will also be factors.
new_individual[, feature_names] <- lapply(new_individual[, feature_names], as.factor)

# this will predict the life expectancy
predicted_group <- naive_bayes_predict(new_individual, prob_tables, priors, feature_names, levels(data$LifeExpectancyGroup))

predicted_group_label <- paste("Predicted Life Expectancy Group:", predicted_group)
cat(predicted_group_label, "\n")

age_range_label <- paste("Corresponding Age Range:", bin_intervals[as.integer(predicted_group)])
cat(age_range_label, "\n")
