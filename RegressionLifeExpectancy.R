library(tidyr)
library(dplyr)

# Load the dataset
data <- read.csv("/Users/VladPavlovich/Desktop/Datafinalproject - Sheet1.csv")


long_data_race <- data %>%
  gather(key = "Race", value = "LifeExpectancy", 
         White.16., Hispanic.16., Black.16., Asian.16., American.Indianand.Alaska.Native.16.) %>%
  mutate(Race = gsub(".16.", "", Race),
         Gender = "Total") 


long_data_gender <- data %>%
  gather(key = "Gender", value = "LifeExpectancy", 
         LifeExpectancyMale, LifeExpectancyFemale) %>%
  mutate(Race = "Total")  

long_data <- bind_rows(long_data_race, long_data_gender) %>%
  filter(!is.na(LifeExpectancy))

# Convert 'State', 'Race', and 'Gender' to factors
long_data$State <- as.factor(long_data$State)
long_data$Race <- factor(long_data$Race)
long_data$Gender <- factor(long_data$Gender)


cat("Levels in State:", nlevels(long_data$State), "\n")
cat("Levels in Race:", nlevels(long_data$Race), "\n")
cat("Levels in Gender:", nlevels(long_data$Gender), "\n")


linear_model <- lm(TotalLifeExpectancy ~ State + Race + Gender, data = long_data)
summary(linear_model)


predictions <- predict(linear_model, long_data)
residuals <- long_data$TotalLifeExpectancy - predictions
rmse <- sqrt(mean(residuals^2))
r_squared <- summary(linear_model)$r.squared

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")


new_observation <- data.frame(
  State = factor("California", levels = levels(long_data$State)),
  Race = factor("Black", levels = levels(long_data$Race)),
  Gender = factor("Total", levels = levels(long_data$Gender))
)
predicted_life_expectancy <- predict(linear_model, new_observation)
print(predicted_life_expectancy)

