library(dplyr)
library(here)

set.seed(123)
######################################################################
# test data using 0302 network metrics
# test data
test_0601_0302 <- read.csv(here('outputs/scaled_test_0302.csv'))

####0302
# model 1: baseline features only
model1_0302 <- readRDS(here("outputs/ml_models/scaled_logistic_model1_0302.rds"))

test_1_0302 <- test_0601_0302[colnames(test_0601_0302)[c(c(1:4), 12)]] %>% select(-connected)
######################################################################
# model 2: network metrics only
model2_0302 <- readRDS(here("outputs/ml_models/scaled_logistic_model2_0302.rds"))

test_2_0302 <- test_0601_0302[colnames(test_0601_0302)[5:12]] %>% select(-connected)
######################################################################
# model 3: baseline features + network metrics
model3_0302 <- readRDS(here("outputs/ml_models/scaled_logistic_model3_0302.rds"))

test_3_0302 <- test_0601_0302[colnames(test_0601_0302)[1:12]] %>% select(-connected)
######################################################################
# test data using 0505 network metrics
test_0601_0505 <- read.csv(here('outputs/scaled_test_0505.csv'))

# model 1: NETWORK METRICS ONLY
model1_0505 <- readRDS(here("outputs/ml_models/scaled_logistic_model1_0505.rds"))

test_1_0505 <- test_0601_0302[colnames(test_0601_0505)[1:12]] %>% select(-connected)

# helper function to calculate accuracy
calculate_accuracy <- function(model, test_data, initial_test_data) {
  probabilities <- model %>% predict(test_data, type = "response")
  predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
  accuracy <- mean(predicted_classes == initial_test_data$connected, na.rm = TRUE)
  return(accuracy)
}

model_names <- list("baseline 0302", "network metrics 0302", "baseline + network metrics 0302", "network metrics 0505")

model_list <- list(model1_0302, model2_0302, model3_0302, model1_0505)

test_list <- list(test_1_0302, test_2_0302, test_3_0302, test_1_0505)

initial_test_list <- list(test_0601_0302, test_0601_0302, test_0601_0302, test_0601_0505)


# Create an empty dataframe to store results
results_df <- data.frame(Model = character(), Accuracy = numeric(), stringsAsFactors = FALSE)

# Loop over models and tests
for (i in seq_along(model_list)) {
  model <- model_list[[i]]
  test_data <- test_list[[i]]
  model_name <- model_names[[i]]
  initial_test_data <- initial_test_list[[i]]
  
  # Calculate accuracy using the helper function
  accuracy <- calculate_accuracy(model, test_data, initial_test_data)
  
  results_row <- data.frame(Model = model_name, Accuracy = accuracy)
  # Store results in the dataframe
  results_df <- bind_rows(results_df, results_row)
}

# Print or inspect the results dataframe
write.csv(results_df, here("outputs/plots_and_tables/7_regression-results.csv"), row.names=FALSE)
######################################################################
# Extract coefficients and their names

coefficients_df <- as.data.frame(list(coef(model1_0505), coef(model2_0302)))

colnames(coefficients_df) <- c("network_0302", "network_0505")

coefficients_df$change <- ifelse(coefficients_df$network_0505 > coefficients_df$network_0302, "Increase", "Decrease")

coefficients_df <- coefficients_df[2:8, ] # drop intercept row
coefficients_df

# Print or inspect the results dataframe
write.csv(coefficients_df, here("outputs/plots_and_tables/8_regression-coefficients.csv"), row.names=FALSE)












