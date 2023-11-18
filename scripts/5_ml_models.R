library(here)
library(dplyr)
setwd(here())
########################################################################
########################################################################

# function to scale data
scale_dataset <- function(df, dependent_var = 'connected', exclude_vars = c()) {
  # Identify the column indices of the independent variables
  independent_vars <- setdiff(names(df), c(dependent_var, exclude_vars))
  #print(independent_vars)
  
  # Scale the independent variables
  df[independent_vars] <- scale(df[independent_vars])
  
  # Ensure the dependent variable is a factor if it's not already
  df[[dependent_var]] <- as.factor(df[[dependent_var]])
  
  return(df)
}

mean_metrics <- function(df){
  df <- df %>%
    mutate(
      mean_closeness = rowMeans(select(., c("from_closeness", "to_closeness"))),
      mean_degree = rowMeans(select(., c("from_degree", "to_degree"))),
      mean_transitivity = rowMeans(select(., c("from_transitivity", "to_transitivity"))),
      mean_pagerank = rowMeans(select(., c("from_pagerank", "to_pagerank"))),
      mean_eigen = rowMeans(select(., c("from_eigenvec_centrality", "to_eigenvec_centrality"))),
      mean_betweenness = rowMeans(select(., c("from_betweenness", "to_betweenness"))),
      
      mean_salesrank = rowMeans(select(., c("from_salesrank", "to_salesrank"))),
      mean_reviews = rowMeans(select(., c("from_reviews", "to_reviews"))),
      mean_ratings = rowMeans(select(., c("from_rating", "to_rating"))),
    ) %>% 
    select(
      num_common_genre,
      mean_salesrank,
      mean_reviews,
      mean_ratings,
      
      mean_closeness,
      mean_degree,
      mean_transitivity,
      mean_pagerank,
      mean_eigen,
      mean_betweenness,
      same_community,
      
      connected
    )
  
  return(df)
}

########################################################################

# LOAD train full data
train_0302 <- read.csv(here('outputs/baseline_and_network_metrics_0302.csv'))
nrow(train_0302)
########################################################################

# Replacing NA values for 'transitivity' columns with the value -1.
train_0302$from_transitivity[is.na(train_0302$from_transitivity)] <- -1
train_0302$to_transitivity[is.na(train_0302$to_transitivity)] <- -1

scaled_train_0302 <- mean_metrics(
  scale_dataset(
    train_0302,
    exclude_vars = c()
  )
)
colnames(scaled_train_0302)
# [1] "num_common_genre" 
# [2] "mean_salesrank"   
# [3] "mean_reviews"     
# [4] "mean_ratings"     
# [5] "mean_closeness"   
# [6] "mean_degree"      
# [7] "mean_transitivity"
# [8] "mean_pagerank"    
# [9] "mean_eigen"       
# [10] "mean_betweenness" 
# [11] "same_community"   
# [12] "connected" 

scaled_train_0302_model1 <- scaled_train_0302[colnames(scaled_train_0302)[c(c(1:4), 12)]]

# Model 1: Using only the baseline features
model1 <- glm(connected ~ ., data = scaled_train_0302_model1, family = "binomial")

saveRDS(model1, file = here("outputs/ml_models/scaled_logistic_model1_0302.rds"))

# MODEL 2
# only network features:
scaled_train_0302_model2 <- scaled_train_0302[colnames(scaled_train_0302)[5:12]]

model2 <- glm(connected ~ ., data = scaled_train_0302_model2, family = "binomial")

saveRDS(model2, file = here("outputs/ml_models/scaled_logistic_model2_0302.rds"))

# Model 3: Using all features
model3 <- glm(connected ~ ., data = scaled_train_0302[1:12], family = "binomial")

saveRDS(model3, file = here("outputs/ml_models/scaled_logistic_model3_0302.rds"))


########################################################################
# LOAD 0505 train full data
train_0505 <- read.csv(here('outputs/baseline_and_network_metrics_0505.csv'))
nrow(train_0505)
# Replacing NA values for 'transitivity' columns with the value -1.
train_0505$from_transitivity[is.na(train_0505$from_transitivity)] <- -1
train_0505$to_transitivity[is.na(train_0505$to_transitivity)] <- -1

scaled_train_0505 <- mean_metrics(
  scale_dataset(
    train_0505,
    exclude_vars = c()
  )
)
########################################################################
# using 0505 network metrics
# MODEL 1
colnames(scaled_train_0505)
# [1] "num_common_genre" 
# [2] "mean_salesrank"   
# [3] "mean_reviews"     
# [4] "mean_ratings"     
# [5] "mean_closeness"   
# [6] "mean_degree"      
# [7] "mean_transitivity"
# [8] "mean_pagerank"    
# [9] "mean_eigen"       
# [10] "mean_betweenness" 
# [11] "same_community"   
# [12] "connected" 

scaled_train_0505_model1 <- scaled_train_0505[colnames(scaled_train_0505)[5:12]]

# Model 1: Using only the network metric
model1_0505 <- glm(connected ~ ., data = scaled_train_0505_model1, family = "binomial")

saveRDS(model1_0505, file = here("outputs/ml_models/scaled_logistic_model1_0505.rds"))

########################################################################
########################################################################
# TEST DATA
########################################################################

test_0302 <- read.csv(here('outputs/baseline_0601_and_0302_network_metrics.csv'))
test_0302$from_transitivity[is.na(test_0302$from_transitivity)] <- -1
test_0302$to_transitivity[is.na(test_0302$to_transitivity)] <- -1
test_0302$from_closeness[is.na(test_0302$from_closeness)] <- -1
test_0302$to_closeness[is.na(test_0302$to_closeness)] <- -1

scaled_test_0302 <- mean_metrics(
  scale_dataset(
    test_0302,
    exclude_vars = c()
  )
)
write.csv(scaled_test_0302, here("outputs/scaled_test_0302.csv"), row.names=FALSE)

########################################################################
test_0505 <- read.csv(here('outputs/baseline_0601_and_0505_network_metrics.csv'))
test_0505$from_transitivity[is.na(test_0505$mean_transitivity)] <- -1
test_0505$to_transitivity[is.na(test_0505$to_transitivity)] <- -1
test_0505$from_closeness[is.na(test_0505$from_closeness)] <- -1
test_0505$to_closeness[is.na(test_0505$to_closeness)] <- -1

scaled_test_0505 <- mean_metrics(
  scale_dataset(
    test_0505,
    exclude_vars = c()
  )
)

write.csv(scaled_test_0505, here("outputs/scaled_test_0505.csv"), row.names=FALSE)



