library(here)
library(dplyr)
setwd(here())
########################################################################
########################################################################

# function to scale data
scale_dataset <- function(df, dependent_var = 'connected') {
  # Identify the column indices of the independent variables
  independent_vars <- setdiff(names(df), dependent_var)
  
  # Scale the independent variables
  df[independent_vars] <- scale(df[independent_vars])
  
  # Ensure the dependent variable is a factor if it's not already
  df[[dependent_var]] <- as.factor(df[[dependent_var]])
  
  return(df)
}

# LOAD train full data
train_0302 <- read.csv(here('outputs/baseline_and_network_metrics_0302.csv'))
########################################################################

# Replacing NA values for 'transitivity' columns with the value -1.
train_0302$from_transitivity[is.na(train_0302$from_transitivity)] <- -1
train_0302$to_transitivity[is.na(train_0302$to_transitivity)] <- -1

# scaling the dataset first
scaled_train_0302 <- scale_dataset(train_0302)

# MODEL 1
# baseline features:
colnames(scaled_train_0302)[3:10]
# [1] "connected"        "num_common_genre"
# [3] "from_salesrank"   "from_rating"     
# [5] "from_reviews"     "to_salesrank"    
# [7] "to_rating"        "to_reviews" 

scaled_train_0302_model1 <- scaled_train_0302[colnames(scaled_train_0302)[3:10]]

# Model 1: Using only the baseline features
model1 <- glm(connected ~ ., data = scaled_train_0302_model1, family = "binomial")

saveRDS(model1, file = here("outputs/ml_models/scaled_logistic_model1_0302.rds"))

colnames(scaled_train_0302)[c(3, c(11:23))]
# [1] "connected"                "from_degree"             
# [3] "from_closeness"           "from_betweenness"        
# [5] "from_transitivity"        "from_eigenvec_centrality"
# [7] "from_pagerank"            "to_degree"               
# [9] "to_closeness"             "to_betweenness"          
# [11] "to_transitivity"          "to_eigenvec_centrality"  
# [13] "to_pagerank"              "same_community"  

# MODEL 2
# only network features:
scaled_train_0302_model2 <- scaled_train_0302[colnames(scaled_train_0302)[c(3, c(11:23))]]

model2 <- glm(connected ~ ., data = scaled_train_0302_model2, family = "binomial")

saveRDS(model2, file = here("outputs/ml_models/scaled_logistic_model2_0302.rds"))

# Model 3: Using all features
model3 <- glm(connected ~ ., data = scaled_train_0302[3:23], family = "binomial")

saveRDS(model3, file = here("outputs/ml_models/scaled_logistic_model3_0302.rds"))


########################################################################
# LOAD 0505 train full data
train_0505 <- read.csv(here('outputs/baseline_and_network_metrics_0505.csv'))

# Replacing NA values for 'transitivity' columns with the value -1.
train_0505$from_transitivity[is.na(train_0505$from_transitivity)] <- -1
train_0505$to_transitivity[is.na(train_0505$to_transitivity)] <- -1

# scaling the dataset
scaled_train_0505 <- scale_dataset(train_0505)
########################################################################
# using 0505 network metrics
# MODEL 1
# baseline features:
colnames(scaled_train_0505)[3:10]
# [1] "connected"        "num_common_genre"
# [3] "from_salesrank"   "from_rating"     
# [5] "from_reviews"     "to_salesrank"    
# [7] "to_rating"        "to_reviews" 

scaled_train_0505_model1 <- scaled_train_0505[colnames(scaled_train_0505)[3:10]]

# Model 1: Using only the baseline features
model1_0505 <- glm(connected ~ ., data = scaled_train_0505_model1, family = "binomial")

saveRDS(model1_0505, file = here("outputs/ml_models/scaled_logistic_model1_0505.rds"))

colnames(scaled_train_0505)[c(3, c(11:23))]
# [1] "connected"               
# [2] "from_degree"             
# [3] "from_closeness"          
# [4] "from_betweenness"        
# [5] "from_transitivity"       
# [6] "from_eigenvec_centrality"
# [7] "from_pagerank"           
# [8] "to_degree"               
# [9] "to_closeness"            
# [10] "to_betweenness"          
# [11] "to_transitivity"         
# [12] "to_eigenvec_centrality"  
# [13] "to_pagerank"             
# [14] "same_community"   

# MODEL 2
# only network features:
scaled_train_0505_model2 <- scaled_train_0505[colnames(scaled_train_0505)[c(3, c(11:23))]]

model2_0505 <- glm(connected ~ ., data = scaled_train_0505_model2, family = "binomial")

saveRDS(model2_0505, file = here("outputs/ml_models/scaled_logistic_model2_0505.rds"))

# Model 3: Using all features
model3_0505 <- glm(connected ~ ., data = scaled_train_0505[3:23], family = "binomial")

saveRDS(model3_0505, file = here("outputs/ml_models/scaled_logistic_model3_0505.rds"))

########################################################################

test_0302 <- read.csv(here('outputs/baseline_0601_and_0302_network_metrics.csv'))
scaled_test_0302 <- scale_dataset(test_0302)
write.csv(scaled_test_0302, here("outputs/scaled_test_0302.csv"), row.names=FALSE)

test_0505 <- read.csv(here('outputs/baseline_0601_and_0505_network_metrics.csv'))
scaled_test_0505 <- scale_dataset(test_0505)
write.csv(scaled_test_0505, here("outputs/scaled_test_0505.csv"), row.names=FALSE)
