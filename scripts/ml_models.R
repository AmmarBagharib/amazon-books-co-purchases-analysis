library(here)
library(dplyr)
setwd(here())
########################################################################
########################################################################
# LOAD train full data
train_0302 <- read.csv(here('outputs/baseline_and_network_metrics_0302.csv'))
########################################################################
# MODEL 1
# baseline features:
colnames(train_0302)[3:10]
# [1] "connected"        "num_common_genre"
# [3] "from_salesrank"   "from_rating"     
# [5] "from_reviews"     "to_salesrank"    
# [7] "to_rating"        "to_reviews" 

train_0302_model1 <- train_0302[colnames(train_0302)[3:10]]

# Model 1: Using only the baseline features
model1 <- glm(connected ~ ., data = train_0302_model1, family = "binomial")

saveRDS(model1, file = here("outputs/ml_models/logistic_model1_0302.rds"))

colnames(train_0302)[c(3, c(11:23))]
# [1] "connected"               
# [2] "from_degree"             
# [3] "from_closeness"          
# [4] "from_betweenness"        
# [5] "from_transitivity"       
# [6] "from_eigenvec_centrality"
# [7] "pagerank.x"              
# [8] "to_degree"               
# [9] "to_closeness"            
# [10] "to_betweenness"          
# [11] "to_transitivity"         
# [12] "to_eigenvec_centrality"  
# [13] "pagerank.y"              
# [14] "same_community"   

# MODEL 2
# only network features:
train_0302_model2 <- train_0302[colnames(train_0302)[c(3, c(11:23))]]

model2 <- glm(connected ~ ., data = train_0302_model2, family = "binomial")

saveRDS(model2, file = here("outputs/ml_models/logistic_model2_0302.rds"))

# Model 3: Using all features
model3 <- glm(connected ~ ., data = train_0302[3:23], family = "binomial")

saveRDS(model3, file = here("outputs/ml_models/logistic_model3_0302.rds"))


########################################################################
# LOAD 0505 train full data
train_0505 <- read.csv(here('outputs/baseline_and_network_metrics_0505.csv'))
########################################################################
# using 0505 network metrics
# MODEL 1
# baseline features:
colnames(train_0505)[3:10]
# [1] "connected"        "num_common_genre"
# [3] "from_salesrank"   "from_rating"     
# [5] "from_reviews"     "to_salesrank"    
# [7] "to_rating"        "to_reviews" 

train_0505_model1 <- train_0505[colnames(train_0505)[3:10]]

# Model 1: Using only the baseline features
model1_0505 <- glm(connected ~ ., data = train_0505_model1, family = "binomial")

saveRDS(model1_0505, file = here("outputs/ml_models/logistic_model1_0505.rds"))

colnames(train_0505)[c(3, c(11:23))]
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
train_0505_model2 <- train_0505[colnames(train_0505)[c(3, c(11:23))]]

model2_0505 <- glm(connected ~ ., data = train_0505_model2, family = "binomial")

saveRDS(model2_0505, file = here("outputs/ml_models/logistic_model2_0505.rds"))

# Model 3: Using all features
model3_0505 <- glm(connected ~ ., data = train_0302[3:23], family = "binomial")

saveRDS(model3_0505, file = here("outputs/ml_models/logistic_model3_0505.rds"))

