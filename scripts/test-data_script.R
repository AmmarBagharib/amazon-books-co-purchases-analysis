########################################################################
#This script outputs the following file:
# 1. a csv file containing the filtered test data, with all the baseline features
# baseline features include only book related features
library(data.table)
library(here)
library(dplyr)
setwd(here())


base_0302 <- read.csv(here('outputs/baseline_features_0302.csv'))
base_0505 <- read.csv(here('outputs/baseline_features_0505.csv'))
base_0601 <- read.csv(here('outputs/baseline_features_0601.csv'))

# filter out connections not present in either base_0302 or base_0505 (union of connections not in both)
# we are looking for the newly formed links
filtered_base_0601 <- base_0601 %>%
  anti_join(base_0505, by = c("from_name", "to_name")) %>%
  anti_join(base_0302, by = c("from_name", "to_name"))

# extract all nodes of newly formed links in 0601
new_nodes <- union(filtered_base_0601$from_name, filtered_base_0601$to_name)

########################################################################
# find common nodes between new nodes and 0505 graph
# LOAD 0505 graph
txt_file_name_0505 <- paste0("../data/Amazon", '0505', ".txt")
ls_0505 <- read.table(txt_file_name)

# create graph object, and remove multiple loops
g_0505 <- simplify(graph_from_data_frame(ls, directed = FALSE))

# Find nodes with degree greater than 1
nodes_to_keep <- V(g_0505)[degree(g_0505) > 1]

g_0505 <- subgraph(g_0505, nodes_to_keep)

common_nodes <- intersect(new_nodes, V(g_0505)$name)

########################################################################
# find common nodes between new nodes and 0301 graph
txt_file_name_0301 <- paste0("../data/Amazon", '0301', ".txt")
ls_0301 <- read.table(txt_file_name)

# create graph object, and remove multiple loops
g_0301 <- simplify(graph_from_data_frame(ls, directed = FALSE))

# Find nodes with degree greater than 1
nodes_to_keep <- V(g_0301)[degree(g_0301) > 1]

g_0301 <- subgraph(g_0301, nodes_to_keep)

common_nodes <- intersect(common_nodes, V(g_0301)$name)

# Number of nodes to sample from graph
total_sample_size <- 100000

# Number of nodes from g_filtered to include in the sample
num_filtered_common_nodes <- length(common_nodes)

# Number of remaining nodes to sample from g
remaining_sample_size <- total_sample_size - num_filtered_nodes

########################################################################
# appending network metrics to 0602 new links
########################################################################

########################################################################
# 0302 network metrics
# we use indices to create the subgraph
# Convert node names to indices
common_node_indices_0301 <- match(common_nodes, V(g_0301)$name)

set.seed(42)

remaining_indices_0301 <- sample(setdiff(1:length(V(g_0301)), common_node_indices_0301), size=remaining_sample_size)

# Combine nodes from nodes and remaining_sampled_nodes
combined_nodes_0301 <- c(common_node_indices_0301, remaining_indices_0301)

# Filter graph to include only nodes present in the nodes vector
g_filtered_0301<- subgraph(g_0301, combined_nodes_0301)

# Check if all nodes are present in the filtered graph
all_nodes_present_0301 <- all(common_nodes %in% V(g_filtered_0301)$name)

if (!all_nodes_present_0301) {
  # Print the names that are missing in g_filtered
  missing_nodes_0301 <- setdiff(common_nodes, intersect(common_nodes, V(g_filtered_0301)$name))
  cat("Nodes missing in g_filtered_0301.:", missing_nodes_0301, "\n")
} else {
  cat("All nodes are present in g_filtered_0301.\n")
}

# appending 0301 network metrics on nodes present in newly formed links in 0601
df1=data.frame(vertex_name=as.integer(V(g_filtered_0301)$name),
              degree=degree(g_filtered_0301),
              closeness=closeness(g_filtered_0301),
              betweenness=betweenness(g_filtered_0301, normalized=T),
              transitivity=transitivity(g_filtered_0301, type="local"),
              eigenvec_centrality=eigen_centrality(g_filtered_0301)$vector,
              community_membership=membership(cluster_louvain(g_filtered_0301))
)

final_df_0301 <- left_join(filtered_base_0601, df1, by=c("from_name"="vertex_name")) %>%
  rename('from_degree'='degree',
         'from_closeness'='closeness',
         'from_betweenness'='betweenness',
         'from_transitivity'='transitivity',
         'from_eigenvec_centrality'='eigenvec_centrality',
         'from_community_membership'='community_membership') %>%
  
  left_join(df1, by=c("to_name"="vertex_name")) %>%
  rename('to_degree'='degree',
         'to_closeness'='closeness',
         'to_betweenness'='betweenness',
         'to_transitivity'='transitivity',
         'to_eigenvec_centrality'='eigenvec_centrality',
         'to_community_membership'='community_membership') %>%
  
  mutate(same_community = as.integer(from_community_membership==to_community_membership)) %>%
  select(-c('from_community_membership', 'to_community_membership'))

file_path_0301 <- paste0('outputs/baseline_0601_and_0302_network_metrics.csv')
write.csv(final_df_0301, here(file_path_0301), row.names = FALSE)


########################################################################
# 0505 network metrics
# we use indices to create the subgraph
# Convert node names to indices
common_node_indices_0505 <- match(common_nodes, V(g_0505)$name)

set.seed(42)

remaining_indices_0505 <- sample(setdiff(1:length(V(g_0505)), common_node_indices_0505), size=remaining_sample_size)

# Combine nodes from nodes and remaining_sampled_nodes
combined_nodes_0505 <- c(common_node_indices_0505, remaining_indices_0505)

# Filter graph to include only nodes present in the nodes vector
g_filtered_0505<- subgraph(g_0505, combined_nodes_0505)

# Check if all nodes are present in the filtered graph
all_nodes_present_0505 <- all(common_nodes %in% V(g_filtered_0505)$name)

if (!all_nodes_present_0505) {
  # Print the names that are missing in g_filtered
  missing_nodes_0505 <- setdiff(common_nodes, intersect(common_nodes, V(g_filtered_0301)$name))
  cat("Nodes missing in g_filtered_0505:", missing_nodes_0505, "\n")
} else {
  cat("All nodes are present in g_filtered_0505\n")
}

# appending 0505 network metrics on nodes present in newly formed links in 0601
df2=data.frame(vertex_name=as.integer(V(g_filtered_0505)$name),
              degree=degree(g_filtered_0505),
              closeness=closeness(g_filtered_0505),
              betweenness=betweenness(g_filtered_0505, normalized=T),
              transitivity=transitivity(g_filtered_0505, type="local"),
              eigenvec_centrality=eigen_centrality(g_filtered_0505)$vector,
              community_membership=membership(cluster_louvain(g_filtered_0505))
)

final_df_0505 <- left_join(filtered_base_0601, df2, by=c("from_name"="vertex_name")) %>%
  rename('from_degree'='degree',
         'from_closeness'='closeness',
         'from_betweenness'='betweenness',
         'from_transitivity'='transitivity',
         'from_eigenvec_centrality'='eigenvec_centrality',
         'from_community_membership'='community_membership') %>%
  
  left_join(df1, by=c("to_name"="vertex_name")) %>%
  rename('to_degree'='degree',
         'to_closeness'='closeness',
         'to_betweenness'='betweenness',
         'to_transitivity'='transitivity',
         'to_eigenvec_centrality'='eigenvec_centrality',
         'to_community_membership'='community_membership') %>%
  
  mutate(same_community = as.integer(from_community_membership==to_community_membership)) %>%
  select(-c('from_community_membership', 'to_community_membership'))

file_path_0505 <- paste0('outputs/baseline_0601_and_0505_network_metrics.csv')
write.csv(final_df_0505, here(file_path_0505), row.names = FALSE)






