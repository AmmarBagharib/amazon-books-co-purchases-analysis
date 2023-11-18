########################################################################
#This script outputs 2 files:
# 1. a csv file containing the newly formed links in 0602, with network metrics of 0302 graph appended
# 2. a csv file containing the newly formed links in 0602, with network metrics of 0505 graph appended
# baseline features include only book related features
library(data.table)
library(here)
library(dplyr)
library(igraph)
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
length(new_nodes)
########################################################################
# find common nodes between new nodes and 0505 graph
# LOAD 0505 graph
txt_file_name_0505 <- paste0("../data/Amazon", '0505', ".txt")
ls_0505 <- read.table(txt_file_name_0505)

# create graph object, and remove multiple loops
g_0505 <- simplify(graph_from_data_frame(ls_0505, directed = FALSE))

# Find nodes with degree greater than 1
nodes_to_keep_0505 <- V(g_0505)[degree(g_0505) > 1]

g_0505 <- subgraph(g_0505, nodes_to_keep_0505)

########################################################################
# find common nodes between new nodes and 0302 graph
txt_file_name_0302 <- paste0("../data/Amazon", '0302', ".txt")
ls_0302 <- read.table(txt_file_name_0302)

# create graph object, and remove multiple loops
g_0302 <- simplify(graph_from_data_frame(ls_0302, directed = FALSE))

# Find nodes with degree greater than 1
nodes_to_keep_0302 <- V(g_0302)[degree(g_0302) > 1]

g_0302 <- subgraph(g_0302, nodes_to_keep_0302)

# keep only nodes that were present in g_0302 and g_0505
common_nodes <- intersect(new_nodes, intersect(V(g_0505)$name, V(g_0302)$name))
length(common_nodes)

# Number of nodes to sample from graph
total_sample_size <- 100000

# Number of nodes from g_filtered to include in the sample
num_filtered_common_nodes <- length(common_nodes)

# Number of remaining nodes to sample from g
remaining_sample_size <- total_sample_size - num_filtered_common_nodes

########################################################################
# appending network metrics to 0602 new links
########################################################################

########################################################################
# 0302 network metrics
# we use indices to create the subgraph
# Convert node names to indices
common_node_indices_0302 <- match(common_nodes, V(g_0302)$name)

set.seed(42)

remaining_indices_0302 <- sample(setdiff(1:length(V(g_0302)), common_node_indices_0302), size=remaining_sample_size)

# Combine nodes from nodes and remaining_sampled_nodes
combined_nodes_indices_0302 <- c(common_node_indices_0302, remaining_indices_0302)


# Filter graph to include only nodes present in the nodes vector
g_filtered_0302<- subgraph(g_0302, combined_nodes_indices_0302)

# Check if all nodes are present in the filtered graph
all_nodes_present_0302 <- all(common_nodes %in% V(g_filtered_0302)$name)

if (!all_nodes_present_0302) {
  # Print the names that are missing in g_filtered
  missing_nodes_0302 <- setdiff(common_nodes, intersect(common_nodes, V(g_filtered_0302)$name))
  cat("Nodes missing in g_filtered_0302.:", missing_nodes_0302, "\n")
} else {
  cat("All nodes from new links are present in g_filtered_0302.\n")
}

start_time <- Sys.time()

# appending 0302 network metrics on nodes present in newly formed links in 0601
df1=data.frame(vertex_name=as.integer(V(g_filtered_0302)$name),
              degree=degree(g_filtered_0302),
              closeness=closeness(g_filtered_0302),
              betweenness=betweenness(g_filtered_0302, normalized=T),
              transitivity=transitivity(g_filtered_0302, type="local"),
              eigenvec_centrality=eigen_centrality(g_filtered_0302)$vector,
              pagerank=page_rank(g_filtered_0302)$vector,
              community_membership=membership(cluster_louvain(g_filtered_0302))
)

# Record the end time
end_time <- Sys.time()

# Calculate and print the runtime
runtime <- end_time - start_time
print(paste("Elapsed time:", runtime))

length(setdiff(new_nodes, common_nodes)) 
# this signifies that we have to filter out for nodes only present in 0302 and 0505 within filtered_base_0601

final_df_0302 <- left_join(filtered_base_0601, df1, by=c("from_name"="vertex_name")) %>%
  rename('from_degree'='degree',
         'from_closeness'='closeness',
         'from_betweenness'='betweenness',
         'from_transitivity'='transitivity',
         'from_eigenvec_centrality'='eigenvec_centrality',
         "from_pagerank"="pagerank",
         'from_community_membership'='community_membership') %>%
  filter(!is.na(from_degree)) %>% # remove rows where the nodes from `new_nodes` are not present in `from_name`
  
  left_join(df1, by=c("to_name"="vertex_name")) %>%
  rename('to_degree'='degree',
         'to_closeness'='closeness',
         'to_betweenness'='betweenness',
         'to_transitivity'='transitivity',
         'to_eigenvec_centrality'='eigenvec_centrality',
         "to_pagerank"="pagerank",
         'to_community_membership'='community_membership') %>%
  filter(!is.na(to_degree)) %>%
  
  mutate(same_community = as.integer(from_community_membership==to_community_membership)) %>%
  select(-c('from_community_membership', 'to_community_membership'))


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
g_filtered_0505 <- subgraph(g_0505, combined_nodes_0505)

# Check if all nodes are present in the filtered graph
all_nodes_present_0505 <- all(common_nodes %in% V(g_filtered_0505)$name)

if (!all_nodes_present_0505) {
  # Print the names that are missing in g_filtered
  missing_nodes_0505 <- setdiff(common_nodes, intersect(common_nodes, V(g_filtered_0302)$name))
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
              pagerank=page_rank(g_filtered_0505)$vector,
              community_membership=membership(cluster_louvain(g_filtered_0505))
)

final_df_0505 <- left_join(filtered_base_0601, df2, by=c("from_name"="vertex_name")) %>%
  rename('from_degree'='degree',
         'from_closeness'='closeness',
         'from_betweenness'='betweenness',
         'from_transitivity'='transitivity',
         'from_eigenvec_centrality'='eigenvec_centrality',
         "from_pagerank"="pagerank",
         'from_community_membership'='community_membership'
         ) %>%
  filter(!is.na(from_degree)) %>% # remove rows where the nodes from `new_nodes` are not present in `from_name`
  
  left_join(df1, by=c("to_name"="vertex_name")) %>%
  rename('to_degree'='degree',
         'to_closeness'='closeness',
         'to_betweenness'='betweenness',
         'to_transitivity'='transitivity',
         'to_eigenvec_centrality'='eigenvec_centrality',
         "to_pagerank"="pagerank",
         'to_community_membership'='community_membership'
         ) %>%
  filter(!is.na(to_degree)) %>%

  mutate(same_community = as.integer(from_community_membership==to_community_membership)) %>%
  select(-c('from_community_membership', 'to_community_membership'))

########################################################################
# check output dfs will have same number of nodes

x <- union(final_df_0302$from_name, final_df_0302$to_name)
y <- union(final_df_0505$from_name, final_df_0505$to_name)
length(x) == length(y)
# [1] FALSE

# since false, conduct inner join to further ensure same number of nodes
# store final links
final_links <- inner_join(final_df_0302, final_df_0505, by=c("from_name", "to_name")) %>% 
  select(from_name, to_name)

# filter 0302
final_df_0302_ <- inner_join(final_df_0302, final_links, by=c("from_name", "to_name"))

# filter 0505
final_df_0505_ <- inner_join(final_df_0505, final_links, by=c("from_name", "to_name"))

# ensure links are identical
identical(final_df_0302_[c("from_name", "to_name")], final_df_0505_[c("from_name", "to_name")])
# [1] TRUE

# ensure they're not the same dataframes
identical(final_df_0302_, final_df_0505_)
# [1] FALSE
if (identical(final_df_0302_[c("from_name", "to_name")], final_df_0505_[c("from_name", "to_name")])){
  
  file_path_0302 <- paste0('outputs/baseline_0601_and_0302_network_metrics.csv')
  write.csv(final_df_0302_, here(file_path_0302), row.names = FALSE)
  
  file_path_0505 <- paste0('outputs/baseline_0601_and_0505_network_metrics.csv')
  write.csv(final_df_0505_, here(file_path_0505), row.names = FALSE)
} else {
  print("links are not identical")
}


