library(igraph)
library(data.table)
library(here)
library(dplyr)
setwd(here())

########################################################################
# SPECIFY THE FILE NAME HERE
graph_name <- '0505'

# THEN RUN THE ENTIRE SCRIPT
########################################################################

# LOAD graph
txt_file_name <- paste0("../data/Amazon", graph_name, ".txt")
ls <- read.table(txt_file_name)

# create graph object, and remove multiple loops
g <- simplify(graph_from_data_frame(ls, directed = FALSE))

# Find nodes with degree greater than 1
nodes_to_keep <- V(g)[degree(g) > 1]

g <- subgraph(g, nodes_to_keep)

file_path <- paste0('outputs/baseline_features_', graph_name, '.csv')

features_df <- read.csv(here(file_path))

########################################################################

# extract all nodes
nodes <- union(features_df$from_name, features_df$to_name)

# Number of nodes to sample from graph
total_sample_size <- 100000

# Number of nodes from g_filtered to include in the sample
num_filtered_nodes <- length(nodes)

# Number of remaining nodes to sample from g
remaining_sample_size <- total_sample_size - num_filtered_nodes

########################################################################
# we use indices to create the subgraph
# Convert node names to indices
node_indices <- match(nodes, V(g)$name)

set.seed(42)

remaining_indices <- sample(setdiff(1:length(V(g)), node_indices), size=remaining_sample_size)

# Combine nodes from nodes and remaining_sampled_nodes
combined_nodes <- c(node_indices, remaining_indices)

# Filter graph to include only nodes present in the nodes vector
g_filtered <- subgraph(g, combined_nodes)

# Check if all nodes are present in the filtered graph
all_nodes_present <- all(nodes %in% V(g_filtered)$name)

if (!all_nodes_present) {
  # Print the names that are missing in g_filtered
  missing_nodes <- setdiff(nodes, intersect(nodes, V(g_filtered)$name))
  cat("Nodes missing in g_filtered:", missing_nodes, "\n")
} else {
  cat("All nodes are present in g_filtered.\n")
}

start_time <- Sys.time()

df=data.frame(vertex_name=V(g_filtered)$name,
              degree=degree(g_filtered),
              closeness=closeness(g_filtered),
              betweenness=betweenness(g_filtered, normalized=T),
              transitivity=transitivity(g_filtered, type="local"),
              eigenvec_centrality=eigen_centrality(g_filtered)$vector,
              pagerank=page_rank(g_filtered)$vector
              )

# Record the end time
end_time <- Sys.time()

# Calculate and print the runtime
runtime <- end_time - start_time
print(paste("Elapsed time:", runtime))

########################################################################
# Deciding on community algorithm
########################################################################

########################################################################
# Greedy community detection
# greedy method (hiearchical, fast method)
c1 = cluster_fast_greedy(g_filtered)

# modularity measure
modularity(c1)
# 0302:
## [1] 0.9332996

# number of communities
length(c1)
# 0302:
## [1] 12916

########################################################################
# louvain community
c2 <- cluster_louvain(g_filtered)

# modularity measure
modularity(c2)
# 0302:
# [1] 0.9534968

# number of communities
length(c2)
# 0302:
## [1] 11871

########################################################################

# assign community membership column
df$community_membership <- membership(c2)

df$vertex_name <- as.integer(df$vertex_name)

if (graph_name == "0302"){
  network_df <- df %>% filter(df$vertex_name %in% nodes)
  file_path <- paste0('outputs/network_metrics_', graph_name, ".csv")
  write.csv(network_df, here(file_path), row.names = FALSE)
}

final_df <- left_join(features_df, df, by=c("from_name"="vertex_name")) %>%
  rename('from_degree'='degree',
         'from_closeness'='closeness',
         'from_betweenness'='betweenness',
         'from_transitivity'='transitivity',
         'from_eigenvec_centrality'='eigenvec_centrality',
         'from_pagerank'='pagerank',
         'from_community_membership'='community_membership') %>%
  
  left_join(df, by=c("to_name"="vertex_name")) %>%
  rename('to_degree'='degree',
         'to_closeness'='closeness',
         'to_betweenness'='betweenness',
         'to_transitivity'='transitivity',
         'to_eigenvec_centrality'='eigenvec_centrality',
         'to_pagerank'='pagerank',
         'to_community_membership'='community_membership') %>%
  
  mutate(same_community = as.integer(from_community_membership==to_community_membership)) 

final_df <- final_df %>% select(-c('from_community_membership', 'to_community_membership'))

file_path <- paste0('outputs/baseline_and_network_metrics_', graph_name, ".csv")
write.csv(final_df, here(file_path), row.names = FALSE)


