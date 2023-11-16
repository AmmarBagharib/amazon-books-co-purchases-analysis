library(igraph)
library(data.table)
library(here)
library(dplyr)
setwd(here())

########################################################################
# SPECIFY THE FILE NAME HERE
graph_name <- '0302'

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
nodes <- union(features_df$from, features_df$to)

# Filter graph to only include nodes present in the DataFrame
g_filtered <- induced_subgraph(g, nodes)

# induced_subgraph calculates the induced subgraph of a set of vertices in a graph. 
# This means that exactly the specified vertices and all the edges between them will be 
# kept in the result graph.

# Number of nodes to sample from graph
total_sample_size <- 100000

# Number of nodes from g_filtered to include in the sample
filtered_nodes_in_filtered_graph <- length(V(g_filtered))

# Number of remaining nodes to sample from g
remaining_sample_size <- total_sample_size - filtered_nodes_in_filtered_graph

########################################################################
# attempt 1
# Ensure the final sample size
remaining_sampled_nodes <- sample(V(g), size = remaining_sample_size, replace = FALSE)

# Combine nodes from g_filtered and the remaining sample
sampled_nodes <- c(as_ids(V(g_filtered)), as_ids(remaining_sampled_nodes))

# Check if all nodes are in sampled_nodes
all(nodes %in% sampled_nodes)

########################################################################
# attempt 2
# Get the remaining nodes not in g_filtered
remaining_nodes <- setdiff(V(g), V(g_filtered))

# Sample the remaining nodes
remaining_sampled_nodes <- dplyr::sample_n(as.data.frame(remaining_nodes), size = remaining_sample_size, replace = FALSE)

# Combine nodes from g_filtered and the remaining sample
sampled_nodes <- c(as_ids(V(g_filtered)), remaining_sampled_nodes$remaining_sampled_nodes)

# Check if all nodes are in sampled_nodes
all(nodes %in% sampled_nodes)

########################################################################


# Create a subgraph with the sampled nodes
sampled_subgraph <- subgraph(g, as_ids(sampled_nodes))

df=data.frame(vertex_name=as_ids(V(sampled_subgraph)),
              degree=degree(sampled_subgraph),
              closeness=closeness(sampled_subgraph),
              betweenness=betweenness(sampled_subgraph,normalized=T),
              transitivity=transitivity(sampled_subgraph,type="local"),
              eigenvec_centrality=eigen_centrality(sampled_subgraph)$vector
              )


########################################################################
# Deciding on community algorithm
########################################################################
set.seed(42)
########################################################################
# Greedy community detection
# greedy method (hiearchical, fast method)
c1 = cluster_fast_greedy(sampled_subgraph)

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
c2 <- cluster_louvain(sampled_subgraph)

# modularity measure
modularity(c2)
# 0302:
# [1] 0.9495689

# number of communities
length(c2)
# 0302:
## [1] 12470

########################################################################

# assign community membership column
df$community_membership <- membership(c2)

# Get the names of nodes in g
nodes_in_g_filtered <- as_ids(V(g_filtered))

df$vertex_name <- as.integer(df$vertex_name)

left_join(features_df, df, by=c("from"="vertex_name")) %>% filter(!is.na(degree)) %>% View()

