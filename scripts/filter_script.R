########################################################################
#This script outputs the following file:
# 1. a csv file containing the filtered graph, with all the baseline features
# baseline features include only book related features

library(igraph)
library(data.table)
library(here)
library(dplyr)
library(caret)
library(pROC)
setwd(here())


########################################################################
# SPECIFY THE FILE NAME HERE
graph_name <- "0302"
num_months <- 31 #number of months between date of co purchases graph and 7th August 2006
# THEN RUN THE ENTIRE SCRIPT
########################################################################

########################################################################
########################################################################
# LOAD FILES

# date collected for book_data was in Summer 2006. We take the median date which is 7th August 2006
book_data <- data.table(distinct(read.csv("../data/item_index_books.csv")))

# Split the 'cleaned_genres' column by '|' and extract the first genre
book_data <- book_data[, first_genre := sub("\\|.*", "", cleaned_genres)][, c("id", "cleaned_genres", "salesrank", "rating", "reviews")]

# filter for books with average of > 1 review per month

# load file
txt_file_name <- paste0("../data/Amazon", graph_name, ".txt")
ls <- read.table(txt_file_name)



# create graph object, and remove multiple loops
g <- simplify(graph_from_data_frame(ls, directed = FALSE))
g_df <- as_long_data_frame(g)[, c("from_name", "to_name")] #create dataframe of graph

# Export the igraph object to a GraphML file
#graph_filename <- paste0("outputs/filtered_", graph_name, "_graph.graphml")
#write_graph(g, here(graph_filename), format = "graphml")
########################################################################
########################################################################
# FILTERING GRAPH

# Extract unique vertex names
vertex_names <- V(g)

num_months <- num_months * 0.8

# filtered book_data
book_filtered <- book_data %>% 
  filter(id %in% vertex_names,
         !is.na(cleaned_genres),
         cleaned_genres != "",
         (reviews/(num_months)) > 1   # we filter for books which on average, has > 1 review per 1.25 months
         )

g_df$from_name <- as.integer(g_df$from_name)
g_df$to_name <- as.integer(g_df$to_name)

g_df_filtered <- inner_join(g_df, book_filtered[, c("id")], by = c("from_name" = "id")) %>%
  # inner join once more for valid "to" books
  inner_join(., book_filtered[, c("id")], by = c("to_name" = "id"))

dim(g_df_filtered)

########################################################################
########################################################################
# PRODUCING NEGATIVE COMBINATIONS

# Check combinations of links
g_filtered <- graph_from_data_frame(g_df_filtered)

g_df_filtered$connected <- 1

# Extract the set of nodes from your original graph
sample_nodes <- V(g_filtered) %>% as_ids()

# Generate pairs of nodes that are not connected
non_connected_pairs <- expand.grid(from = sample_nodes, to = sample_nodes) %>%
  filter(!are.connected(g_filtered, from, to) & from != to)

# Sample a subset to match the size of the original g_df_filtered dataset
set.seed(123)  # Set seed for reproducibility
non_connected_pairs_subset <- non_connected_pairs[sample(nrow(non_connected_pairs), nrow(g_df_filtered)), ]

# Create instances with connected == 0
non_connected_data <- data.frame(
  from_name = non_connected_pairs_subset$from,
  to_name = non_connected_pairs_subset$to,
  connected = 0
)

# Combine with your original data
g_final_df <- rbind(g_df_filtered, non_connected_data) %>% 
  mutate(from_name = as.numeric(from_name), to_name = as.numeric(to_name))

########################################################################
########################################################################
# APPEND BASELINE FEATURES

# aggregating_number_common_genres
# First, split the genres for each unique node
node_genres <- book_filtered %>%
  select(id, cleaned_genres) %>%
  distinct() %>%
  mutate(genres_list = strsplit(cleaned_genres, "\\|"))

# Create a named list for fast lookup
genre_list <- setNames(node_genres$genres_list, node_genres$id)

# Compute the number of common genres for each row in edgelist
g_final_df$num_common_genre <- mapply(function(v1, v2) {
  length(intersect(
    genre_list[[as.character(v1)]], 
    genre_list[[as.character(v2)]]))
}, g_final_df$from, g_final_df$from)

# Left join 'book_attributes' onto 'g_df' based on "from/to" and "id"
g_final_df <- g_final_df %>% left_join(book_filtered[,-c("cleaned_genres")], by = c("from_name"="id")) %>%
  rename(
    "from_salesrank"="salesrank",
    "from_rating"="rating",
    "from_reviews"="reviews"
  ) %>%
  left_join(book_filtered[,-c("cleaned_genres")], by=c("to_name"="id")) %>%
  rename(
    "to_salesrank"="salesrank",
    "to_rating"="rating",
    "to_reviews"="reviews"
  )

#save csv
g_final_df_filename <- paste0("outputs/baseline_features_", graph_name, ".csv")
write.csv(g_final_df, here(g_final_df_filename), row.names = FALSE)
