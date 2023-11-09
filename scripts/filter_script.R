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
########################################################################
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
g_df <- as_long_data_frame(g)[, c("from", "to")] #create dataframe of graph

# Export the igraph object to a GraphML file
graph_filename <- paste0("outputs/filtered_", graph_name, "_graph.graphml")
#write_graph(g, here(graph_filename), format = "graphml")
########################################################################
########################################################################
# filtering graph

# Extract unique vertex names
vertex_names <- V(g)

# filtered book_data
book_filtered <- book_data %>% 
  filter(id %in% vertex_names,
         !is.na(cleaned_genres),
         cleaned_genres != "",
         (reviews/(num_months)) > 1   # we filter for books which on average, has > 1 review per 1.5 months
         )

g_df_filtered <- inner_join(g_df, book_filtered[, c("id")], by = c("from" = "id")) %>%
  # inner join once more for valid "to" books
  inner_join(., book_filtered[, c("id")], by = c("to" = "id"))

########################################################################
########################################################################
# Check combinations of links
g_filtered <- graph_from_data_frame(g_df_filtered)

# combinations
#generate combinations
sample_nodes <- V(g_filtered) %>% as_ids()
combinations <- combn(sample_nodes, 2, simplify = TRUE)

#function to check if a given pair of nodes are connected in a given graph
check_connection <- function(combi) {
  result <- are.connected(g_filtered, combi[1], combi[2])
  return(result)
}

#check if each pair of nodes are connected
is_connected <- combinations %>% apply(., 2, check_connection)

g_final_df <- data.frame(B1 = as.numeric(combinations[1,]), 
                         B2 = as.numeric(combinations[2,]), 
                         is_connected = as.numeric(is_connected))

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
}, g_final_df$B1, g_final_df$B2)

# Left join 'book_attributes' onto 'g_df' based on "B1/B2" and "id"
g_final_df <- left_join(book_filtered, by = c("B1"="id")) %>%
  rename(
    "B1_salesrank"="salesrank",
    "B1_rating"="rating",
    "B1_reviews"="reviews"
  ) %>%
  left_join(book_filtered, by=c("B2"="id")) %>%
  rename(
    "B2_salesrank"="salesrank",
    "B2_rating"="rating",
    "B2_reviews"="reviews"
  )
  # inner join once more for valid "to" books
  inner_join(., book_filtered[, c("id")], by = c("to" = "id"))

#save csv
g_final_df_filename <- paste0("outputs/baseline_features_", graph_name, ".csv")
write.csv(g_final_df, here(g_final_df_filename), row.names = FALSE)
