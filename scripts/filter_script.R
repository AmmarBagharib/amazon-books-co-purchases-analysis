########################################################################
#This script outputs the following files:
# 1. book_filtered_(graph).csv
# 2. 

library(igraph)
library(data.table)
library(here)
library(dplyr)
library(caret)
library(pROC)
setwd(here())


########################################################################
# SPECIFY THE FILE NAME HERE
graph_name <- "0601"

# input whether running for train or test: if train, put "FALSE", else, put "TRUE"
test <- TRUE
# THEN RUN THE ENTIRE SCRIPT
########################################################################

########################################################################
########################################################################
# Create book dataframe

# date collected for book_data was in Summer 2006. We take the median date which is 7th August 2006
book_data <- data.table(distinct(read.csv("../data/item_index_books.csv")))

# Split the 'cleaned_genres' column by '|' and extract the first genre
book_data[, first_genre := sub("\\|.*", "", cleaned_genres)]

# Get the counts of the first genre
genre_counts <- book_data[, .N, by = first_genre]

head(book_data)

# filter for books with average of > 1 review per month

# date collected for ls_0302 was in 03 March 2003

txt_file_name <- paste0("../data/Amazon", graph_name, ".txt")

ls_0302 <- read.table(txt_file_name)
g0302 <- graph_from_data_frame(ls_0302, directed = FALSE)

g_df <- as_long_data_frame(g0302) %>% select(-c("from_name", "to_name"))

book_attributes <- book_data[, c("id", "first_genre", "salesrank", "rating", "reviews")]

# Left join 'book_attributes' onto 'g_df' based on "from_name" and "id"
result_df <- inner_join(g_df, book_attributes, by = c("from" = "id")) %>%
  rename(
    "from_first_genre"="first_genre", 
    "from_salesrank"="salesrank",
    "from_rating"="rating",
    "from_reviews"="reviews"
  ) %>%
  # append book attributes to the to_books
  left_join(., book_attributes, by = c("to" = "id")) %>%
  rename(
    "to_first_genre"="first_genre", 
    "to_salesrank"="salesrank",
    "to_rating"="rating",
    "to_reviews"="reviews"
  ) %>%
  # filter out NA to_genre, as this signifies that these items are NOT books
  filter(!is.na(to_first_genre)) %>%
  
  # filter out books that do not have any genre from the metadata
  filter(from_first_genre != "", to_first_genre != "") %>%
  
  # number of months from 03 March 2003 to 7th August 2006 is 30 months and 3 weeks, we round up to 31 months
  # we now filter for the books which on average, has > 1 review per month
  # filter out books that have an average of < 1 review every month in the 31 month period
  filter((from_reviews/31) > 1, (to_reviews/31) > 1)


#result_df

# Extract unique vertex names
vertex_names <- unique(c(result_df$from, result_df$to))

# filtered book_data
book_filtered <- book_data %>% 
  filter(id %in% vertex_names) %>%
  select(id, cleaned_genres, rating, reviews, salesrank, first_genre)

# First, extract unique list of main genres from books. Main genres are defined to be the first genre for each book
main_genres <- book_filtered %>%
  select(first_genre) %>%
  distinct() %>% pull()

# Function to count common main genres
count_common_main_genres <- function(genre_string) {
  genres <- unlist(strsplit(genre_string, "\\|"))
  common_genres <- genres[genres %in% main_genres]
  return(length(common_genres))
}

# Apply the function to each row of the dataframe
book_filtered$num_main_genres <- sapply(book_filtered$cleaned_genres, count_common_main_genres)

bookfiltered_filename <- paste0("outputs/book_filtered_", graph_name, ".csv")

write.csv(book_filtered, here(bookfiltered_filename), row.names=FALSE)

# append main genres to result df
num_main_genres_df <- book_filtered[, c("id", "num_main_genres")]
result_df <- result_df %>%
  
  left_join(num_main_genres_df, by = c("from" = "id")) %>%
  rename("from_num_main_genres"="num_main_genres") %>%
  
  left_join(num_main_genres_df, by = c("to" = "id")) %>%
  rename("to_num_main_genres"="num_main_genres")

### remove multiple loops from result_df
# the following code sorts each row of result_df to ensure consistent order for undirected edges 
# and then removes duplicates, resulting in unique undirected connections. 

# Sort each row in result_df to ensure consistent order for undirected edges
result_df_sorted <- result_df
result_df_sorted[] <- t(apply(result_df_sorted, 1, sort))

# Remove duplicates
result_df_unique <- unique(result_df_sorted)

# check if duplicates done correctly:
# Create an igraph object, and remove the multiple loops
g <- simplify(graph_from_data_frame(result_df[, c("from", "to")], directed=FALSE))

if (nrow(result_df_unique) == ecount(g)){
  print("result_df_unique has filtered for multiple loops successfully")
} else {
  print("result_df_unique has NOT filtered for multiple loops")
}

results_df_unique_filename <- paste0()

  
########################################################################
########################################################################
# Create graph
# we reference graph 'g' that was created earlier

# Export the igraph object to a GraphML file
graph_filename <- paste0("outputs/filtered_", graph_name, "_graph.graphml")
write_graph(g, here(graph_filename), format = "graphml")


########################################################################
########################################################################
# Check combinations of links
# combinations
#generate combinations
sample_nodes <- V(g) %>% as_ids()
combinations <- combn(sample_nodes, 2, simplify = TRUE)

## New combination code
all_combi_edgelist <- data.frame(V1 = as.character(combinations[1,]),
                                 V2 = as.character(combinations[2,]))

current_edgelist <- as.data.frame(as_edgelist(simplify(g), names=TRUE))

no_connection_edgelist <- anti_join(all_combi_edgelist, current_edgelist)

if (test){
  
}
# now, sample 4473 rows of data from the no_connection_edgelist
set.seed(123)
# Take a sample of the row numbers
sampled_rows <- sample(nrow(no_connection_edgelist), size = 4473, replace = FALSE)
# Use the sampled row numbers to subset the data frame
sampled_connections <- no_connection_edgelist[sampled_rows, ]
# combining the  edgelist
combined_data <- rbind(sampled_connections, current_edgelist)
# shuffling the data
shuffled_data <- combined_data[sample(nrow(combined_data)), ]
row.names(shuffled_data) <- NULL
#View(shuffled_data)

shuffled_data <- 
write.csv(shuffled_data, here("outputs/0601_edgelist.csv"), row.names=FALSE)

### Old combination code that will not work if network has too many nodes
#function to check if a given pair of nodes are connected in a given graph
check_connection <- function(combi) {
  result <- are.connected(g, combi[1], combi[2])
  return(result)
}

#check if each pair of nodes are connected
is_connected <- combinations %>% apply(., 2, check_connection)

df_edgelist = data.frame(V1 = as.numeric(combinations[1,]), 
                         V2 = as.numeric(combinations[2,]), 
                         is_connected = is_connected)


########################################################################
########################################################################
# aggregating_number_common_genres
# First, split the genres for each unique node
node_genres <- book_filtered %>%
  select(id, cleaned_genres) %>%
  distinct() %>%
  mutate(genres_list = strsplit(cleaned_genres, "\\|"))

# Create a named list for fast lookup
genre_list <- setNames(node_genres$genres_list, node_genres$id)

# Compute the number of common genres for each row in edgelist
df_edgelist$num_common_genre <- mapply(function(v1, v2) {
  length(intersect(genre_list[[as.character(v1)]], genre_list[[as.character(v2)]]))
}, df_edgelist$V1, df_edgelist$V2)
#
# converting dependent variable, is_connected to interger (initially TRUE/FALSE values)
df_edgelist$is_connected <- df_edgelist_book$is_connected * 1

#save csv

write.csv(df_edgelist, "../data/0601_baseline_dataset.csv", row.names = FALSE)
