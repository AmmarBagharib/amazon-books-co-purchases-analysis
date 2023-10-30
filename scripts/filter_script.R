library(igraph)
library(data.table)
library(here)
library(dplyr)
library(caret)
library(pROC)
setwd(here())

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
ls_0302 <- read.table('../data/Amazon0302.txt')
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

write.csv(book_filtered, here("outputs/book_filtered.csv"), row.names=FALSE)


########################################################################
########################################################################
# Create graph

# Create an igraph object
g <- graph_from_data_frame(result_df[, c("from", "to")], directed=FALSE)

# Step 1: Create a mapping between book IDs in the graph and row IDs in the data frame
book_ids <- V(g)$name  # Extract book IDs from the graph
row_ids <- match(book_ids, book_filtered$id)  # Match graph IDs to row IDs

# Step 2: Create a data frame with the attributes to be added
vertex_data <- data.frame(
  book1_first_genre = book_filtered$first_genre[row_ids],
  book2_first_genre = book_filtered$first_genre[row_ids],
  book1_rating = book_filtered$rating[row_ids],
  book2_rating = book_filtered$rating[row_ids],
  book1_reviews = book_filtered$reviews[row_ids],
  book2_reviews = book_filtered$reviews[row_ids],
  book1_salesrank = book_filtered$salesrank[row_ids],
  book2_salesrank = book_filtered$salesrank[row_ids]
)


# Step 3: Add these attributes to the graph as vertex attributes
V(g)$book1_first_genre <- vertex_data$book1_first_genre
V(g)$book2_first_genre <- vertex_data$book2_first_genre
V(g)$book1_rating <- vertex_data$book1_rating
V(g)$book2_rating <- vertex_data$book2_rating
V(g)$book1_reviews <- vertex_data$book1_reviews
V(g)$book2_reviews <- vertex_data$book2_reviews
V(g)$book1_salesrank <- vertex_data$book1_salesrank
V(g)$book2_salesrank <- vertex_data$book2_salesrank

#g

# Export the igraph object to a GraphML file
write_graph(g, here("outputs/filtered_graph.graphml"), format = "graphml")


########################################################################
########################################################################
# Check combinations of links


# combinations
#generate combinations
sample_nodes <- V(g) %>% as_ids()
combinations <- combn(sample_nodes, 2, simplify = TRUE)

combi <- combinations[,1]

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
df_edgelist$num_common_main_genre <- mapply(function(v1, v2) {
  length(intersect(genre_list[[as.character(v1)]], genre_list[[as.character(v2)]]))
}, df_edgelist$V1, df_edgelist$V2)
#
# converting dependent variable, is_connected to interger (initially TRUE/FALSE values)
df_edgelist$is_connected <- df_edgelist_book$is_connected * 1

#save csv
write.csv(df_edgelist, "../data/baseline_dataset.csv", row.names = FALSE)
