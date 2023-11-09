library(data.table)
library(here)
library(dplyr)
library(igraph)

########################################################################
# SPECIFY THE FILE NAME HERE
graph_name <- "0601"
# THEN RUN THE ENTIRE SCRIPT
########################################################################

########################################################################
########################################################################
# Create book dataframe

# date collected for book_data was in Summer 2006. We take the median date which is 7th August 2006
book_data <- data.table(distinct(read.csv("../data/item_index_books.csv")))

# Split the 'cleaned_genres' column by '|' and extract the first genre
book_data <- book_data[, first_genre := sub("\\|.*", "", cleaned_genres)][, c("id", "cleaned_genres", "first_genre", "salesrank", "rating", "reviews")]

head(book_data)

# filter for books with average of > 1 review per month

# load file
txt_file_name <- paste0("../data/Amazon", graph_name, ".txt")
ls_test <- read.table(txt_file_name)

# create graph object, and remove multiple loops
g_test <- simplify(graph_from_data_frame(ls_test, directed = FALSE))

# convert graph to dataframe
g_df <- as_long_data_frame(g_test) %>% select(-c("from_name", "to_name"))

# Left join 'book_attributes' onto 'g_df' based on "from_name" and "id"
result_df <- inner_join(g_df, book_data[, -c("cleaned_genres")], by = c("from" = "id")) %>%
  rename(
    "from_first_genre"="first_genre", 
    "from_salesrank"="salesrank",
    "from_rating"="rating",
    "from_reviews"="reviews"
  ) %>%
  # append book attributes to the to_books
  left_join(., book_data[, -c("cleaned_genres")], by = c("to" = "id")) %>%
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
  
  # number of months from 01 June 2003 to 7th August 2006 is 38 months
  # we now filter for the books which on average, has > 1 review per month
  # filter out books that have an average of < 1 review every month in the 31 month period
  filter((from_reviews/38) > 1, (to_reviews/38) > 1) %>%
  # create is common main genre
  mutate(is_common_main_genre = as.numeric(from_first_genre==to_first_genre))

# Extract unique vertex names
vertex_names <- V(g_test)

# filtered book_data
book_filtered <- book_data %>% 
  filter(id %in% vertex_names) %>%
  select(id, cleaned_genres, first_genre)

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

#### Work on counting number of ALL common genres
node_genres <- book_filtered %>%
  select(id, cleaned_genres) %>%
  distinct() %>%
  mutate(genres_list = strsplit(cleaned_genres, "\\|"))

# Create a named list for fast lookup
genre_list <- setNames(node_genres$genres_list, node_genres$id)
  
# Compute the number of common genres for each row in edgelist
result_df$num_common_genre <- mapply(
  function(book1, book2) {
  length(intersect(
    genre_list[[as.character(book1)]], 
    genre_list[[as.character(book2)]]
      )
    )
  }, result_df$from, result_df$to)

result_df_filename <- paste0("outputs/test_copurchases_filtered_", graph_name, ".csv")

write.csv(result_df, here(result_df_filename), row.names=FALSE)

