########################################################################
#This script outputs the following file:
# 1. a csv file item_index_books.csv containing all book specific features of all the books from amazon-meta.txt
# baseline features include only book related features
# this code takes very long to run, thus @Prof, we provide the file for you.

library(dplyr)
library(tidyr)
library(igraph)
library(data.table)
library(glue)
library(gridExtra)
library(ggplot2)
library(glue)
library(stringr)
library(here)

###############################################################################################################
#1. Preprocessing the Metadata
#read metadata txt
meta <- readLines(paste0("../data/amazon-meta.txt"))

#exclude blanks, reviews, actor/actress info so that we deal with less data
meta <- meta[meta != "" & !grepl("^\\s*\\d+-\\d+-\\d+\\s*cutomer:\\s*[A-Za-z0-9]+", meta) & !grepl("\\|Actors & Actresses\\[\\d+\\]\\|[^|]*", meta)]

#define regex pattern and identify item id, start/stop index for each item
pattern <- "^\\s*[iI]+\\s*[dD]+\\s*:+\\s*\\d+$"
start_ind <- which(grepl(pattern, meta))
stop_ind <- c(start_ind[2:length(start_ind)]-1, length(meta))

#function to handle empty ID matches
conditional_fn <- function(x) {
  ifelse(length(x) == 0, NA, x)
}

#get item ids with regex
item_id <- meta[start_ind] %>% regmatches(., gregexpr("\\d+", .)) %>% lapply(., conditional_fn) %>% unlist()


#compile into dataframe
item_index <- data.frame(id = item_id, start = start_ind, stop = stop_ind, length = stop_ind - start_ind + 1)

#get product type
grp_pattern <- "group:\\s*(.+)"
product_type <- c()

for (i in 1:length(item_index %>% row.names())) {
  test_item <- meta[item_index$start[i]:item_index$stop[i]]
  type <- test_item %>% .[grep(grp_pattern, .)] %>% str_match(., grp_pattern) %>% .[2]
  product_type <- c(product_type, type)
}

item_index$type <- product_type

#view distribution of attribute length
item_index %>% group_by(length) %>% summarize(n = n())

#save
#write.csv(item_index, "item_index.csv", row.names = FALSE)
###############################################################################################################



# extract_features_from_books
###############################################################################################################
#create another dataframe which only contain books
item_index_books <- item_index[item_index$type == "Book",] %>% drop_na()

###Extract book genres - the data is formatted such that each book has a main genre, followed by a bunch of sub-genres. 
### Each book can belong in multiple main and sub-genres. For simplicity, we'll only be looking at the main genre as there are too many sub-genres

#define regex pattern for elements containing genre info
genre_pattern <- "\\|\\w+\\[\\d+\\]\\|"

#regex patterns for rating, reviews and salesrank
rating_pattern = "avg rating:\\s*(\\d+(\\.\\d+)?)"
reviews_pattern = "reviews:\\s*total:\\s*(\\d+)"
salesrank_pattern = "salesrank:\\s*(\\d+)"

#empty list to hold extracted features and the re-concatenated genre string
main_genres <- c()
cleaned_genres <- c()

rating_ls <- c()
reviews_ls <- c()
salesrank_ls <- c()

#elements to exclude as they are not genres
non_genre_elems<- c(
  "Books", "Subjects", "Specialty Stores", 
  "Formats", "Amazon.com Stores", "jp-unknown2", 
  "jp-unknown3","jp-unknown1", "By Publisher", 
  "O'Reilly", "Categories","John Wiley & Sons",
  "VHS", "DVD", ""
)

#iterate over every book
for (i in 1:length(row.names(item_index_books))) {
  if (i %% 1000 == 0) {
    print(glue("processed {i} entries"))
  }
  #select an item's character vector and trim white spaces on either side of each element
  char_vec <- meta[item_index_books$start[i]:item_index_books$stop[i]] %>% trimws()
  
  #extract and clean genre using regex
  genres_dirty <- char_vec[which(grepl(genre_pattern, char_vec))] %>% #select the elements which contain genre info
    paste(., collapse = "") %>% #combine them together using paste
    strsplit("\\|") %>% #split into elements using "|" as delimiter
    unlist() %>% #unlist
    str_replace_all(., "\\[\\d+\\]", "") #remove genre and sub-genre ID
  
  #remove non-genre elements from list
  genres_clean <- genres_dirty %>% unique() %>% .[!(. %in% non_genre_elems)]
  
  cleaned_genres <- c(cleaned_genres, paste(genres_clean, collapse = "|"))
  
  #add main genre to main_genres vector if it's not already inside
  main_genres <- c(main_genres, genres_clean[1][!(genres_clean[1] %in% main_genres)])
  
  #get rating
  rating =  char_vec %>% str_extract(., rating_pattern) %>% unlist() %>% .[!is.na(.)] %>% gsub("avg rating: ", "", .) %>% as.numeric()
  rating == ifelse(length(rating) == 0, 0, rating)
  rating_ls <- c(rating_ls, rating)
  
  #get reviews
  reviews = char_vec %>% str_extract(., reviews_pattern) %>% unlist() %>% .[!is.na(.)] %>% gsub("reviews:\\s*total:\\s*", "", .) %>% as.numeric()
  reviews = ifelse(length(reviews) == 0, 0, reviews)
  reviews_ls <- c(reviews_ls, reviews)
  
  #get salesrank
  salesrank = char_vec %>% str_extract(., salesrank_pattern) %>% unlist() %>% .[!is.na(.)] %>% gsub("salesrank:\\s*", "", .) %>% as.numeric()
  salesrank = ifelse(length(salesrank) == 0, 9999999, salesrank)
  salesrank_ls <- c(salesrank_ls, salesrank)
  
  #sound off for every 1000 books processed (to track progress)
  if (i %% 1000 == 0) {
    print(glue("{i} books processed"))
  }
}

#remove NA elements
main_genres <- main_genres[!(is.na(main_genres))]
main_genres[!(main_genres %in% non_genre_elems)]

#save features to dataframe
item_index_books$cleaned_genres <- cleaned_genres
item_index_books$rating <- rating_ls
item_index_books$reviews <- reviews_ls
item_index_books$salesrank <- salesrank_ls
#save csv
#write.csv(item_index_books, "item_index_books.csv", row.names = FALSE)

###############################################################################################################

#save csv
write.csv(item_index_books, "../data/item_index_books.csv", row.names = FALSE)

###############################################################################################################
# book_data <- data.table(item_index_books)

# # Split the 'cleaned_genres' column by '|' and extract the first genre
# book_data[, first_genre := sub("\\|.*", "", cleaned_genres)]
# 
# # Get the counts of the first genre
# genre_counts <- book_data[, .N, by = first_genre]
# 
# # Assuming 'genre_counts' contains the counts of each genre
# top_genres <- genre_counts[order(-N)][1:50]
# 
# # Print or inspect the top 20 genres
# print(top_genres)
# #preview
# #item_index_books
###############################################################################################################
