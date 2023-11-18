library(igraph)
library(ggplot2)
library(dplyr)
library(here)
library(ggpubr)
library(gridExtra)

########################################################################
########################################################################
# degree and transitivity distributions

#load network metrics for sampled 0302
sampled_metrics <- read.csv(here("outputs/network_metrics_0302.csv"))

#load original network and calculate deg and local transitivity
original_0302 <- read.table("../data/Amazon0302.txt")
original_0302_graph <- simplify(graph_from_data_frame(original_0302, directed = FALSE))
original_deg <- degree(original_0302_graph)
original_trans <- transitivity(original_0302_graph, type = "local")

font_size <- 7
title_font <- 7
#visualize distributions
sampled_gr_deg <- ggplot(sampled_metrics, aes(x = degree)) + 
  geom_histogram(bins = 7, fill="#b3a369") + 
  scale_y_log10() + 
  scale_x_log10() +
  labs(title = "Sampled Graph", x = "log(Degree Centrality)", y = "log(Frequency)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


orig_gr_deg <- ggplot() + 
  geom_histogram(bins = 7, aes(x = original_deg), fill="#69b3a2") + 
  scale_y_log10() +
  scale_x_log10() +
  labs(title = "Original Graph", x = "log(Degree Centrality)", y = "log(Frequency)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


sampled_gr_trans <- ggplot(sampled_metrics, aes(x = transitivity)) + 
  geom_histogram(bins = 7, fill="#b3a369") + 
  scale_y_log10() +
  labs(title = "Sampled Graph", x = "Local Transitivity", y = "log(Frequency)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


orig_gr_trans <- ggplot() + 
  geom_histogram(bins = 7, aes(x = original_trans), fill="#69b3a2") + 
  scale_y_log10() +
  labs(title = "Original Graph", x = "Local Transitivity", y = "log(Frequency)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Combine the plots
grid.arrange(sampled_gr_deg, orig_gr_deg, sampled_gr_trans, orig_gr_trans,
             ncol = 2)

#save
g1 <- arrangeGrob(sampled_gr_deg, orig_gr_deg, sampled_gr_trans, orig_gr_trans, ncol=2) #generates g
ggsave(file=here("outputs/plots_and_tables/1_combined_plot.png"), 
       plot=g1,
       width = 8,
       height = 8,
       units = "in",
       dpi = 300
       ) #saves g

########################################################################
########################################################################
# degree

g2 <- ggplot(sampled_metrics, aes(x = degree)) + 
  geom_histogram(fill="#69b3a2") + 
  labs(title = "Sampled Graph - Degree Distribution", x = "Degree Centrality", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave(file=here("outputs/plots_and_tables/2_degree_plot.png"), 
       plot=g2,
       width = 8,
       height = 4,
       units = "in",
       dpi = 300
)

########################################################################
########################################################################
# global transitivity & denisty

sampled_0302_graph <- read_graph(here("outputs/sample_graph_0302.graphml"), format="graphml")
global_transitivity <- transitivity(sampled_0302_graph, type="global")
density <- graph.density(sampled_0302_graph)

df1 <- data.frame(
  Metric = c("Global Transitivity", "Density"),
  Value = c(global_transitivity, density)
)

write.csv(df1, here("outputs/plots_and_tables/3_transitivity_density.csv"), row.names = FALSE)

########################################################################
########################################################################
# pagerank

#visualize distribution of pagerank
g3 <- ggplot(sampled_metrics) + 
  geom_histogram(aes(x = pagerank), fill="#69b3a2") + 
  #scale_y_log10() +
  labs(title = "Sampled Graph - Pagerank", x = "Pagerank", y = "log(Frequency)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = font_size),   # Adjust x-axis label size
        axis.text.y = element_text(size = font_size),
        axis.title.x = element_text(size = title_font),  # Adjust x-axis title size
        axis.title.y = element_text(size = title_font),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave(file=here("outputs/plots_and_tables/4_pagerank_plot.png"), 
       plot=g3,
       width = 8,
       height = 4,
       units = "in",
       dpi = 300
)

########################################################################
########################################################################
# top 10 books and genres

#load book-specific features
books_info <- read.csv("../data/item_index_books.csv")

#left join books_info to sampled_metrics
sampled_metrics <- sampled_metrics %>% left_join(., books_info, by = c("vertex_name"="id")) %>% arrange(desc(pagerank)) %>% distinct()

#view top 20 books' pagerank
df2 <- sampled_metrics %>% head(10) %>% .[,c("vertex_name", "pagerank")]

write.csv(df2, here("outputs/plots_and_tables/5_top-n-books.csv"), row.names = FALSE)

top_genres <- c()

for (i in 1:100) {
  vec = sampled_metrics$cleaned_genres[i] %>% strsplit(., "\\|")
  top_genres <- c(top_genres, vec) %>% unlist()
}

#count genres in top 100 pagerank books
res <- top_genres %>% table() %>% as.data.frame()
colnames(res) <- c("Genre", "Count")
df3 <- res %>% arrange(desc(Count)) %>% .[3:13,]
write.csv(df3, here("outputs/plots_and_tables/6_top-n-genres.csv"), row.names = FALSE)











