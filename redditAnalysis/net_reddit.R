
# *********************************************************
# This script contains functions for working with network
# representations of Reddit conversations.
# - net_grow_tree() [simple wrapper around igraph::graph_from_data_frame]
# - net_plot()
# *********************************************************

net_grow_tree <- function(nodes, edges) {
  igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
}

# **********************************************************
# Use purrr::map2 to use this function in a dplyr pipeline:
# df <- df %>% 
#   filter(purrr::map_lgl(edges, is.null)) %>% 
#   mutate(graph = purrr::map2(nodes, edges, net_grow_tree))
# **********************************************************

net_plot <- function(
  graph,
  title = "",
  url = "",
  style = 1,
  n_label_rank = 5
  ) {
  
  stopifnot(style %in% 1:4)
  
  if (style == 1) {
    message("circle pack!")
    g <- tidygraph::as_tbl_graph(graph) %>% 
      mutate(root = ifelse(is.na(score), TRUE, FALSE)) %>%
      ggraph("circlepack", circular = TRUE) +
      geom_edge_link(alpha = 0.5, width = 0.1, check_overlap = TRUE) +
      geom_node_point(size = 0.01)
    
  }
  
  if (style == 2) {
    message("curvy circle pack!")
    g <- tidygraph::as_tbl_graph(graph) %>% 
      mutate(root = ifelse(is.na(score), TRUE, FALSE)) %>%
      ggraph("circlepack", circular = TRUE) +
      geom_edge_arc(alpha = 0.5, width = 0.1, check_overlap = TRUE, strength = 1/4) +
      geom_node_point(size = 0.01)
    
  }
  
  if (style == 3) {
    message("dendrogram!")
    g <- tidygraph::as_tbl_graph(graph) %>% 
      mutate(root = ifelse(is.na(score), TRUE, FALSE)) %>%
      ggraph("dendrogram", circular = FALSE) +
      geom_edge_diagonal(alpha = 0.5, width = 0.1, check_overlap = TRUE) +
      geom_node_point(size = 0.01)
  }
  
  if (style == 4) {
    message("circular dendrogram!")
    g <- tidygraph::as_tbl_graph(graph) %>% 
      mutate(root = ifelse(is.na(score), TRUE, FALSE)) %>%
      ggraph("dendrogram", circular = TRUE) +
      geom_edge_diagonal(alpha = 0.5, width = 0.1, check_overlap = TRUE) +
      geom_node_point(size = 0.01)
    
  }
  
  g + geom_node_label(
    aes(filter = is.na(score) | rank(-descendents) <= n_label_rank, 
        label = str_wrap(str_trunc(text, width = 300), width = 50), color = root), 
    size = 2, repel = TRUE, alpha = 0.9, show.legend = FALSE) +
    scale_color_manual(values = c("black", "dodgerblue")) +
    theme_graph() +
    labs(title = str_wrap(title, width = 50), caption = url) +
    theme(plot.title = element_text(color = "dodgerblue3", face = "bold"))
}






###



# 
# 
# 
# library(tidyverse)
# theme_set(hrbrthemes::theme_ipsum())
# 
# ## Reddit Network
# 
# df <- readr::read_rds("reddit/affiliation_network.rds") %>% 
#   tibble::enframe(name = "author", value = "subreddit") %>% 
#   unnest(cols = "subreddit")
# 
# # Number of unique subreddits
# length(unique(df$subreddit))
# 
# # Number of unique users
# length(unique(df$author))
# 
# df %>% 
#   count(subreddit) %>% 
#   mutate(prop = n / sum(n)) %>% 
#   arrange(desc(n)) %>%
#   top_n(20) %>%
#   ggplot(aes(x = fct_reorder(subreddit, prop), y = prop)) +
#   geom_point(color = "steelblue1") + 
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   scale_y_continuous(labels = scales::percent)
# 
# adj_mat <- df %>% 
#   count(author, subreddit) %>% 
#   group_by(subreddit) %>% filter(sum(n) > 100) %>%  ## throw away very idiosyncratic subreddits
#   tidytext::cast_sparse(author, subreddit, n) 
# 
# dim(adj_mat)
# 
# library(Matrix) ## for sparse matrix operations
# 
# ## sparsity
# (sum(adj_mat == 0) / prod(dim(adj_mat))) %>% scales::percent()
# 
# ## dual projection
# M <- Matrix::crossprod(adj_mat)
# 
# dim(M)
# 
# M[1:5, 1:5]
# 
# ## create graph object
# g <- igraph::graph_from_adjacency_matrix(
#   adjmatrix = M, 
#   mode = "undirected", 
#   weighted = TRUE, 
#   diag = FALSE
# )
# 
# igraph::V(g)$users <- diag(M)
# 
# igraph::V(g)$louvain_cluster <- igraph::cluster_louvain(g) %>% 
#   igraph::membership() %>% 
#   factor()
# 
# igraph::V(g)$greedy_cluster <- igraph::cluster_fast_greedy(g) %>% 
#   igraph::membership() %>% 
#   factor()
# 
# igraph::V(g)$walktrap_cluster <- igraph::cluster_walktrap(g) %>% 
#   igraph::membership() %>% 
#   factor()
# 
# 
# igraph::V(g)$eigen_cluster <- igraph::cluster_leading_eigen(g) %>% 
#   igraph::membership() %>% 
#   factor()
# 
# 
# igraph::V(g)$spinglass_cluster <- igraph::cluster_spinglass(g) %>% 
#   igraph::membership() %>% 
#   factor()
# 
# 
# # *********************************************
# # Cluster diagnostics
# # *********************************************
# 
# cluster_gini <- function(x, remove_na = TRUE) {
#   if(!remove_na && any(is.na(x))) return(NA_real_)
#   x <- as.numeric(na.omit(x)) %>% table()
#   n <- length(x)
#   x <- sort(x)
#   G <- sum(x * 1L:n)
#   G <- 2 * G/sum(x) - (n + 1L)
#   return(G/n)
# }
# 
# tidygraph::as_tbl_graph(g) %>% 
#   as_tibble() %>%
#   filter(name != "keto") %>% 
#   summarise_if(is.integer, max)
# 
# tidygraph::as_tbl_graph(g) %>%
#   as_tibble() %>%
#   filter(name != "keto") %>% 
#   summarise_if(is.integer, cluster_gini)
# 
# 
# # ****************************************
# # Plot communities
# # ****************************************
# 
# tidygraph::as_tbl_graph(g) %>%
#   as_tibble() %>% 
#   pivot_longer(cols = ends_with("cluster"), names_to = "algorithm", values_to = "cluster") 
# group_by()
# ggplot(aes(x = name, y = users)) + 
#   geom_col() +
#   coord_flip() 
# 
# 
# 
# tidygraph::as_tbl_graph(g) %>%
#   as_tibble() %>%
#   group_by(spinglass_cluster) %>%
#   top_n(10, wt = users) %>% 
#   ggplot(aes(x = fct_reorder(name, users), y = users)) + 
#   geom_col() +
#   coord_flip() + 
#   facet_wrap(~spinglass_cluster, scales = "free_y", ncol = 3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# tidygraph::as_tbl_graph(g) %>% 
#   as_tibble() %>%
#   group_by(walktrap_cluster) %>% 
#   ggplot(aes(x = fct_reorder(name, users), y = users)) + 
#   geom_col() + coord_flip() +
#   facet_wrap(~louvain_cluster, scales = "free_y", ncol = 2)
# 
# library(ggraph)
# tidygraph::as_tbl_graph(g) %>%
#   tidygraph::activate(edges) %>% 
#   mutate(rank = rank(-weight)) %>% 
#   filter(rank %in% 1:5) %>% 
#   ggraph("kk") +
#   geom_edge_arc()

