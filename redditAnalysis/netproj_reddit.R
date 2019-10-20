
# *********************************************************
# This script contains functions for working with affiliation
# networks between subreddits and redditors
# - netproj_create_edgelist() from the output in affiliationNetwork.R
# - netproj_subreddit_adj_mat()
# - netproj_plot_n()
# - netproj_cluster()
# - netproj_cluster_diagnostics()
# - netproj_plot_communities()
# *********************************************************


library(tidyverse)
theme_set(theme_minimal())


# 1st step. Import the affiliation network created with affiliationNetwork.R


# input <- read_rds("truly/affiliation_network.rds") 
# 
# index <- map_lgl(input, is_empty)

netproj_create_edgelist <- function(input) {
  stopifnot(class(input) == "list")
  
  output <- input %>% 
    tibble::enframe(name = "author", value = "subreddit") %>% 
    tidyr::unnest(cols = "subreddit")
  
  message(paste(length(unique(output$author)), "unique users"))
  message(paste(length(unique(output$subreddit)), "unique subreddits"))
  
  return(output)

}

# data_frame <- netproj_create_edgelist(input[!index])

# data_frame %>%
#   count(subreddit) %>%
#   mutate(prop = n / sum(n)) %>%
#   arrange(desc(n)) %>%
#   top_n(20) %>%
#   ggplot(aes(x = fct_reorder(subreddit, prop), y = prop)) +
#   geom_point(color = "steelblue1") +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   scale_y_continuous(labels = scales::percent)

netproj_subreddit_adj_mat <- function(data_frame, min_n = 100) {
  require(Matrix) 
  
  adj_mat <- data_frame %>% 
    count(author, subreddit) %>% 
    group_by(subreddit) %>% filter(sum(n) > min_n) %>%  ## throw away very idiosyncratic subreddits
    tidytext::cast_sparse(author, subreddit, n) 
  
  message("Original matrix dimensions: ", paste(dim(adj_mat), collapse = " x "))
  
  M <- Matrix::crossprod(adj_mat)
  
  message("Projected matrix dimensions: ", paste(dim(M), collapse = " x "))
  message("Sparsity ", (sum(adj_mat == 0) / prod(dim(adj_mat))) %>% scales::percent())
  
  return(M)
  
}

# M <- netproj_subreddit_adj_mat(data_frame, min_n = 100)


netproj_plot_n <- function(M, N = 10, title = NULL, caption = NULL) {
  require(ggraph)
  
  index <- order(diag(M), decreasing = TRUE)

  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = M[index, index][1:N, 1:N], 
    mode = "undirected", 
    weighted = TRUE, 
    diag = FALSE
    )
  
  ggraph(g) +
    geom_edge_fan(aes(alpha = weight, width = weight), show.legend = FALSE) +
    geom_node_point() +
    geom_node_label(aes(label = name), repel = TRUE) +
    theme_graph() +
    labs(title = title, caption = caption)
}


# tag <- c('If a Redditor talked about Truly, what are the top 20 subreddits they are also participating in?')
# tag_caption <- c("the width of each tie indicates a larger number of mutual users connecting both subreddits")
# 
# netproj_plot_n(M, N = 20, title = str_wrap(tag, width = 60), caption = str_wrap(tag_caption, width = 50))
# ggsave("truly/truly_top_20.png", device = "png", dpi = "print", height = 10, width = 8)

netproj_cluster <- function(M) {
  
  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = M, 
    mode = "undirected", 
    weighted = TRUE, 
    diag = FALSE
    )
  
  igraph::V(g)$users <- diag(M)
  
  message("Finding community structure by multi-level optimization of modularity: igraph::louvain_cluster")
  igraph::V(g)$louvain_cluster <- igraph::cluster_louvain(g) %>% 
    igraph::membership() %>% 
    factor()
  
  message("Community structure via greedy optimization of modularity: igraph::cluster_fast_greedy")
  igraph::V(g)$greedy_cluster <- igraph::cluster_fast_greedy(g) %>% 
    igraph::membership() %>% 
    factor()
  
  message("Community strucure via short random walks: igraph::cluster_walktrap")
  igraph::V(g)$walktrap_cluster <- igraph::cluster_walktrap(g) %>% 
    igraph::membership() %>% 
    factor()
  
  message("Community structure detecting based on the leading eigenvector of the community matrix: igraph::cluster_leading_eigen")
  igraph::V(g)$eigen_cluster <- igraph::cluster_leading_eigen(g) %>% 
    igraph::membership() %>% 
    factor()
  
  message("Finding communities in graphs based on statistical mechanics: igraph::cluster_spinglass")
  igraph::V(g)$spinglass_cluster <- igraph::cluster_spinglass(g) %>% 
    igraph::membership() %>% 
    factor()
  
  return(g)
}



# g <- netproj_cluster(M)
# tidygraph::as_tbl_graph(g)


netproj_cluster_diagnostics <- function(g) {
  
  cluster_gini <- function(x) {
    stopifnot(!any(is.na(x)))
    x <- as.numeric(x) %>% table()
    n <- length(x)
    x <- sort(x)
    G <- sum(x * 1L:n)
    G <- 2 * G/sum(x) - (n + 1L)
    return(round(G/n, 2))
  }
  
  bind_rows(
  igraph::as_data_frame(g, what = "vertices") %>% 
    summarise_if(is.integer, max) %>% 
    mutate(diagnostic = "groups"),

  igraph::as_data_frame(g, what = "vertices") %>% 
    summarise_if(is.integer, cluster_gini) %>% 
    mutate(diagnostic = "gini")
  )
  
}

# netproj_cluster_diagnostics(g)

netproj_plot_communities <- function(g, igraph_algorithm, N = 15, title = "", ncol = 3) {
  stopifnot(igraph_algorithm %in% c("louvain_cluster", "greedy_cluster", "walktrap_cluster", "spinglass_cluster", "eigen_cluster"))
  
  algo <- sym(igraph_algorithm)
  
  tidygraph::as_tbl_graph(g) %>% 
    as_tibble() %>% 
    group_by(!!algo) %>% 
    top_n(N, wt = users) %>% 
    ggplot(aes(x = fct_reorder(name, users), y = users)) + 
    geom_col(aes(alpha = log(users)), show.legend = FALSE) +
    coord_flip() + 
    facet_wrap(vars(!!algo), scales = "free", ncol = ncol) +
    labs(x = NULL, y = NULL, title = title, caption = igraph_algorithm)
  
}


# netproj_cluster_diagnostics(g)
# netproj_plot_communities(g, igraph_algorithm = "louvain_cluster", title = "Algorithm # 1")
# ggsave("sam adams/cluster1.png", device = "png", dpi = "print", height = 10, width = 8)
# 
# 
# netproj_plot_communities(g, igraph_algorithm = "greedy_cluster", title = "Algorithm # 2")
# ggsave("sam adams/cluster2.png", device = "png", dpi = "print", height = 10, width = 8)
# 
# netproj_plot_communities(g, igraph_algorithm = "walktrap_cluster", title = "Algorithm # 3")
# ggsave("sam adams/cluster3.png", device = "png", dpi = "print", height = 10, width = 8)
# 

