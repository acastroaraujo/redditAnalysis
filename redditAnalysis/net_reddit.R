
# *********************************************************
# This script contains functions for working with word2vec
# models trained on Reddit conversations.
# - net_plot
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


