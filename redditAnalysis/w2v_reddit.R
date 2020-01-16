
# *********************************************************
# This script contains functions for working with word2vec
# models trained on Reddit conversations.
# - w2v_preprocess
# - w2v_fit
# - w2v_cosine_similarity
# - w2v_similarity_matrix
# - w2v_wordcloud
# *********************************************************


library(tidyverse)
library(text2vec)
library(tm)
library(textclean)


w2v_preprocess <- function(data_frame, rs = c("/r/" = ""), words_to_remove = NULL) {
  
  if (is.null(words_to_remove)) {
    words_to_remove <- tidytext::stop_words %>% 
      filter(lexicon == "snowball") %>% 
      pull(word)
  }
  
  output <- data_frame %>% 
    filter(num_comments > 0) %>%
    select(-edge_list, -date, -title, -subreddit) %>% 
    unnest(cols = node_data) %>% 
    filter(text != "[deleted]") %>% 
    mutate(text = text %>% 
             str_replace_all(pattern = "’", replacement = "'") %>% 
             textclean::replace_contraction() %>% 
             textclean::replace_url() %>% 
             textclean::replace_emoji() %>% 
             tm::removeNumbers() %>%
             tm::removePunctuation() %>%
             str_squish() %>% 
             str_to_lower() %>% 
             str_replace_all(rs) %>% 
             tm::removeWords(words_to_remove)) %>% 
    group_by(title, date) %>% 
    summarize(text = str_c(text, collapse = " ")) %>%
    ungroup() %>% 
    mutate(text = str_squish(text))
  
  return(output)
}



w2v_fit <- function(data_frame, word_min = 30, lr = 0.1, skip_grams_window = 3, ngram_max = 1){
  
  message("Fitting word2vec...")
  tokens <- str_split(data_frame$text, pattern = " ")
  vocab <- create_vocabulary(itoken(tokens), ngram = c(1, ngram_max))
  vocab <- prune_vocabulary(vocab, term_count_min = word_min)
  
  iter <- itoken(tokens)
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(iter, vectorizer, skip_grams_window = skip_grams_window)
  fit_glove <- GloVe$new(word_vectors_size = 100,
                         vocabulary = vocab,
                         x_max = 10,
                         learning_rate = lr)
  word_vectors_main = fit_glove$fit_transform(tcm, n_iter = 30)
  word_vectors_context <-  fit_glove$components
  word_vectors   <- word_vectors_main + t(word_vectors_context)
  row.names(word_vectors) <- rownames(tcm)
  
  return(word_vectors)
}


w2v_fit_from_lemmas <- function(tokens, word_min = 30, lr = 0.1, skip_grams_window = 3, ngram_max = 1){
  
  message("Fitting word2vec...")
  vocab <- create_vocabulary(itoken(tokens), ngram = c(1, ngram_max))
  vocab <- prune_vocabulary(vocab, term_count_min = word_min)
  
  iter <- itoken(tokens)
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(iter, vectorizer, skip_grams_window = skip_grams_window)
  fit_glove <- GloVe$new(word_vectors_size = 100,
                         vocabulary = vocab,
                         x_max = 10,
                         learning_rate = lr)
  word_vectors_main = fit_glove$fit_transform(tcm, n_iter = 30)
  word_vectors_context <-  fit_glove$components
  word_vectors   <- word_vectors_main + t(word_vectors_context)
  row.names(word_vectors) <- rownames(tcm)
  
  return(word_vectors)
}

w2v_cosine_similarity <- function(embedding, x) {
  stopifnot(all(x %in% rownames(embedding)))
  
  w <- sym(paste(x, collapse = "_"))
  lookup <- embedding[x, , drop = FALSE] %>% 
    colSums() %>% 
    matrix(nrow = 1) 
  
  attr(lookup, which = "dimnames") <- list(paste(x, collapse = "_"), NULL)
  
  df <- text2vec::sim2(embedding, lookup, method = "cosine", norm = "l2") %>% 
    as_tibble(rownames = "word") %>% 
    arrange(desc(!!w))
  
  return(df)
}


w2v_similarity_matrix <- function(embedding, word_list) {
  text2vec::sim2(embedding, embedding[word_list, ], method = "cosine", norm = "l2")[word_list, ]
}

w2v_dist_to_target <- function(embedding, target, word_list) {
  w2v_cosine_similarity(word_vectors, target) %>% 
    filter(word %in% word_list) %>% 
    arrange(desc(!!sym(target)))
}


w2v_wordcloud <- function(embedding, n = 50, x, max_size = 10) {
  require(ggwordcloud)
  
  w <- sym(paste(x, collapse = "_"))
  df <- w2v_cosine_similarity(embedding, x) %>% 
    filter(!word %in% x)
  
  df %>% ## Wordcloud
    top_n(n) %>%
    ggplot(aes(size = !!w, label = word, color = !!w)) +
    geom_text_wordcloud(family = "Avenir", fontface = "bold", shape = "square") +
    theme_minimal(base_line_size = 0) +
    theme(plot.title = element_text(family = "Avenir", face = "bold"),
          plot.subtitle = element_text(family = "Avenir")) + 
    scale_size_area(max_size = max_size) + 
    scale_color_viridis_c(option = "magma",begin = 0.3, end = 0.85, direction = -1) +
    labs(title = as.character(w))
}


