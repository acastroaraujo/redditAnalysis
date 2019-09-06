##Load packages
library(tidyverse)
library(text2vec)
library(tm)
library(textclean)


w2v_preprocess <- function(data_frame, rs) {
  
  output <- data_frame %>% 
    filter(num_comments > 0) %>% 
    select(-edge_list) %>% 
    unnest() %>%
    filter(text != "[deleted]") %>% 
    mutate(text = text %>% 
             textclean::replace_contraction() %>% 
             textclean::replace_url() %>% 
             str_to_lower() %>% 
             textclean::replace_emoji() %>% 
             tm::removeNumbers() %>%
             tm::removePunctuation() %>%
             str_replace_all(rs) %>% 
             str_squish() %>% 
             tm::removeWords(tidytext::stop_words[tidytext::stop_words$lexicon == "snowball", ]$word)) %>% 
    group_by(title, date) %>% 
    summarize(text = str_c(text, collapse = " ")) %>%
    ungroup() %>% 
    mutate(text = str_squish(text))
  
  return(output)
}



w2v_reddit <- function(data_frame, word_min = 30, lr = 0.1){
  
  cat("\nFitting word2vec...\n")
  tokens <- str_split(data_frame$text, pattern = " ")
  vocab <- create_vocabulary(itoken(tokens), ngram = c(1, 1))
  vocab <- prune_vocabulary(vocab, term_count_min = word_min)
  
  iter <- itoken(tokens)
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(iter, vectorizer, skip_grams_window = 3)
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


w2v_cosine_similarity <- function(x, embedding) {
  lookup <- embedding[x, , drop = FALSE]
  text2vec::sim2(embedding, lookup, method = "cosine", norm = "l2") %>% 
    as_tibble(rownames = "word") %>% 
    arrange(desc(!!sym(x)))
}


w2v_matrix <- function(x_list, embedding) {
  text2vec::sim2(embedding, embedding[x_list, ], method = "cosine", norm = "l2")[x_list, ]
}


