
library(tidyverse)

library(spacyr)
spacy_initialize(condaenv = "r-NLP", model = "en_core_web_sm")

## Read in the thread(s): create "all_threads" object

## This function now gives some control over string replacement and stopwords
all_threads_processed <- w2v_preprocess(all_threads)

## Make use of function operator
safe_parse <- safely(spacy_parse)

## Lemmatization
all_threads_processed <- all_threads_processed %>% 
  mutate(spacy = map(text, safe_parse, dependency = TRUE, tag = TRUE))

## Extract results
all_threads_processed <- all_threads_processed %>% 
  mutate(result = map(spacy, function(x) x$result))

## Analysis: Clustering of most common nouns
df <- bind_rows(lemmatized_threads$result)

noun_list <- df %>%
  filter(pos == "NOUN") %>% 
  count(lemma) %>% 
  top_n(100, wt = n) %>% 
  arrange(desc(n)) %>% 
  pull(lemma)

noun_mat <- w2v_similarity_matrix(lemma_vectors, noun_list)

tsne_out <- Rtsne::Rtsne(noun_mat, check_duplicates = FALSE, perplexity = 5)
hc_norm <- hclust(dist(tsne_out$Y), method = "ward.D")
tsne_df <- tibble(
  word = rownames(noun_mat), 
  x = tsne_out$Y[ , 1], y = tsne_out$Y[, 2], 
  group = factor(cutree(hc_norm, 7)) ## seven clusters, change this through eye-balling
  ) 

tsne_df %>% 
  ggplot(aes(x, y, label = word, color = group)) + 
  #geom_point() +
  ggrepel::geom_label_repel(show.legend = FALSE) +
  theme_custom() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank())

