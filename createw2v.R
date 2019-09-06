library(tidyverse)
source("w2v_reddit.R")
subreddit <- readr::read_rds("consulting.rds")



replacement_string <- c("&" = "and",
                        "/r/" = "",
                        "firms" = "firm")

df <- w2v_preprocess(subreddit, rs = replacement_string)
wordVectors_consulting <- w2v_reddit(df)
readr::write_rds(wordVectors_consulting, "consulting_matrix.rds", compress = "gz")

