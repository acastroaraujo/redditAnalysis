
## Re do all this!!!!


try(setwd("dataCollect"))
source("secretInfo.R")
library(tidyverse)
library(reticulate)
use_condaenv(condaenv = "reddit")
source_python("authenticateAPI.py")
source_python("helperFunctions.py")

# 
# unique_users <- subreddit %>% 
#   filter(num_comments > 0) %>% 
#   select(node_data) %>% 
#   unnest() %>% 
#   distinct(author) %>% 
#   filter(author != "[deleted]") %>% 
#   pull()
# 
# readr::write_rds(unique_users, "consulting_unique_users.rds", 
#                  compress = "gz")
# 

unique_users <- readr::read_rds("consulting_unique_users.rds")

# ********************
# Function
# ********************

output <- vector("list", length(unique_users))
pb <- dplyr::progress_estimated(length(output))

for (i in 515:length(output)) {
  output[[i]] <- try(py$subreddit_list(unique_users[[i]]) %>% 
    map_chr(as.character))
  
  try(pb$tick()$print())
  
  cat("\n", py$reddit$auth$limits$used, "used requests, ",
      py$reddit$auth$limits$remaining, "left...\n")
}

names(output) <- unique_users

# ********************************************
# Debugging
# ********************************************

get_error_index <- function(x) {
  class(unlist(x)) == "try-error"
}

index <- map_lgl(output, get_error_index)

## Number of users with error
length(output[index])


## Save
readr::write_rds(output[!index], "affiliation_network.rds", compress = "gz")



