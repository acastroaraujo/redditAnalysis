
source("redditData/R/secretInfo.R")
library(tidyverse)
library(reticulate)
use_condaenv(condaenv = "reddit")
source_python("redditData/Python/authenticateAPI.py")

## Define a function.
py_run_string('
def subreddit_list(name):
  output = []
  for i in reddit.redditor(name).new():
    output.append(i.subreddit)
  return(output)
')


# df <- read_rds("boston/boston_subreddits.rds")
# user_unique <- unique(bind_rows(df$node_data)$author)

create_affiliation_network <- function(list_of_unique_users) {
  
  output <- vector("list", length(list_of_unique_users))
  pb <- dplyr::progress_estimated(length(list_of_unique_users))
  for (i in seq_along(list_of_unique_users)) {
    output[[i]] <- try(py$subreddit_list(list_of_unique_users[[i]]) %>% map_chr(as.character))
    
    try(pb$tick()$print())
    message("\n", py$reddit$auth$limits$used, " used requests, ", py$reddit$auth$limits$remaining, " left..")
  }
  
  names(output) <- list_of_unique_users
  return(output)
  
}


#output <- create_affiliation_network(user_unique[1:5])



# ********************************************
# Debugging
# ********************************************

get_error_index <- function(x) class(unlist(x)) == "try-error" | is.null(x)
# index <- map_lgl(output, get_error_index)
# 
# ## Number of users with error
# length(output[index])

# ********************************************
# Save
# ********************************************

# readr::write_rds(output[!index], "boston/affiliation_network.rds", compress = "gz")












