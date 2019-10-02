
## Re do all this, see code at the end!!!!

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



## Re build ideas!



source("reddit/secretInfo.R")
library(tidyverse)
library(reticulate)
use_condaenv(condaenv = "reddit")
source_python("reddit/authenticateAPI.py")
source_python("reddit/helperFunctions.py")



### Notes. The rebuild of this depends on whether it's better to use a list comprehension
## Inside a py_run_string function or whether to source_python a small python function.
## If I can define Python functions from within an R script, that would be better.
## Right now, it might be better to wrap the list comprehension inside a py_to_r() that is 
## itself wrapped inside a try() function.

user_df <- readr::read_rds("reddit/content.rds")
user_unique <- unique(user_df$user, unique(user_df$author))


# reticulate::py_run_string("
#   def subreddit_list(name):
#     output = []
#   for i in reddit.redditor(name).new():
#     output.append(i.subreddit)
#   return(output)
#   ")


output <- vector("list", length(user_unique))
pb <- dplyr::progress_estimated(length(user_unique))
for (i in seq_along(user_unique)) {
  name <- user_unique[[i]]
  try(py_run_string("temp = [i.subreddit.display_name for i in reddit.redditor(r.name).new()]"))
  output[[i]] <- py$temp
  try(pb$tick()$print())
}

names(output) <- user_unique



py$temp

a <- py$reddit$redditor(x)$new()

for (i in a) {
  print(i)
}

x <- user_unique[[1]]

py$b %>% map_chr(as.character)


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


# ********************
# Function
# ********************

output <- vector("list", length(user_unique))
pb <- dplyr::progress_estimated(length(output))

for (i in 1:length(output)) {
  output[[i]] <- try(py$subreddit_list(user_unique[[i]]) %>% 
                       map_chr(as.character))
  
  try(pb$tick()$print())
  
  cat("\n", py$reddit$auth$limits$used, "used requests, ",
      py$reddit$auth$limits$remaining, "left...\n")
}

names(output) <- user_unique

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
readr::write_rds(output[!index], "reddit/affiliation_network.rds", compress = "gz")








