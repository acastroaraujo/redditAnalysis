try(setwd("dataCollect"))
source("secretInfo.R")
library(reticulate)
use_condaenv(condaenv = "reddit")
source_python("authenticateAPI.py")

# *********************************
# Replace this with the subreddit you are interested in.
sub <- "consulting"
# *********************************

source_python("get_threads.py")

subreddit_df <- tibble(
  title = py$title,
  post_score = py$post_score,
  num_comments = py$num_comments,
  date = as.POSIXct(py$date, origin = "1970-01-01", tz = "UTC"),
  link = py$link,
  subreddit = sub
)

dplyr::glimpse(subreddit_df)

readr::write_csv(subreddit_df, "subreddit_urls.csv")



