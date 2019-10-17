
source("redditData/R/secretInfo.R")

# *********************************
# Replace this with the subreddit you are interested in.
# *********************************

sub <- "init" 
keyword <- "init"
## this is necessary (I think) due to weird scoping and interoperability between R and Python
message("Any variables in the global environment named 'sub' or 'keyword' have now been replaced.")

downloadSubredditURLs <- function(sub) {
  
  sub <<- sub
  
  cat(paste0("Pulling from /r/", sub, "...\n"))
  source_python("redditData/Python/get_threads.py")
  
  subreddit_df <- tibble(
    title = py$title,
    post_score = py$post_score,
    num_comments = py$num_comments,
    date = as.POSIXct(py$date, origin = "1970-01-01", tz = "UTC"),
    link = py$link,
    subreddit = sub
  )
  
  return(subreddit_df)

}


downloadKeywordURLs <- function(keyword) {
  
  sub <<- "all"
  keyword <<- keyword
  
  cat(paste0("Searching for ", keyword, "...\n"))
  source_python("redditData/Python/get_threads_by_keyword.py")
  
  subreddit_df <- tibble(
    title = py$title,
    post_score = py$post_score,
    num_comments = py$num_comments,
    date = as.POSIXct(py$date, origin = "1970-01-01", tz = "UTC"),
    link = py$link,
    subreddit = sub
  )
  
  return(subreddit_df)
  
}

# readr::write_csv(subreddit_df, "subreddit_urls.csv")



