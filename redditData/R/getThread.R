
source("redditData/R/secretInfo.R")

# ************************************************************
# Functions 
# ************************************************************

thread <- "init"
message("Any variable in the global environment named 'thread' has now been replaced.")

extractThread <- function(path) {
  
  get_date <- function(x) {
    as.POSIXct(x$created_utc, origin = "1970-01-01", tz = "UTC")
  }
  
  get_author <- function(x) {
    output <- x$author
    if (is.null(output)) return("[deleted]")
    as.character(output)
  }

  thread <<- paste0("https://www.reddit.com", path, ")") ## special assignment operator
  
  py_run_string("submission = reddit.submission(url = r.thread)")
  
  message("Extracting comment forest from", path, "\n")
  py_run_string("submission.comments.replace_more(limit = None)")
  py_run_string("index = slice(None)")
  
  # ************************************************************
  # Assemble data frame and edge list
  # ************************************************************
  
  root <- py$submission ## root node
  nodes <- py$submission$comments$list()  ## all nodes
  
  root_df <- tibble::tibble(
    name = root$id,
    author = as.character(root$author),
    date = get_date(root),
    children = length(root$comments[py$index]),
    descendents = length(root$comments$list()),
    text = root$selftext,
    title = root$title,
    media = root$url,
    subreddit = as.character(root$subreddit),
    path = root$permalink
  )
  
  branches_df <- tibble::tibble(
    name = map_chr(nodes, function(x) x$id),
    author = map_chr(nodes, get_author),
    date = map_dbl(nodes, get_date),
    children = map_int(nodes, function(x) length(x$replies[py$index])),
    descendents = map_int(nodes, function(x) length(x$replies$list())),
    text = map_chr(nodes, function(x) x$body),
    score = map_int(nodes, function(x) x$score),
    title = py$submission$title,
    subreddit = as.character(root$subreddit),
    path = root$permalink
  ) %>% 
    mutate(date = as.POSIXct(date, origin = "1970-01-01"))
  
  output <- vector("list", length(nodes))
  for (i in seq_along(output)) {
    output[[i]] <- list(from = nodes[[i]]$parent_id, to = as.character(nodes[[i]]))
  }

  edge_list <- bind_rows(output) %>%
    mutate(from = str_sub(from, 4))
  
  df <- full_join(root_df, branches_df, by = c("name", "author", "date", "children", "descendents", "text", "title", "subreddit", "path")) %>% 
    select(name, author, score, text, children, descendents, everything())
  
  message(paste0(py$reddit$auth$limits$used, " used requests, ", py$reddit$auth$limits$remaining, " left..."))
  
  return(list(nodes = df, edges = edge_list))
}



