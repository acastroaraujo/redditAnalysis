## Call this script from inside downloadURLs.R 

subreddit = reddit.subreddit("all")

top = subreddit.search(r.keyword, limit = None, sort = "top") ## maximum limit
date = []
post_score = []
num_comments = []
title = []
link = []
author = []

for submission in top:
  title.append(submission.title)
  post_score.append(submission.score)
  num_comments.append(submission.num_comments)
  author.append(submission.author)
  date.append(submission.created_utc)
  link.append(submission.permalink)
