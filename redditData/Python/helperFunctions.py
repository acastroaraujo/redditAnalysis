

def subreddit_list(name):
  output = []
  for i in reddit.redditor(name).new():
    output.append(i.subreddit)
  return(output)
