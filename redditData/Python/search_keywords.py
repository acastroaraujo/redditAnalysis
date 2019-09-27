
subreddit = reddit.subreddit(r.sub)

sub_list = []
url_list = []
n_comments = []
titles = []
location = []
subCount = 0

for q in r.q:
  print("searching for", q)
  for submission in subreddit.search(q, sort='new', limit=500):
    sub_list.append(submission.id)
    url_list.append(submission.url)
    n_comments.append(submission.num_comments)
    titles.append(submission.title)
    location.append(submission.subreddit)
    subCount+=1


print(str(subCount) + " submissions have added to list")
print("1st entry is:")
print(reddit.submission(id=str(sub_list[0])).title)
print("Last entry is:")
print(reddit.submission(id=str(sub_list[subCount-1])).title)
