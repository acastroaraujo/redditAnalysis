import praw

reddit = praw.Reddit(client_id = r.client_id,
                     client_secret = r.client_secret,
                     password = r.password,
                     #redirect_uri='http://localhost:8080',
                     user_agent = 'Comment Extraction (by acastroaraujo)',
                     username = r.username)

