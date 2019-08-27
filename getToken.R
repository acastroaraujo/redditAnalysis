source("secretInfo.R")
library(httr)

# Find OAuth settings for reddit: https://github.com/reddit/reddit/wiki/OAuth2
reddit <- oauth_endpoint(
  authorize = "https://www.reddit.com/api/v1/authorize",
  access = "https://www.reddit.com/api/v1/access_token"
)

app <- oauth_app(
  appname = "acastroaraujo", 
  key = key, 
  secret = secret
)

token <- oauth2.0_token(
  reddit, app,
  scope = c("read", "modposts"),
  use_basic_auth = TRUE,
  config_init = user_agent("acastroaraujo")
)

