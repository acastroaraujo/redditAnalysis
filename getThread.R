
## https://www.reddit.com/dev/api/#GET_comments_{article}
## oauth_url <- "https://oauth.reddit.com"

source("getToken.R")


url <- "https://www.reddit.com"
ua <- user_agent("acastroaraujo")

GET(modify_url(url, path = "/api/v1/me"), ua)


path <- paste0("/r/", "datascience","/comments/", "cvo2fb/webscraping_seleniumbeautiful_soup_scrappy/")
url <- modify_url("https://www.reddit.com", path = path)

output <- GET(url, ua)


typeof(output)
jsonlite::fromJSON(content(output, "text"))


rraw <- function(path = "/api/v1/me") {
  url <- modify_url("https://www.reddit.com", path = path)
  
  resp <- GET(url, user_agent("acastroaraujo"))
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "Reddit API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    )
  )
}


rraw()
