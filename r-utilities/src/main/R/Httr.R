# Load the httr package
library(httr)

# Get the url, save response to resp
url <- "http://www.example.com/"

# Print resp
resp<-GET(url)
resp

# Get the raw content of resp: raw_content
raw_content<-content(resp,as = "raw")

# Print the head of raw_content
head(raw_content)

# Get the url
url <- "http://www.omdbapi.com/?apikey=ff21610b&t=Annie+Hall&y=&plot=short&r=json"

resp<-GET(url)
# Print resp
resp

# Print content of resp as text
content(resp,"text")
content(resp)

# Print content of resp
resp