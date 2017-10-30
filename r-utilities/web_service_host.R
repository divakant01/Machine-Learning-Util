library(plumber)
r <- plumber::plumb(file = "r-utilities/src/main/R/web_service.R")
r$run(port=8999) 
