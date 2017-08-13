# https URL to the swimming_pools csv file.
url_csv <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv"

# Import the file using read.csv(): pools1
pools1<- read.csv(url_csv)

# Load the readr package
library(readr)

# Import the file using read_csv(): pools2
pools2<- read_csv(url_csv)

# Print the structure of pools1 and pools2
str(pools1)
str(pools2)


### Import EXCEL Files from web

# Load the readxl and gdata package
library(readxl)
library(gdata)

# Specification of url: url_xls
url_xls <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/latitude.xls"

# Import the .xls file with gdata: excel_gdata
excel_gdata<- read.xls(url_xls)

# Download file behind URL, name it local_latitude.xls
download.file(url_xls,"local_latitude.xls")

# Import the local .xls file with readxl: excel_readxl
excel_readxl <- readxl::read_excel("local_latitude.xls")

