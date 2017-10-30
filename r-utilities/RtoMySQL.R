# Load the DBI package
library(DBI)

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Get table names
table_names <- dbListTables(con)

# Import all tables
tables <- lapply(dbListTables(con),dbReadTable, conn = con)

# Print out tables
tables

# Sample Query
# Create data frame short
short <- dbGetQuery(con,"select id,name from users where char_length(name)<5")

# Print short
short