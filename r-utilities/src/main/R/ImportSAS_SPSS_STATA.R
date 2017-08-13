# Load the haven package
library(haven)

# Import sales.sas7bdat: sales
sales <-read_sas("sales.sas7bdat")

# Display the structure of sales
str(sales)

# Import the data from the URL: sugar
sugar <- read_dta("http://assets.datacamp.com/production/course_1478/datasets/trade.dta")

# Structure of sugar
str(sugar)

# Convert values in Date column to dates
sugar$Date<-as.Date(as_factor(sugar$Date))

# Structure of sugar again
str(sugar)

# Import person.sav: traits
traits <- read_sav("person.sav")

# Summarize traits
summary(traits)

# Print out a subset
subset(traits,Extroversion  >40 & Agreeableness> 40)


url<-'http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/employee.sav'
# Import SPSS data from the URL: work
work <-read_sav(url)

# Display summary of work$GENDER
summary(work$GENDER)


# Convert work$GENDER to a factor
work$GENDER<-as_factor(work$GENDER)


# Display summary of work$GENDER again
summary(work$GENDER)
