# load the data.table package
library(data.table)

# Import potatoes.csv with fread(): potatoes
potatoes <- fread("potatoes.csv")


# Import columns 6 and 8 of potatoes.csv: potatoes
potatoes<-fread("potatoes.csv",select=c(6,8))