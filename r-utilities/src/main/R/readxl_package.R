# Load the readxl package
library(readxl)

dir()
# Print out the names of both spreadsheets
excel_sheets(dir())

# Read the sheets, one by one
pop_1 <- read_excel("urbanpop.xlsx", sheet = 1)
pop_2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop_3<- read_excel("urbanpop.xlsx",sheet=3)

# Put pop_1, pop_2 and pop_3 in a list: pop_list
pop_list <- list(pop_1,pop_2,pop_3)

# Display the structure of pop_list
str(pop_list)


# Read all Excel sheets with lapply(): pop_list
pop_list <- lapply(excel_sheets("urbanpop.xlsx"),read_excel,path="urbanpop.xlsx")

# Display the structure of pop_list
str(pop_list)

# Import the the first Excel sheet of urbanpop_nonames.xlsx (R gives names): pop_a
pop_a <- read_excel("urbanpop_nonames.xlsx",col_names=FALSE)

# Import the the first Excel sheet of urbanpop_nonames.xlsx (specify col_names): pop_b
cols <- c("country", paste0("year_", 1960:1966))
pop_b <- read_excel("urbanpop_nonames.xlsx",col_names=cols)

# Print the summary of pop_a
summary(pop_a)

# Print the summary of pop_b
summary(pop_b)


# Import the second sheet of urbanpop.xlsx, skipping the first 21 rows: urbanpop_sel

urbanpop_sel <- read_excel("urbanpop.xlsx",sheet=2,skip=21,col_names=FALSE)
# Print out the first observation from urbanpop_sel
head(urbanpop_sel,1)