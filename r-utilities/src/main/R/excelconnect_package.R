# Load the XLConnect package
library(XLConnect)

# Build connection to urbanpop.xlsx: my_book
my_book <- loadWorkbook("urbanpop.xlsx")

# Print out the class of my_book
class(my_book)

# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# List the sheets in my_book
getSheets(my_book)

# Import the second sheet in my_book
readWorksheet(my_book,sheet = 2)



# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# Import columns 3, 4, and 5 from second sheet in my_book: urbanpop_sel
urbanpop_sel <- readWorksheet(my_book, sheet = 2,startCol=3,endCol=5)

# Import first column from second sheet in my_book: countries
countries <- readWorksheet(my_book,sheet=2,startCol=1,endCol=1)

# cbind() urbanpop_sel and countries together: selection
selection <- cbind(countries,urbanpop_sel)

# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# Add a worksheet to my_book, named "data_summary"
createSheet(name = "data_summary",object=my_book)

# Use getSheets() on my_book
getSheets(my_book)


# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# Add a worksheet to my_book, named "data_summary"
createSheet(my_book, "data_summary")

# Create data frame: summ
sheets <- getSheets(my_book)[1:3]
dims <- sapply(sheets, function(x) dim(readWorksheet(my_book, sheet = x)), USE.NAMES = FALSE)
summ <- data.frame(sheets = sheets,
                   nrows = dims[1, ],
                   ncols = dims[2, ])

# Add data in summ to "data_summary" sheet
writeWorksheet(object=my_book,data=summ,sheet="data_summary")

# Save workbook as summary.xlsx
saveWorkbook(my_book,file = "summary.xlsx")


# Rename "data_summary" sheet to "summary"

renameSheet(object = my_book,sheet = "data_summary",newName = "summary")
# Print out sheets of my_book
getSheets(my_book)

# Save workbook to "renamed.xlsx"
saveWorkbook(file ="renamed.xlsx",object = my_book )

# Build connection to renamed.xlsx: my_book
my_book <- loadWorkbook("renamed.xlsx")

# Remove the fourth sheet
removeSheet(object = my_book,sheet = "summary")

# Save workbook to "clean.xlsx"
saveWorkbook(object = my_book,file = "clean.xlsx")