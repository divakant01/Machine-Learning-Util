library(stringr)

# Trim all leading and trailing whitespace
str_trim(c("   Filip ", "Nick  ", " Jonathan"))

# Pad these strings with leading zeros
str_pad(c("23485W", "8823453Q", "994Z"),width=9,side="left",pad="0")


# Make states all uppercase and save result to states_upper
states_upper <- toupper(states)

# Make states_upper all lowercase again
tolower(states_upper)

## stringr has been loaded for you

# Look at the head of students2
head(students2)

# Detect all dates of birth (dob) in 1997
str_detect(students2$dob,pattern = "1997")

# In the sex column, replace "F" with "Female"...
students2$sex <- str_replace(students2$sex,pattern = "F",replacement = "Female")

# ...And "M" with "Male"
students2$sex <- str_replace(students2$sex,pattern = "M",replacement = "Male")

# View the head of students2
head(students2)