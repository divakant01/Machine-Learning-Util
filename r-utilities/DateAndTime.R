# Get the current date: today
today=Sys.Date()

# See what today looks like under the hood
unclass(today)

# Get the current time: now
now= Sys.time()

# See what now looks like under the hood
unclass(now)

# Definition of character strings representing dates
str1 <- "May 23, '96"
str2 <- "2012-03-15"
str3 <- "30/January/2006"

# Convert the strings to dates: date1, date2, date3
date1 <- as.Date(str1, format = "%b %d, '%y")
date2 <- as.Date(str2, format = "%Y-%m-%d")
date3 <- as.Date(str3, format = "%d/%B/%Y")

# Convert dates to formatted strings
format(date1, "%A")
format(date2, "%d")
format(date3, "%b %Y")


# Definition of character strings representing times
str1 <- "May 23, '96 hours:23 minutes:01 seconds:45"
str2 <- "2012-3-12 14:23:08"

# Convert the strings to POSIXct objects: time1, time2
time1 <- as.POSIXct(str1, format = "%B %d, '%y hours:%H minutes:%M seconds:%S")
time2=as.POSIXct(str2)

# Convert times to formatted strings
format(time1,"%M")
format(time2,"%I:%M %p")


# day1, day2, day3, day4 and day5 are already available in the workspace

# Difference between last and first pizza day
day5-day1

# Create vector pizza
pizza <- c(day1, day2, day3, day4, day5)

# Create differences between consecutive pizza days: day_diff
day_diff=diff(pizza)

# Average period between two consecutive pizza days
mean(day_diff)

# login and logout are already defined in the workspace
# Calculate the difference between login and logout: time_online
time_online=logout-login

# Inspect the variable time_online
time_online

# Calculate the total time online
sum(time_online)

# Calculate the average time online
mean(time_online)

# Convert astro to vector of Date objects: astro_dates
astro_dates=as.Date(astro,format="%d-%b-%Y")

# Convert meteo to vector of Date objects: meteo_dates
meteo_dates=as.Date(meteo,format="%B %d, %y")

# Calculate the maximum absolute difference between astro_dates and meteo_dates
max(abs(meteo_dates-astro_dates))