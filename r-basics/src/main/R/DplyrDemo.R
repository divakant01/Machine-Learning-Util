#'@author Divakant Pandey
#'
#'@title Dplyr Demo
#'
#'@Description Sample queries for dplyr
#'

setwd("~/Documents/R/multi-page")

library(data.table)
library(dplyr)

# Find all csv files in the current directory
files = list.files(pattern = "*.csv")

#Combine multiple data.table to one using rbind, fread for reading files listed above
#tableData = as.data.frame(do.call(rbind, lapply(files, fread)))
#tableData=na.omit(tableData)

patientData = (as.data.frame(fread("patient_data.csv")))
transactionData = na.omit(as.data.frame(fread("transaction_data.csv")))

dateFrame <- as.Date(transactionData$date, "%d/%m/%Y")

# For Numeric - as.POSIXlt(transactionData$date)$wday
transactionData$day = weekdays(dateFrame)

transactionData$month = months(dateFrame)

transactionData$quarter = quarters(dateFrame)

transactionData$year = year(dateFrame)

#Table Data
tableData= transactionData %>% inner_join(patientData, by=c("pid"="id")) %>% select(name,city,state, date,issue,fees)
  
# KPI - 1
totalPatientVisited=count(transactionData)

# KPI 2- Total Sales for Current Year
maxSalesforCurrentYear=transactionData %>% group_by(year) %>%  summarise(sum= sum(fees)) %>% arrange(desc(year)) %>% head(1)

visitGap = 7

#difftime(as.Date("1/6/2017","%d/%m/%Y"),as.Date("9/6/2017","%d/%m/%Y"),units = c("days"))

 
countByTotalPatientVisit = select(inner_join(count(transactionData, pid), patientData, by =
                                               c("pid" = "id")), name, n)


# Count By city covered
countByCity = count(transactionData, city)

#No Days Gap between similar patient visit
countByUniquePatientVisit = transactionData %>% group_by(pid) %>% mutate(Diff =
                                                                           c(NA, abs(diff.Date(
                                                                             as.Date(date, "%d/%m/%Y")
                                                                           )))) %>% filter(Diff > visitGap) %>% count(pid) %>% inner_join(patientData,by=c("pid"="id")) %>% select(name,n,pid)
# Sales Comparison based on Unique Visitors
salesPercent= countByTotalPatientVisit %>% inner_join(countByUniquePatientVisit, by=c("name"="name"),suffix=c(".a",".b")) %>% mutate(sales_percent=(n.b/n.a)*100.0)

# KPI - 3 - Expected Total Sales Turnover
salesTurnover= (sum(salesPercent$n.b)/sum(salesPercent$n.a))*100.0

# Total Sales per city
SalesByCity=transactionData %>% group_by(city) %>% summarise(sum(fees)) 

# Total Sales per city, year, month
summarisedSalesByCity=transactionData %>% group_by(city,year,month) %>% mutate(sum=sum(fees)) %>% select(city,year,month,sum) 

# Total Sales per issue
SalesByIssue=transactionData %>% group_by(issue) %>% summarise(sum(fees)) 

# Total Sales per issue, year
summarisedSalesByIssueNYr=transactionData %>% group_by(issue,year) %>% mutate(sum=sum(fees)) %>% select(issue,year,sum) 

# Total Sales per issue, year, month
summarisedSalesByIssue=transactionData %>% group_by(issue,year,month) %>% mutate(sum=sum(fees)) %>% select(issue,year,month,sum) 

# Total Sales per issue, city, year, month
summarisedSalesByIssueNCity=transactionData %>% group_by(issue,city, year) %>% mutate(sum=sum(fees)) %>% select(issue,city,year,sum) 

