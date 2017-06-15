#'@author Divakant Pandey
#'
#'@title Date Manipulation
#'

dateFrame<-as.Date(transactionData$date,"%d/%m/%Y")

# For Numeric - as.POSIXlt(transactionData$date)$wday
transactionData$day=weekdays(dateFrame) 

transactionData$month=months(dateFrame) 

transactionData$quarter=quarters(dateFrame) 

transactionData$year=year(dateFrame) 

difftime(as.Date("1/6/2017","%d/%m/%Y"),as.Date("9/6/2017","%d/%m/%Y"),units = c("days"))
