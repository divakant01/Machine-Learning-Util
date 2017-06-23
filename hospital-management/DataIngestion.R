#'@author Divakant Pandey
#'
#'@title Data Ingestion Logic
#'
#'@Description Source Hospital Data
#'

# Find all csv files in the current directory


#Combine multiple data.table to one using rbind, fread for reading files listed above
#tableData = as.data.frame(do.call(rbind, lapply(files, fread)))
#tableData=na.omit(tableData)

#IPD
files = list.files(path = "data/IPD/",pattern = "*.csv",full.names = TRUE)
IPD = as.data.frame(do.call(rbind, lapply(files, read.csv)))
IPD=unique(IPD[!apply(is.na(IPD) | IPD == "", 1, all),])
#IPD=IPD[!duplicated(as.list(IPD))]
IPD$S.NO.=NULL

#OPD
files = list.files(path = "data/OPD/",pattern = "*.csv",full.names = TRUE)
OPD = as.data.frame(do.call(rbind, lapply(files, read.csv)))
OPD=unique(OPD[!apply(is.na(OPD) | OPD == "", 1, all),])
#OPD=OPD[!duplicated(as.list(OPD))]
OPD$SRE_NO=NULL

#Procedure
files = list.files(path = "data/Procedure/",pattern = "*.csv",full.names = TRUE)
Procedure = as.data.frame(do.call(rbind, lapply(files, read.csv)))
Procedure=unique(Procedure[!apply(is.na(Procedure) | Procedure == "", 1, all),])
#Procedure=Procedure[!duplicated(as.list(Procedure))]
Procedure$Sr..No.=NULL
Procedure$X=NULL

# Xray
files = list.files(path = "data/XRay/",pattern = "*.csv",full.names = TRUE)
Xray = as.data.frame(do.call(rbind, lapply(files, read.csv)))
Xray=unique(Xray[!apply(is.na(Xray) | Xray == "", 1, all),])
#Xray=Xray[!duplicated(as.list(Xray))]
Xray$sr_no=NULL

# Cashbook
files = list.files(path = "data/Cashbook/",pattern = "*.csv",full.names = TRUE)
Cashbook = as.data.frame(do.call(rbind, lapply(files, read.csv)))
Cashbook=unique(Cashbook[!apply(is.na(Cashbook) | Cashbook == "", 1, all),])
#Cashbook=Cashbook[!duplicated(as.list(Cashbook))]

# Pro
files = list.files(path = "data/PRO/",pattern = "*.csv",full.names = TRUE)
Pro = as.data.frame(do.call(rbind, lapply(files, read.csv)))
Pro=unique(Pro[!apply(is.na(Pro) | Pro == "", 1, all),])
#Pro=Pro[!duplicated(as.list(Pro))]


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
maxSalesforCurrentYear=transactionData %>% group_by(year) %>%  dplyr::summarise(sum= sum(fees)) %>% arrange(desc(year)) %>% head(1)

visitGap = 7

countByTotalPatientVisit = select(inner_join(count(transactionData, pid), patientData, by =
                                               c("pid" = "id")), name, n)


# Count By city covered
countByCity = count(transactionData, city)

#No Days Gap between similar patient visit
countByUniquePatientVisit = transactionData %>% group_by(pid) %>% mutate(Diff =
                                                                           c(NA, abs(diff.Date(
                                                                             as.Date(date, "%d/%m/%Y")

                                                                                                                                                        )))) %>% filter(Diff > visitGap) %>% count(pid) %>% inner_join(patientData,by=c("pid"="id")) %>% select(name,n,pid)
# Report 1 - Sales Comparison based on Unique Visitors
salesPercent= countByTotalPatientVisit %>% inner_join(countByUniquePatientVisit, by=c("name"="name"),suffix=c(".a",".b")) %>% mutate(sales_percent=round((n.b/n.a)*100.0,digits=2))

# KPI - 3 - Expected Total Sales Turnover
salesTurnover= (sum(salesPercent$n.b)/sum(salesPercent$n.a))*100.0

# Report 2 - Total Sales per city
SalesByCity=transactionData %>% group_by(city) %>% dplyr::summarise(sum(fees)) 

# Total Sales per city, year, month -Dashboard 1
summarisedSalesByCity=transactionData %>% group_by(year,city) %>% dplyr::summarise(sum=sum(fees)) 

# Dashboard 4 - Total Sales per Quarter
salesByQuarter=transactionData %>% group_by(year,quarter) %>% dplyr::summarise(n=sum(fees)) 

# Report 3 - Total Sales per issue, year
summarisedSalesByIssueNYr=unique(transactionData %>% group_by(issue,year) %>% mutate(sum=sum(fees)) %>% select(issue,year,sum)) 

# Total Sales per issue, year, month - Dashboard 2
summarisedSalesByIssue=transactionData %>% group_by(issue,year,month) %>% mutate(sum=sum(fees)) %>% select(issue,year,month,sum) 

# Report 4 - Total Sales per issue, city, year, month
summarisedSalesByIssueNCity=transactionData %>% group_by(issue,city, year) %>% mutate(sum=sum(fees)) %>% select(issue,city,year,sum) 


# Dashboard 3 - Issue by Age Group
grpByAge=patientData %>% inner_join(transactionData, by=c("id"="pid")) %>% select(age,issue,year)

# Create Grouping/bins
grpByAge$ageBins=cut(x=grpByAge$age,breaks = round(seq(from=1,to=ceiling(max(grpByAge$age)),length.out = length(grpByAge$age)),digits = 0))

# Report 5 Age Range / Issues
grpByAge=grpByAge%>% count(ageBins,issue)

