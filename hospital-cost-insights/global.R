#'@author Divakant Pandey
#'
#'@title Data Ingestion Logic
#'
#'@Description Source Hospital Data
#'

dataProperty.cashbook="data/Cashbook/"
dataProperty.IPD="data/IPD/"
dataProperty.OPD="data/OPD/"
dataProperty.PRO="data/PRO/"
dataProperty.Procedure="data/Procedure/"
dataProperty.Xray="data/Xray/"
            
#' Multiple Workbooks, Multiple Sheets, 
#' Combines all files inside a folder to a single data frame adn return.
getExcelData = function(directory) {
  files = list.files(
    path = directory,
    pattern = "*.xlsx",
    full.names = TRUE,
    all.files = FALSE
  )
 
  files = subset(files,!grepl("~", files))
  loadWorkBooks = lapply(files, loadWorkbook)
  readWorksheets = list()
  
  for (i in 1:length(loadWorkBooks)) {
    readWorksheets[[i]] = rbind.fill(readWorksheet(loadWorkBooks[[i]], sheet = getSheets(loadWorkBooks[[i]]),dateTimeFormat = "%d/%m/%Y"))
  }
  
  message("Reading Directory ", directory," ,Files=",files," ,Workbooks=",length(loadWorkBooks)," ,DF=",length(readWorksheets))
  excel.data <- rbind.fill(readWorksheets)
  # excel.data = unique(excel.data[!apply(is.na(excel.data) | excel.data == "", 1, all), ])
  # Unique Column
  #OPD=OPD[!duplicated(as.list(OPD))]
  
  return(excel.data)
}

fetchNonEmptyUniqueDf=function(df.data){
  tryCatch(unique(df.data[!apply(is.na(df.data) | df.data == "", 1, all), ]),error=function(err){
    return(unique(df.data))
  })
}

addDateParameters = function(df.data, colname) {
  dateFrame <- as.Date(colname, "%d/%m/%Y")
  # For Numeric - as.POSIXlt(transactionData$date)$wday
  df.data$day = weekdays(dateFrame)
  
  df.data$month = months(dateFrame)
  
  df.data$quarter = quarters(dateFrame)
  
  df.data$year = year(dateFrame)
  
  return(df.data)
}

#IPD
IPD = getExcelData(dataProperty.IPD)
IPD = fetchNonEmptyUniqueDf(IPD)#unique(IPD[!apply(is.na(IPD) | IPD == "", 1, all), ])
IPD =addDateParameters(IPD,IPD$AdmissionDate)
IPD$S.NO. = NULL
IPD_sub=IPD[,!(colnames(IPD) %in% c("year","day","month","quarter"))]

#OPD
OPD = getExcelData(dataProperty.OPD)
OPD = fetchNonEmptyUniqueDf(OPD)
OPD=addDateParameters(OPD,OPD$Date)
OPD$SRE_NO = NULL
OPD_sub=OPD[,!(colnames(OPD) %in% c("year","day","month","quarter"))]


#Procedure
Procedure = getExcelData(dataProperty.Procedure)
Procedure = fetchNonEmptyUniqueDf(Procedure)#unique(Procedure[!apply(is.na(Procedure) |Procedure == "", 1, all),])
Procedure=addDateParameters(Procedure,Procedure$Date)
Procedure$Sr..No. = NULL
Procedure$X = NULL
Procedure_sub=Procedure[,!(colnames(Procedure) %in% c("year","day","month","quarter"))]


# Xray
Xray = getExcelData(dataProperty.Xray)
Xray=fetchNonEmptyUniqueDf(Xray)
Xray=addDateParameters(Xray,Xray$Date)
Xray$sr_no = NULL
Xray_sub=Xray[,!(colnames(Xray) %in% c("year","day","month","quarter"))]


# Cashbook
Cashbook = getExcelData(dataProperty.cashbook)
Cashbook=fetchNonEmptyUniqueDf(Cashbook)
Cashbook=addDateParameters(Cashbook,Cashbook$Date)
Cashbook_sub=Cashbook[,!(colnames(Cashbook) %in% c("year","day","month","quarter"))]

# Pro
Pro = getExcelData(dataProperty.PRO)
Pro=fetchNonEmptyUniqueDf(Pro)
Pro=addDateParameters(Pro,Pro$Date)
Pro_sub=Pro[,!(colnames(Pro) %in% c("year","day","month","quarter"))]



# Dashboard - Graph 1
patientVisitByCityIPD= IPD %>% group_by(year=stringi::stri_trans_totitle(year),City=stringi::stri_trans_totitle(City)) %>% count()
patientVisitByCityIPD$type="IPD"

patientVisitByCityOPD = OPD %>% group_by(year=stringi::stri_trans_totitle(year),City=stringi::stri_trans_totitle(City)) %>% count()
patientVisitByCityOPD$type="OPD"

patientVisitByCity=na.omit(merge(patientVisitByCityIPD,patientVisitByCityOPD,all = TRUE))



# Dashboard - Graph 2

# Convert all numeric NA's to 0
Cashbook[c("OPD", "Procedure","XRAY", "IPD","Pathology", "Others","HospitalPayment")][is.na(Cashbook[c("OPD", "Procedure","XRAY", "IPD","Pathology", "Others","HospitalPayment")])] <- 0

# Get stats
cashSummary=unique( Cashbook %>% group_by(year,month) %>% mutate(OPD =sum(OPD),Procedure=sum(Procedure),XRAY =sum(XRAY),IPD=sum(IPD),Pathology =sum(Pathology),Others=sum(Others),HospitalPayment=sum(HospitalPayment)) %>% select(year,month,OPD,Procedure,XRAY,IPD,Pathology,Others,HospitalPayment))

CashSummary.col=names(cashSummary)
CashSummary.col=CashSummary.col[!CashSummary.col %in% c("year","month") ]

# Melt CashSummary DF - ie convert Columns to Row values.
cashData=na.omit(melt(cashSummary,measure.vars = CashSummary.col,na.rm = TRUE))


# Dashboard Report 3 - Issue by Age Group

IPD.age=IPD[,c("Age","Issue")] %>% count(Age,Issue=stringi::stri_trans_totitle(Issue))
OPD.age=OPD[,c("Age","Diagnosis")] %>% count(Age,Diagnosis=stringi::stri_trans_totitle(Diagnosis))

names(IPD.age)[names(IPD.age) == 'Issue'] <- 'Diagnosis'

IPD.age=na.omit(IPD.age)
OPD.age=na.omit(OPD.age)
age.data=merge(IPD.age,OPD.age,all = TRUE)
age.data=na.omit(age.data)



# # Create Grouping/bins
# age.data$ageRange = .bincode(x = age.data$Age, breaks = round(seq(
#   from = 1,
#   to = ceiling(max(age.data$Age)),
#   length.out = length(age.data$Age)
# ), digits = 0))
# 


age.data = age.data %>% group_by(Age, Diagnosis) %>% dplyr::summarise(sum=sum(n))


# Dashboard 4 - Total Sales per Quarter

yearlySalesbyDept=cashData %>% group_by(year,month,variable) %>% dplyr::summarise(sum=sum(value))   


# KPI - 1
totalPatientVisited = count(OPD)

# KPI 2- Total Sales for Current Year
currentYr=cashData %>%arrange(desc(year)) %>% head(1) %>% select(year)
salesCurrentYR= Cashbook %>% filter(Cashbook$year==currentYr$year)

columns=colSums(salesCurrentYR[,c("OPD","Procedure","XRAY","IPD","Pathology","Others","HospitalPayment")])
maxSalesforCurrentYear=columns[["OPD"]]+columns[["Procedure"]]+columns[["XRAY"]]+columns[["IPD"]]+columns[["Pathology"]]+columns[["Others"]]-columns[["HospitalPayment"]]

# KPI - 3 - Expected Total Sales Turnover
salesColumns=colSums(Cashbook[,c("OPD","Procedure","XRAY","IPD","Pathology","Others","HospitalPayment")])
totalSales=salesColumns[["OPD"]]+salesColumns[["Procedure"]]+salesColumns[["XRAY"]]+salesColumns[["IPD"]]+salesColumns[["Pathology"]]+salesColumns[["Others"]]-salesColumns[["HospitalPayment"]]
salesTurnover=(maxSalesforCurrentYear/totalSales)*100.0

# Table 1
salesByDept= cashData %>% group_by(year,variable) %>% dplyr::summarise(sum=sum(value))

# Table 2
patientVisits=patientVisitByCity %>% select(year,City,n,type)


# Table 3 age.data
ageData=age.data %>% select(Age,Diagnosis,sum)


