#'@author Divakant Pandey
#'
#'@title Home Page Dashboard
#'
#'@Description Sales Reports
#'

#' Home Page Dashboards
header <- dashboardHeader(title = "Sales Overview")

sidebar <- dashboardSidebar(disable = TRUE)

frow1 <- fluidRow(
  valueBoxOutput("patientVisited")
  ,
  valueBoxOutput("totalSalesForCurrentYear")
  ,
  valueBoxOutput("salesTurnover")
)


# Sales per city by year, month
frow2 <- fluidRow(
  #  sales by City
  box(
    title = "Patient Visit by City per Year and Month"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE
    ,
    selectInput(
      "select_patient_type",
      "Select Department:",
      choices = unique(patientVisitByCity$type),
      width = "150px"
    ),
    plotOutput("patientVisitTrend", height = "300px")
  )
  
  #  sales by Department
  ,
  box(
    title = "Diagnosis Frequency"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE
    ,
    selectInput(
      "select_Dept_Year",
      "Select Year:",
      choices = unique(cashData$year),
      width = "150px"
    )
    ,
    plotOutput("salesYearBar", height = "300px")
  )
  
)

#
frow3 <- fluidRow(
  #
  box(
    title = "Issue Trend with Age Range"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE
    ,
    plotOutput("shareLine", height = "380px")
  )
  
  
  ,
  box(
    title = "Yearly Sales Comparison"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE,
    # selectInput(
    #   "select_month",
    #   "compare Sales",
    #   choices = unique(yearlySalesbyDept$month),
    #   width = "150px"
    # )
    # ,
    plotOutput("quarterPie", height = "380px")
    
  )
  
)

frow5 <- fluidRow(
  infoBoxOutput("copyRight", width = 4)
  )


body <- dashboardBody(frow1, frow2, frow3)


#' Tabular Reports Dashboards

frow4 <- fluidRow(tabBox(
  title = "Tabular Reports"
  ,
  width = 12
  ,
  id = "dataTabBox"
  ,
  tabPanel(title = "Sales by Model"
           , dataTableOutput("salesPercent"))

))

# ouput for the source info
frow5 <- fluidRow(infoBoxOutput("sourceBox", width = 8)
                  ,
                  infoBoxOutput("nameBox", width = 4))


tabularHeader <- dashboardHeader(title = "Tabular Reports")

tabularSidebar <- dashboardSidebar(disable = TRUE)

tabularBody <- dashboardBody(frow4)
