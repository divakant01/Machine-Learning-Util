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
    title = "Sales per City By Year and Month"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE
    ,
    plotOutput("salesQuartBar", height = "380px")
  )
  
  #  sales by Issue
  ,
  box(
    title = "Sales per Issue By Year and Month"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE
    ,
    selectInput(
      "select_Issue",
      "Select an Issue:",
      choices = unique(summarisedSalesByIssue$issue),
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
    title = "Quarter Sales Comparison"
    ,
    status = "primary"
    ,
    solidHeader = TRUE
    ,
    collapsible = TRUE,
    selectInput(
      "select_Year",
      "Select Year",
      choices = unique(salesByQuarter$year),
      width = "150px"
    )
    ,
    plotOutput("quarterPie", height = "300px")
    
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
  # ,
  # tabPanel(title = "Sales by Quarter"
  #          , dataTableOutput("salesbyquarter"))
  # ,
  # tabPanel(title = "Prior Year Sales"
  #          , dataTableOutput("prioryearsales"))
))

# ouput for the source info
frow5 <- fluidRow(infoBoxOutput("sourceBox", width = 8)
                  ,
                  infoBoxOutput("nameBox", width = 4))


tabularHeader <- dashboardHeader(title = "Tabular Reports")

tabularSidebar <- dashboardSidebar(disable = TRUE)

tabularBody <- dashboardBody(frow4)
