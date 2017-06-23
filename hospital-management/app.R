#'@author Divakant Pandey
#'
#'@title Sample MultiPage Application using R Shiny
#'
#'@Description Source Hospital Data
#'

#setwd("~/Documents/R/multi-page")

#Load Shiny, Themes and Dashboard, DT for Data Table in UI, data.table for loading files
library(plyr)
library(ggplot2)
library(plotrix)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(plotly)

library(data.table)

# Always load dplyr after plyr or refer API with pkgname::func
library(dplyr)


source("DataIngestion.R")
source("Dashboard.R")

# Define UI for Multi page application, It has a Data Table, File Uploader, Histogram and Dashboard
ui <- shinyUI(
  navbarPage(
    "Kalyani Knee & Shoulder Clinic",
    theme =  shinytheme("cerulean"),
    #shinythemes::themeSelector(),
    
    fluid  = TRUE,
    tabPanel("Sales DashBoard",
             dashboardPage(header, sidebar, body),
             h5("Copr.Divakant Pandey, email : diva.miet.cse@gmail.com")),
    
    tabPanel(
      "Tabular Reports",
      #dashboardPage(tabularHeader, tabularSidebar, tabularBody)
      # sidebarLayout(
      #   sidebarPanel(
      #     selectInput("dataset", "Choose a dataset:", 
      #                 choices = c("rock", "pressure", "cars"))
      #      ,downloadButton('downloadData', 'Download')
      #   ),
      #   mainPanel(
          tabBox(width = 9,
            tabPanel("Sales Turnover Calculation", dataTableOutput("salesPercent")),
            tabPanel("Sales per City", dataTableOutput("salesPerCity")),
            tabPanel("Sales per Issue", dataTableOutput("summarisedSalesByIssueNYr")),
            tabPanel("Sales per issue By City", dataTableOutput("summarisedSalesByIssueNCity")),
            tabPanel("Issue By Age", dataTableOutput("grpByAge"))
        )
      
    ),
    tabPanel('OPD',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars',
          'Columns Filter:',
          names(OPD),
          selected = names(OPD)
        )
      ),
      mainPanel(DT::dataTableOutput('OPD'))
    )),
    tabPanel('Xray',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars1',
          'Columns Filter:',
          names(Xray),
          selected = names(Xray)
        )
      ),
      mainPanel(DT::dataTableOutput('Xray'))
    )),
    tabPanel('Procedure',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars4',
          'Columns Filter:',
          names(Procedure),
          selected = names(Procedure)
        )
      ),
      mainPanel(DT::dataTableOutput('Procedure'))
    )),
    tabPanel('IPD',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars2',
          'Columns Filter:',
          names(IPD),
          selected = names(IPD)
        )
      ),
      mainPanel(DT::dataTableOutput('IPD'))
    )),
    tabPanel('Cashbook',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars3',
          'Columns Filter:',
          names(Cashbook),
          selected = names(Cashbook)
        )
      ),
      mainPanel(DT::dataTableOutput('Cashbook'))
    )),
    tabPanel('PRO',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars5',
          'Columns Filter:',
          names(Pro),
          selected = names(Pro)
        )
      ),
      mainPanel(DT::dataTableOutput('Pro'))
    ))
  )
)

# Define server logic
server <- shinyServer(function(input, output) {
  output$IPD <-
    DT::renderDataTable(DT::datatable(IPD[, input$show_vars2, drop = FALSE],
                                      options = list(pageLength = 25)))
  output$OPD <-
    DT::renderDataTable(DT::datatable(OPD[, input$show_vars, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Xray <-
    DT::renderDataTable(DT::datatable(Xray[, input$show_vars1, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Cashbook <-
    DT::renderDataTable(DT::datatable(Cashbook[, input$show_vars3, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Pro <-
    DT::renderDataTable(DT::datatable(Pro[, input$show_vars5, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Procedure <-
    DT::renderDataTable(DT::datatable(Procedure[, input$show_vars4, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  
  # fluid row 1, kpi 1: Patient Visited
  output$patientVisited <- renderValueBox({
    valueBox(
      formatC(
        totalPatientVisited$n,
        format = "d",
        big.mark = ','
      )
      ,
      "Total Patient Visited"
      ,
      icon = icon("user")
      ,
      color = ifelse(
        totalPatientVisited$n <= 5,
        "red",
        ifelse(totalPatientVisited$n <= 20, "yellow", "green")
      )
    )
    
  })
  
  # fluid row 1, kpi 2: Total Sales For Current Year
  output$totalSalesForCurrentYear <- renderValueBox({
    valueBox(
      paste0(maxSalesforCurrentYear$sum)
      ,
      paste("Total Sales for ", maxSalesforCurrentYear$year)
      ,
      icon = icon("inr")
      ,
      color = ifelse(
        maxSalesforCurrentYear$sum <= 500,
        "red",
        ifelse(maxSalesforCurrentYear$sum <= 800, "yellow", "green")
      )
    )
  })
  
  # fluid row 1, kpi 3: profit margin
  output$salesTurnover <- renderValueBox({
    valueBox(
      paste0(round(salesTurnover, digits = 2), "%")
      ,
      "Sales Turnover"
      ,
      icon = icon("pie-chart")
      ,
      color = ifelse(
        salesTurnover <= 50,
        "red",
        ifelse(salesTurnover <= 60, "yellow", "green")
      )
    )
  })
  
  #Dashboard 1 sales by region quarter bar graph
  output$salesQuartBar <- renderPlot({
    # summarisedSalesByCity_sub<-subset(summarisedSalesByCity,summarisedSalesByCity$city==input$select_City)
    
    ggplot(data = summarisedSalesByCity, aes(
      x = factor(year),
      y = sum,
      group = city,
      color = city
    )) + geom_line(size = 1.5,
                   position = "dodge",
                   stat = "identity") + geom_point(size = 3, fill = "white") +
      scale_shape_manual(values = c(22, 21)) + ylab("Sales By City") + xlab("Year") +
      labs(fill = "City Legend := ") +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Sales Trend By City")
    
  })
  
  #Dashboard 2 sales per issue by year and month
  output$salesYearBar <- renderPlot({
    summarisedSalesByIssue_sub <-
      subset(summarisedSalesByIssue,
             summarisedSalesByIssue$issue == input$select_Issue)
    
    ggplot(data = summarisedSalesByIssue_sub, aes(
      x = year,
      y = sum,
      fill = factor(month)
    )) + geom_bar(position = "stack", stat = "identity") + coord_flip() + xlab("Year") + theme(legend.position = "bottom",
                                                                                               plot.title = element_text(size = 15, face = "bold")) + ggtitle("Sales By Issue") + labs(fill = "Month Legend := ")
    
    
  })
  
  #Dashboard 3
  output$shareLine <- renderPlot({
    ggplot(data = grpByAge, aes(
      x = factor(ageBins),
      y = n,
      fill = issue
    )) +
      geom_bar(position = "dodge", stat = "identity") + ylab("Issue By Age") +
      xlab("Age Range") + theme(legend.position = "bottom"
                                ,
                                plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Issues By Age") + labs(fill = "Issue Legend := ")
    
  })
  
  #Dashboard 4
  output$quarterPie <- renderPlot({
    salesByQuarter_sub <-
      subset(salesByQuarter,
             salesByQuarter$year == input$select_Year)
    
    percentSales <-
      round((salesByQuarter_sub$n / sum(salesByQuarter_sub$n)) * 100.0, digits = 2)
    salesLabel <- paste(salesByQuarter_sub$quarter, percentSales)
    salesLabel <- paste(salesLabel, "%", sep = "")
    pie3D(
      salesByQuarter_sub$n,
      labels = salesLabel,
      explode = 0.1	,
      main = "Quarterly Sales Figure"
    )
    
    
  })
  
  # Report 1
  salesPercent = subset(salesPercent, select = -c(pid))
  setnames(salesPercent,
           c("Name", "Total Visit", "Unique Visit", "Percentage"))
  output$salesPercent <- renderDataTable(salesPercent)
  
  # Report 2
  setnames(SalesByCity,c("City","Total"))
  output$salesPerCity <- renderDataTable(SalesByCity)
  
  # Report 3
  output$summarisedSalesByIssueNYr <- renderDataTable(summarisedSalesByIssueNYr)
  
  # Report 4
  output$summarisedSalesByIssueNCity <- renderDataTable(summarisedSalesByIssueNCity)
  
  # Report 5
  output$grpByAge <- renderDataTable(grpByAge)
  
  output$copyRight <- renderInfoBox({
    h3(
      title = "Divakant Pandey"
      ,value = "Copyright Divakant Pandey , For any issues email @ diva.miet.cse@gmail.com or contact +919620075151"
      ,color = "purple"
      ,icon = icon("tachometer")
    )
  })
  
})

# Run the application
shinyApp(ui = ui, server = server)
