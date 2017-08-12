#'@author Divakant Pandey
#'
#'@title Sample MultiPage Application using R Shiny
#'
#'@Description Source Hospital Data
#'

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
library(reshape2)

# Always load dplyr after plyr or refer API with pkgname::func
library(dplyr)

#library(XLConnect)
#library(properties)

#dataProperty<- read.properties("data.properties")

source("DataIngestion.R")
source("Dashboard.R")


# Define UI for Multi page application, It has a Data Table, File Uploader, Histogram and Dashboard
ui <- shinyUI(
  navbarPage(
    "Hospital Name",
    theme =  shinytheme("cerulean"),
    #shinythemes::themeSelector(),
    
    fluid  = TRUE,
    tabPanel("Sales DashBoard",
             dashboardPage(header, sidebar, body),
             h5("Copr.Divakant Pandey, email : diva.miet.cse@gmail.com")),
    
    tabPanel(
      "Tabular Reports",
      
          tabBox(width = 9,
            tabPanel("Yearly Sales by Department", dataTableOutput("salesPercent")),
            tabPanel("Patient Visit Frequency", dataTableOutput("patientVisits")),
            tabPanel("Diagnosis by Age", dataTableOutput("ageData"))
            
        )
      
    ),
    tabPanel('OPD',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars',
          'Columns Filter:',
          names(OPD_sub),
          selected = names(OPD_sub)
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
          names(Xray_sub),
          selected = names(Xray_sub)
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
          names(Procedure_sub),
          selected = names(Procedure_sub)
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
          names(IPD_sub),
          selected = names(IPD_sub)
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
          names(Cashbook_sub),
          selected = names(Cashbook_sub)
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
          names(Pro_sub),
          selected = names(Pro_sub)
        )
      ),
      mainPanel(DT::dataTableOutput('Pro'))
    ))
  )
)


# Define server logic
server <- shinyServer(function(input, output) {
  #source("DataIngestion.R")
  output$IPD <-
    DT::renderDataTable(DT::datatable(IPD_sub[, input$show_vars2, drop = FALSE],
                                      options = list(pageLength = 25)))
  output$OPD <-{
   # OPD_sub=OPD[,!(colnames(OPD) %in% c("year","day","month","quarter"))]
    
    DT::renderDataTable(DT::datatable(OPD_sub[, input$show_vars, drop = FALSE],
                                      options = list(pageLength = 25)))
  }
  
  output$Xray <-
    DT::renderDataTable(DT::datatable(Xray_sub[, input$show_vars1, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Cashbook <-
    DT::renderDataTable(DT::datatable(Cashbook_sub[, input$show_vars3, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Pro <-
    DT::renderDataTable(DT::datatable(Pro_sub[, input$show_vars5, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  output$Procedure <-
    DT::renderDataTable(DT::datatable(Procedure_sub[, input$show_vars4, drop = FALSE],
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
        totalPatientVisited$n <= 2000,
        "red",
        ifelse(totalPatientVisited$n <= 4000, "yellow", "green")
      )
    )
    
  })
  
  # fluid row 1, kpi 2: Total Sales For Current Year
  output$totalSalesForCurrentYear <- renderValueBox({
    valueBox(
      paste0(maxSalesforCurrentYear)
      ,
      paste("Overall Sales for ",currentYr$year)
      ,
      icon = icon("inr")
      ,
      color = ifelse(
        maxSalesforCurrentYear <= 1000000,
        "red",
        ifelse(maxSalesforCurrentYear <= 1500000, "yellow", "green")
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
  
  #Dashboard Chart 1 
  output$patientVisitTrend <- renderPlot({
    patientVisitByCity_sub<-subset(patientVisitByCity,patientVisitByCity$type==input$select_patient_type)

    ggplot(data = patientVisitByCity_sub, aes(
      x = factor(year),
      y = n,
      group = City,
      color = factor(City)
    )) + geom_line(size = 1.5,
                   position = "dodge",
                   stat = "identity") + geom_point(size = 3, fill = "white") +
      scale_shape_manual(values = c(22, 21)) +  labs(color='City') +ylab("Patients Visit Count") + xlab("Year") +
      labs(fill = "Year: ") +scale_x_discrete(labels = abbreviate)+
      theme(legend.position = "right",
            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Patients Visit Pattern")
     })
  
    
  #Dashboard Report 2 sales per Department by year and month
  output$salesYearBar <- renderPlot({
    summarisedSalesByDept_sub <-
      subset(cashData,
             cashData$year == input$select_Dept_Year)
    
    ggplot(data = summarisedSalesByDept_sub, aes(
      x = variable,
      y = value,
      fill = factor(month)
    )) + geom_bar(position = "stack", stat = "identity") + scale_y_continuous(labels = function(n){format(n, scientific = FALSE)})+coord_flip() + xlab("Year") + theme(legend.position = "bottom",
                                                                                               plot.title = element_text(size = 15, face = "bold")) + ggtitle("Sales By Issue") + labs(fill = "Month Legend := ")
    
    
  })
  
  #Dashboard 3
  output$shareLine <- renderPlot({
    ggplot(data = age.data, aes(
      x = Age,
      y = sum,
      #group=factor(Diagnosis),
      color=Diagnosis
    )) +geom_point()+
      geom_line(position = "dodge", stat = "identity") + ylab("Age Frequency") +
      scale_x_continuous(breaks = round(seq(min(age.data$Age), max(age.data$Age), by = 20),1)) +
      xlab("Age Range") + theme(legend.position = "bottom"
                                ,
                                plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Issues By Age") + labs(fill = "Diagnosis ")#+scale_x_discrete(labels = abbreviate)
    
  })
  
  #Dashboard 4
  output$quarterPie <- renderPlot({

    
    ggplot(data = yearlySalesbyDept, aes(
      x = factor(variable),
      y = sum,
      fill = factor(year)
    )) +
      geom_bar(position = "dodge", stat = "identity") + ylab("Total Sales") +
      xlab("Department") + theme(legend.position = "bottom"
                                ,
                                plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Yearly Department wise Sales") + labs(fill = "Year : ")
    
  })
  
  # Report 1
  salesPercent = subset(salesByDept)
  setnames(salesPercent,
           c("Year", "Department", "Sales"))
  output$salesPercent <- renderDataTable(salesPercent)
  
  # Report 2
  setnames(patientVisits,c("Year","City","Frequency","Department"))
  output$patientVisits <- renderDataTable(patientVisits)
  
  # Report 3
  setnames(ageData,c("AgeRange","Diagnosis","Frequency"))
  output$ageData <- renderDataTable(ageData)

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
