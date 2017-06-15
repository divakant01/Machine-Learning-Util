#'@author Divakant Pandey
#'
#'@title Sample MultiPage Application using R Shiny
#'
#'@Description Source Hospital Data
#'

setwd("~/Documents/R/multi-page")

#Load Shiny, Themes and Dashboard, DT for Data Table in UI, data.table for loading files
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(sqldf)
library(plotly)

source("DataIngestion.R")


#Prepare Dashboard Header
dashboardHeader <- dashboardHeader(disable = FALSE)
dashboardSidebar <- dashboardSidebar(disable = FALSE, sidebarMenu(
  menuItem(
    "Histogram",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("Widgets", tabName = "widgets", icon = icon("th")),
  menuItem(
    title = "Controls",
    sliderInput("slider", "Number of observations:", 1, dim(hospital_table)[1], 50),
    solidHeader = TRUE,
    collapsible = TRUE
  )
))


#Prepare Dashboard Tabs
histogram <- tabItem(tabName = "dashboard",
                     fluidRow(
                       box(
                         plotOutput("distPlot1", height = 250),
                         solidHeader = TRUE,
                         collapsible = TRUE
                       ),
                       box(
                         plotOutput("distPlot2", height = 250),
                         solidHeader = TRUE,
                         collapsible = TRUE
                       ),
                       box(
                         plotOutput("distPlot3", height = 250),
                         solidHeader = TRUE,
                         collapsible = TRUE
                       ),
                       box(
                         plotOutput("distPlot4", height = 250),
                         solidHeader = TRUE,
                         collapsible = TRUE
                       )
                     ))

widget <- tabItem(tabName = "widgets", h2("Widgets tab content"))



tabItems <- tabItems(histogram, widget)

#Prepare Dashboard Body
dashboardBody <- dashboardBody(tabItems)

# Define UI for Multi page application, It has a Data Table, File Uploader, Histogram and Dashboard
ui <- shinyUI(
  navbarPage(
    "Hospital Name",
    theme =  shinytheme("cerulean"),
    #shinythemes::themeSelector(),
    
    fluid  = TRUE,
    tabPanel('Data',   sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput(
          'show_vars',
          'Columns Filter:',
          names(hospital_table),
          selected = names(hospital_table)
        )
      ),
      mainPanel(DT::dataTableOutput('ex1'))
    )),
    
    tabPanel("Charts", sidebarLayout(
      sidebarPanel(sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )),
      
      mainPanel(plotlyOutput("distPlot"),
                verbatimTextOutput("event")
    ))
  ),
  tabPanel(
    "DashBoard",
    dashboardPage(dashboardHeader, dashboardSidebar, dashboardBody)
  )
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  output$ex1 <-
    DT::renderDataTable(DT::datatable(hospital_table[, input$show_vars, drop = FALSE],
                                      options = list(pageLength = 25)))
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(
      inFile$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
  })
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    df = sqldf('select city,count(*) as count from hospital_table group by city limit 20')
    
    # draw the histogram with the specified number of bins
    plot_ly(
      x = df$city,
      y = df$count,
      type = 'bar',
      mode = 'markers'
    )
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  histdata <- rnorm(500) * 100
  histdata[histdata < 0] = histdata[histdata < 0] * -1
  
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <-
      seq(min(histdata), max(histdata), length.out = input$slider + 1)
    
    # draw the histogram with the specified number of bins
    barplot(histdata, bins,
            col = 'darkgray',
            border = 'white')
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <-
      seq(min(histdata), max(histdata), length.out = input$slider + 1)
    
    # draw the histogram with the specified number of bins
    hist(histdata, bins,
         col = 'darkgray',
         border = 'white')
  })
  
  output$distPlot3 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <-
      seq(min(histdata), max(histdata), length.out = input$slider + 1)
    
    # draw the histogram with the specified number of bins
    hist(histdata, bins,
         col = 'darkgray',
         border = 'white')
  })
  output$distPlot4 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <-
      seq(min(histdata), max(histdata), length.out = input$slider + 1)
    
    # draw the histogram with the specified number of bins
    hist(histdata, bins,
         col = 'darkgray',
         border = 'white')
  })
  
  
})

# Run the application
shinyApp(ui = ui, server = server)
