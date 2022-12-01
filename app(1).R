#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  
  
  # Application title
  titlePanel("Data Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Upload .csv file only"),
      shinyjs::hidden # this hides topWords, but toggleState in server.R doesn't toggle!?!
      (
        selectInput("selected_col1","Choose attributes :: ",choices = c("A","B","C"),
                    selected = c("A","B","C"),multiple = T, width = "100%")
      )
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("sel_attri_ui"),
      br(),
      tabsetPanel(
        tabPanel("Data Table",
                 br(),
                 dataTableOutput("dto")),
        tabPanel("Summary",
                 br(),
                 verbatimTextOutput("sel_vec")),
        tabPanel("Histogram",
                 uiOutput("selected_attrb_hist"),
                 plotOutput("histo1")),
        tabPanel("Box Plot",
                 uiOutput("selected_attrb_bxply"),
                 uiOutput("selected_attrb_bxplx"),
                 plotOutput("boxplo1")),
        tabPanel("Scatter Plot",
                 uiOutput("selected_attrb_pointx"),
                 uiOutput("selected_attrb_pointy"),
                 plotOutput("sctrplo1")),
        tabPanel("Pair Plot",
                 plotOutput("pairplo1")),
        tabPanel("Violin Plot",
                 uiOutput("selected_attrb_vioply"),
                 uiOutput("selected_attrb_vioplx"),
                 plotOutput("vioplo1"))
      ),
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #dfo <- read.csv2("NBA 2018-19 Season.csv",stringsAsFactors = T,sep = ",")
  dfo <- reactive(read.csv2(input$file1$datapath,stringsAsFactors = T,sep = ","))
  
  df <- reactive(dfo()[,input$selected_col])
  
  output$dto <- renderDT({
    datatable(df(), filter = "top")
  })
  output$sel_attri_ui <- renderUI({
    selectInput("selected_col","Choose attributes :: ",choices = names(dfo()),selected = names(dfo()),multiple = T, width = "100%")
    #selectInput("iid","select",choices = c("B","c"))
  })
  output$sel_vec <- renderPrint({
    input$selected_col
    summary(df())
  })
  output$selected_attrb_hist <- renderUI({
    selectInput("hist_attb","Select Attribute for Histogram", 
                choices = names(select_if(df(),is.numeric)), multiple = F)
  })
  output$histo1 <- renderPlot({
    ggplot()+geom_histogram(aes(x=df()[,input$hist_attb]))+labs(x=input$hist_attb)
  })
  output$selected_attrb_bxplx <- renderUI({
    selectInput("bxpl_attbx","Select X variable",
                choices = names(select_if(df(),is.factor)),multiple = F)
  })
  output$selected_attrb_bxply <- renderUI({
    selectInput("bxpl_attby","Select Y variable",
                choices = names(select_if(df(),is.numeric)),multiple = F)
  })
  output$boxplo1 <- renderPlot({
    ggplot()+geom_boxplot(aes(y=df()[,input$bxpl_attby],x=df()[,input$bxpl_attbx]))+labs(x=input$bxpl_attbx, y=input$bxpl_attby)
  })
  output$selected_attrb_pointx <- renderUI({
    selectInput("plot_attbx","Select X attribute",
                choices = names(select_if(df(),is.numeric)),multiple = F)
  })
  output$selected_attrb_pointy <- renderUI({
    selectInput("plot_attby","Select Y attribute",
                choices = names(select_if(df(),is.numeric))[!names(select_if(df(),is.numeric))%in% input$plot_attbx],multiple = F)
  })
  output$sctrplo1 <- renderPlot({
    ggplot()+geom_point(aes(x=df()[,input$plot_attbx],y=df()[,input$plot_attby]))+labs(x=input$plot_attbx, y=input$plot_attby)
  })
  output$pairplo1 <- renderPlot({
    pairs(df())
  })
  output$selected_attrb_vioplx <- renderUI({
    selectInput("viopl_attbx","Select X Attribute:",
                choices = names(select_if(df(),is.factor)),multiple = F)
  })
  output$selected_attrb_vioply <- renderUI({
    selectInput("viopl_attby","Select Y Attribute:",
                choices = names(select_if(df(),is.numeric)),multiple = F)
  })
  output$vioplo1 <- renderPlot({
    ggplot()+geom_violin(aes(y=df()[,input$viopl_attby],x=df()[,input$viopl_attbx]))+labs(x=input$viopl_attbx, y=input$viopl_attby)
  })
  #----------------------------------------------------
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  #-------------------------------------------------------------
}

# Run the application 
shinyApp(ui = ui, server = server)
