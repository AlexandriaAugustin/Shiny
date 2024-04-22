https://alexandriaugustin.shinyapps.io/shiny/


library(shiny)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(tools)
library(DT)
library(readr)
library(dplyr)
library(readxl)
library(rsconnect)


ui <- fluidPage (
  setBackgroundColor("#E6E6FA"),
  titlePanel(title=div('Data Visualization', tags$img(src="cool_biden.jpeg", height="100%", width="7%", align = "right"))), 
  sidebarLayout(
    sidebarPanel(
      tags$h4('User Inputs'),
      fileInput("fileupload1", "Upload your data"),
      selectInput('varx1', 'Select the x-axis variable', choices = NULL),
      selectInput('vary1', 'Select the Y-axis variable', choices = NULL),
      textInput("title1", "Write the title for the graph"),
      selectInput('graph_type1', 'Select the graph type',
                  choices = c("scatter", "bar", "box"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data",
                 tags$br(), 
                 DTOutput('table1')),
        tabPanel("Graph", plotlyOutput('graph1'))
      )
    )
    
  )
)


server <- function(input, output, session){
  
  data1 <- reactive({
    req(input$fileupload1)
    
    ext <- tools::file_ext(input$fileupload1$name)
    
    switch(ext, 
           csv = vroom::vroom(input$fileupload1$datapath, delim = ","),
           tsv = vroom::vroom(input$fileupload1$datapath, delim = "\t"),
           xlsx = read_excel(input$fileupload1$datapath),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  output$table1 <- renderDT({data1()}) 
  
  output$graph1 <- renderPlotly({
    
    data_in <- data1()
    xvars <- input$varx1
    yvars <- input$vary1
    title_in <- input$title1
    type_in = input$graph_type1
    
    plot_ly( data_in, 
             X = ~get(xvars), 
             y = ~get(yvars),
             type = type_in
             
    ) %>%
      layout(title = title_in,
             xaxis = list( title =xvars),
             yaxis = list( title = yvars)
      )
  })
  
  
  
  observeEvent(input$fileupload1, {
    
    updateSelectInput(session, 
                      'varx1', 
                      choices = names(data1())
    )
    updateSelectInput(session, 
                      'vary1', 
                      choices = names(data1())
    )
    
  })
}


shinyApp(ui, server)
