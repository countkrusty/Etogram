library(shiny)
library(ggplot2)
library(readxl)
library(colourpicker)


shinyUI(
  
  
  pageWithSidebar(
    
    headerPanel("Visualise Behaviour"),
    
    sidebarPanel(
      
      fileInput(inputId = "file", label = "Choose an Excel (.xlsx or .csv) File", 
                multiple = FALSE, 
                accept = c("text/xlsx", "text/microsoft-excel-pen-XML-format-spreadsheet-file", ".xlsx",
                           "text/csv", "text/comma-separated-value", ".csv"),
                width = NULL, buttonLabel = "Find...",
                placeholder = "No File selected"
      ),
      uiOutput("optionz"),
      uiOutput("selectize"),
      uiOutput("colourSelecter"),
      
      sliderInput(inputId = "width", label = "width", min = 0.01, max = 0.4, value = 0.2, step = 0.01),
      # To download the finished graph
      downloadButton("downloadData", "Download")
      
    ),
    
    
    mainPanel(
      style = "position:sticky;top: 70px;
  right: 15px;",
      plotOutput("graph"), height = "600px", quoted = TRUE)
  )
)
