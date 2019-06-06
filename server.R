library(shiny)
library(ggplot2)
library(readxl)
library(colourpicker)
library(viridis)



shinyServer(function(input, output, session) {
  
  ##Load file into a data frame
  scor1 <- reactive({
    req(input$file)
    
    if (grepl(".csv", input$file$datapath, fixed = TRUE)){
      read.csv(input$file$datapath)
    }
    else if (grepl(".xlsx", input$file$datapath, fixed = TRUE)){
      data.frame(read_xlsx(input$file$datapath))
    }
    
  })
  
  
  ##Prepare a vector to be passed to the groupCheckBox
  checkC <- reactive({unique(na.omit(as.vector(as.matrix((scor1()[2:ncol(scor1())])))))})
  
  
  
  ##Define the groupCheckBox
  output$optionz <- renderUI({
    checkboxGroupInput("optionz","1) Exclude Behaviour", checkC(), selected = as.vector(as.character(checkC())))
  })
  
  
  ##Create the altered data frame that responds to the group
  ##checkBox vector
  scorSelecter <- reactive({
    
    validate(
      need(length(input$optionz) != length(checkC()), "By default all behaviours are exclueded. Please select a behaviour to be included")
    )
    data.frame(lapply(scor1(), function(x) replace(x, (x %in% input$optionz), NA)))})
  
  ##The function can't handle blank spaces, that's why I remove them for this bit of the program
  output$selectize <- renderUI({
    selectizeInput("select","2) Import behaviour to select colour", 
                   choices = gsub(" ", "_", as.list(levels(as.factor(unique(na.omit(as.character(as.vector(as.matrix((scorSelecter()[2:ncol(scor1())])))))))))), ##here lies the bug of not recognising empty spaces
                   multiple = TRUE)
  })
  
  output$colourSelecter <-  renderUI ({
    
    lev <- sort(input$select)
    
    ###Default is selected colour blind-friendly, randomly picked
    lapply(seq_along(lev), function(i) {
      colourInput(inputId = paste0("colour", lev[i]),
                  label = paste0("Choose colour for ", lev[i]),
                  value = viridis_pal(option = "plasma", begin = runif(1, 0, 1))(16),
                  allowTransparent = TRUE
                  
      )
    })
  })
  
  ## A one-dimensional geom_tile is the easiest way, here wrapped in a reactive render Plot
  ## function
  output$graph <- renderPlot({
    
    colours <-  paste0("c(", paste0("input$colour", sort(input$select), sep = " ", collapse = ", "), ")")
    colours <- eval(parse(text = colours)) ##Try to understand non-standard evaluation!
    
    validate(
      need((length(checkC()) - length(input$optionz))  == length(input$select), "Please provide a colour for every selected behaviour")
    )
    
    ##Plot the data using ggplot
    
    
    PLOT <- ggplot(scorSelecter(), aes(x = scorSelecter()[,1], y = "", fill = as.factor(scorSelecter()[2:ncol(scor1())])) ) +
      
    {if(ncol(scor1()) == 2)ylim(-0.2005, 0.2005)} +
      
    {if(ncol(scor1()) >= 2)geom_tile(aes(x = scorSelecter()[,1], y = 0, fill = as.factor(scorSelecter()[,2])), scorSelecter(), width = 0.2, height = input$width, size = 2)} + 
    {if(ncol(scor1()) >= 3)geom_tile(aes(x = scorSelecter()[,1], y = 0.125, fill = as.factor(scorSelecter()[,3])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +
    {if(ncol(scor1()) >= 4)geom_tile(aes(x = scorSelecter()[,1], y = -0.125, fill = as.factor(scorSelecter()[,4])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +
    {if(ncol(scor1()) >= 5)geom_tile(aes(x = scorSelecter()[,1], y = -0.145, fill = as.factor(scorSelecter()[,5])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +
      
      scale_fill_manual(values = colours, na.translate = FALSE) +
      
      ## Set background, name axes, name legend
      
      labs(title = "Scored Behaviour ", x = "Time(s)", y = NULL,
           fill = "Considered Behaviour") +
      
      ##Background white, I want lines along the axes
      
      
      
      theme(panel.border = element_blank(),
            axis.line.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(), 
            panel.grid = element_blank(),
            panel.grid.minor.x = element_line(size = 0.05),
            panel.grid.major.x = element_line(size = 0.05),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black"))
    
    
    PLOT
  })
  
  
  
  ##To save the finished
  output$downloadData <- downloadHandler(
    filename = function() {
      if (grepl(".csv", input$file$datapath, fixed = TRUE)){
        paste(gsub(".csv*", "", input$file), 'pdf', sep='.') 
      }
      else if (grepl(".xlsx", input$file$datapath, fixed = TRUE)){
        paste(gsub(".xlsx.*", "", input$file), 'pdf', sep='.') 
      }
    },
    content = function(file) {
      ggsave(file, device = "pdf", width=10, height=10, unit="in", dpi=600)
    }
  )
  
  
  
})


