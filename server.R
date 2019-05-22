library(shiny)
library(ggplot2)
library(readxl)
library(colourpicker)



shinyServer(function(input, output, session) {
  
  ##Load file into a data frame
  scor1 <- reactive({
              req(input$file)
              
              if (grepl(".csv", input$file$datapath, fixed = TRUE)){
                read.csv2(input$file$datapath)
              }
              else if (grepl(".xlsx", input$file$datapath, fixed = TRUE)){
                data.frame(read_xlsx(input$file$datapath))
              }
             
          })
  
  
  ##Prepare a vector to be passed to the groupCheckBox
  checkC <- reactive({unique(na.omit(as.vector(as.matrix((scor1()[2:ncol(scor1())])))))})
  #checkC <- reactive({gsub(" ", "_", checkC())}) ##erase all blank spaces, so variables can be handled internally ****NEW****
  
  
  ##Define the groupCheckBox
  output$optionz <- renderUI({
    checkboxGroupInput("optionz","Select Behaviour", checkC(), selected = as.vector(as.character(checkC())))
    })
  
  
  ##Create the altered data frame that responds to the group
  ##checkBox vector
  scorSelecter <- reactive({data.frame(lapply(scor1(), function(x) replace(x, (x %in% input$optionz), NA)))})
  
  ##The function can't handle blank spaces, that's why I remove them for this bit of the program
  output$selectize <- renderUI({
    selectizeInput("select","Select:", 
                   choices = gsub(" ", "_", as.list(levels(as.factor(unique(na.omit(as.character(as.vector(as.matrix((scorSelecter()[2:ncol(scor1())])))))))))), ##here lies the bug of not recognising empty spaces
                   multiple = TRUE)
    })

  output$colourSelecter <-  renderUI ({
    lev <- sort(input$select)
   
###Default is #FFFFFF, even though I set value differently
    lapply(seq_along(lev), function(i) {
      colourInput(inputId = paste0("colour", lev[i]),
                label = paste0("Choose colour for ", lev[i]),
                value = ##4166F5
               # palette = "limited",
              #  allowedCols = plasma(256)
              
      )
    })
  })
  
  ## A one-dimensional geom_tile is the easiest way, here wrapped in a reactive render Plot
  ## function
  output$graph <- renderPlot({
    
    colours <-  paste0("c(", paste0("input$colour", sort(input$select), sep = " ", collapse = ", "), ")")
    colours <- eval(parse(text = colours)) ##Try to understand non-standard evaluation!

    req(length(colours) == length(input$select)) #why?

    
    ##Plot the data using ggplot
    PLOT <- reactive({
      
      print(
        
        #somehow the interactive heigth setting is now muted within the expression
        ggplot(scorSelecter(), aes(x = scorSelecter()[,1], y = "", fill = as.factor(scorSelecter()[2:ncol(scor1())])) ) +
         {if(ncol(scor1()) >= 2)geom_tile(aes(x = scorSelecter()[,1], y = 0, fill = as.factor(scorSelecter()[,2])), scorSelecter(), width = 0.2, height = input$width, size = 2)} + 
          {if(ncol(scor1()) >= 3)geom_tile(aes(x = scorSelecter()[,1], y = 0.125, fill = as.factor(scorSelecter()[,3])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +
          {if(ncol(scor1()) >= 4)geom_tile(aes(x = scorSelecter()[,1], y = -0.125, fill = as.factor(scorSelecter()[,4])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +
          {if(ncol(scor1()) >= 5)geom_tile(aes(x = scorSelecter()[,1], y = -0.145, fill = as.factor(scorSelecter()[,5])), scorSelecter(), width = 0.2, height = 0.02, size = 2)} +

          scale_fill_manual(values = colours) +
          
          ## Set background, name axes, name legend
          
          labs(title = "Scored Behaviour ", x = "Time(s)", y = NULL,
               fill = "Considered Behaviour") +
          scale_y_continuous(breaks = NULL, labels = NULL) +
          ##Background white, I want lines along the axes
          
          
          theme_bw() +
          theme(panel.border = element_blank(),
                axis.line = element_line(colour = "black"))
      )
      
      
    })
    
    PLOT()
    
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
                                        ggsave(file, plot = PLOT(), device = "pdf", width=10, height=10, unit="in", dpi=600)
                                      }
                          )
    
    
    
  })
  
})  