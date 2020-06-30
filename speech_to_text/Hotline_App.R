#Load required Packages

packages <- c("dplyr", "purrr", "readr", "stringr",
              "magrittr", "stringr", "ggplot2", "shiny", "sentimentr", "shinythemes")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}

completex <- completex_text

transcripts <- rough_data_trans



ui <- fluidPage(theme = shinytheme("lumen"),
                 titlePanel("DSPG Hotline"),
                 sidebarLayout(
                   sidebarPanel(
                     
                     
                     selectInput(inputId = "call_number", label = strong("Select Call"),
                                 choices = unique(completex$Call_Number),
                                 selected = "6183"),
                     
                     selectInput(inputId = "call_number2", label = strong("Select Log"),
                                 choices = unique(transcripts$Call_Number),
                                 selected = "6183")
                     
                   ),
                   
                   mainPanel(
                     plotOutput(outputId = "lineplot"),
                     textOutput("calllog"), style = "height:900px; overflow-y: scroll;"
                   )
                 ))


server <- function(input, output){
  
  
  filtered_data <- reactive({
    filter(completex, Call_Number == input$call_number )
  })
  
  
  filtered_data2 <- reactive({
    filter(transcripts, Call_Number == input$call_number2)
  })

  
  output$lineplot <- renderPlot({
    
    ggplot(filtered_data(), 
             aes(x = call_record, y = ave_sentiment, color = currently_speaking ))+ 
      geom_line()
    
  })
  
  output$calllog <- renderText({filtered_data2() %>% select(Call_Transcript) %>% as.character()})
}


shinyApp(ui = ui, server = server)
