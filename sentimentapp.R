library(shiny)
library(rsconnect)
library(plyr)
library(dplyr)

#ui.R

ui <- fluidPage(
  titlePanel("Word Frequency by Artist"),
  sidebarLayout(
    sidebarPanel(
        p("How often words are used to describe Rihanna, Taylor Swift, Justin Bieber, and Kendrick Lamar on Twitter"),
      wellPanel(
        textInput("text1","Enter A Word", placeholder = "enter a word"),
        submitButton("Plot")
        
      )
    ),
      
    
    mainPanel(plotOutput("final_plot"))        
        )
)


# Define server logic required to draw a histogram
library(ggplot2)

server <- function(input, output) {
  
  output$final_plot<- renderPlot({
    filtered <- total %>%
      filter(word == input$text1)
    ggplot(filtered, aes(x = artist)) +
      geom_bar(fill = "#cc15fa")
  
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


