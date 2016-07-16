library(shiny)
source("model.R")

shinyServer(function(input, output) {

  p <- reactive(predict(input$text))

  output$top <- renderText({
    p()[1]
  })
  
  output$prediction <- renderTable({
    data.frame(prediction = p())
  })
  
})