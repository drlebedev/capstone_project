library(shiny)

page <- fluidPage(

  headerPanel("Kneser Ney word prediction"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    helpText("Enter a text you want the prediction be based on: "),
    textInput("text", label = "", placeholder = "Text"),
    br(),
    p("(c) Kirill Lebedev, 2016"),
    a("http://www.drlebedev.com")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
          "Prediction",
          h2("Most probable word"),
          strong(textOutput("top")),
          h2("Top 5 preditcions"),
          tableOutput("prediction")
      ),
      tabPanel(
          "Description",
          h2("Model description"),
          h4("Overview"),
          h4("Sources"),
          p(
            "App/Model sources are available on: ", 
            a("https://github.com/drlebedev/capstone_project")
          ),
          h4("Acknowledge"),
          p(
            "An author would like to express an aprreciation to all the authors and contributors of Data Science Specialization (",
            a("https://www.coursera.org/specializations/jhu-data-science"),
            "). The Specialization was a great opportunity to refresh and improve Data Science-related knowledge"
          )
      )
    )
  )
) 

shinyUI(page)