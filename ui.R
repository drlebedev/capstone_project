library(shiny)

page <- fluidPage(

  headerPanel("Kneser Ney smoothing-based word prediction model"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    helpText("Enter a text you want the prediction be based on: "),
    textInput("text", label = "", placeholder = "Text"),
    br(),
    p("(c) Kirill Lebedev, 2016"),
    a("http://www.drlebedev.com", href="http://www.drlebedev.com")
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
          h4("Overview"),
          p(
            "Next word language prediction model is based on 3-gram concept (",
            a("https://en.wikipedia.org/wiki/Language_model", href="https://en.wikipedia.org/wiki/Language_model"),
            "). Model is using Kneser-Ney smoothing (",
            a("http://www.ee.columbia.edu/~stanchen/papers/h015a-techreport.pdf", href="http://www.ee.columbia.edu/~stanchen/papers/h015a-techreport.pdf"), 
            ") to achieve better performance."
          ),
          p(
            "Model is persisted as RData file and available in source repository."
          ),
          p(
            "Model is built on Data downloaded from ", 
            a("http://www.corpora.heliohost.org", href="http://www.corpora.heliohost.org"), 
            " and includes 2% random sample of blogs, tweets amd news articles. ", 
            "Model has profanity words filtering."
          ),
          h4("Sources"),
          p(
            "App/Model sources are available on: ", 
            a("https://github.com/drlebedev/capstone_project", href="https://github.com/drlebedev/capstone_project")
          ),
          h4("Acknowledge"),
          p(
            "An author would like to express an appreciation to all the authors and contributors of Data Science Specialization (",
            a("https://www.coursera.org/specializations/jhu-data-science", href="https://www.coursera.org/specializations/jhu-data-science"),
            "). The Specialization was a great opportunity to refresh and improve Data Science-related knowledge"
          )
      )
    )
  )
) 

shinyUI(page)