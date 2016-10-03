library(shiny)

shinyUI
(  ui <- fluidPage(
     titlePanel("Time Series"),
      
      plotOutput("plot")
   )
)

