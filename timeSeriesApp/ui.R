library(shiny)

shinyUI
(  ui <- fluidPage(
     singleton(tags$head(tags$script(src="handlers.js"))),
     
     selectInput
      (  inputId="option",
         label="Time Series options",
         choices=c('', "time_series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)")
      ),
      
      plotOutput("plot")
   )
)
