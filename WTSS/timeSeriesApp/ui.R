library(shiny)

shinyUI
(  ui <- fluidPage(
      singleton(tags$head(tags$script(src="handlers.js"))),
      
      fillRow
      (  flex=c(NA, 1, NA, 1, 10),
         
         selectInput
         (  inputId="option",
            label="Filter",
            choices=c('', "time_series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)"),
            width="210px"
         ),
         
         div(),
         
         selectInput
         (  inputId="attributes",
            label="Attributes",
            choices=character(0),
            multiple=TRUE,
            width="155px"
         ),
         
         div(),         
         
         uiOutput("dates_interface")
      ),
      
      br(),
      
      uiOutput("result_interface")
      
   )
)
