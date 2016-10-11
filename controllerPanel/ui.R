library(shiny)

shinyUI
(  ui <- fluidPage(
     singleton(tags$head(tags$script(src="handlers.js"))),
      
      fluidRow
      (  column
         (  2,
            wellPanel
            (  titlePanel("Time Series"),
               
               selectInput
               (  inputId="server",
                  label="Server address",
                  choices=c("http://www.dpi.inpe.br/tws/wtss", "http://www.dpi.inpe.br/ts/wtss")
               ),
               
               selectInput
               (  inputId="coverage",
                  label="Coverage",
                  choices=character(0)
               ),
               
               selectInput
               (  inputId="attributes",
                  label="Attributes",
                  choices=character(0),
                  multiple=TRUE
               ),
               
               numericInput
               (  inputId="latitude",
                  label="Latitude",
                  value=NULL
               ),
               
               numericInput
               (  inputId="longitude",
                  label="Longitude",
                  value=NULL
               ),
               
               uiOutput("dates_interface"),
                           
               actionButton
               (  inputId="ts_button",
                  label="Get time series"
               )
            )
         )
      )
   )
)
