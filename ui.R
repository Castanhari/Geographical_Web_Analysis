library(shiny)

shinyUI
(  fluidPage
   (  fluidRow
      (  selectInput
         (  inputId="application",
            label="Application",
            choices=c("Time Series")
         )
      ),
      
      fluidRow
      (  column
         (  7,
            wellPanel
            (  titlePanel("Analysis"),
               
               fluidRow
               (  column
                  (  6,
                           
                     fileInput
                     (  inputId="files",
                        label="Import R files",
                        multiple=TRUE,
                        accept=c("text/plain", ".R")
                     ),
                     
                     tabsetPanel
                     (  tabPanel
                        (  "server.R",
                           br(),
                           
                           tags$textarea(id="code", rows=25, cols=55)
                        ),
                     
                        tabPanel
                        (  "ui.R",
                           br(),
                           
                           tags$textarea(id="code", rows=15, cols=40)
                        )
                     ),
                     
                     actionButton
                     (  inputId="code_button",
                        label="Run code"
                     )
                  ),
                  
                  column
                  (  6,
                     verbatimTextOutput("result")
                  )
               )
            )
         ),
         
         column
         (  5,
            wellPanel
            (  titlePanel("Time Series"),
                     
               fluidRow
               (  column
                  (  6,
                     
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
                        choices=c(),
                        multiple=TRUE
                     )
                     
                  ),
                  
                  column
                  (  6,
                     
                     textInput
                     (  inputId="location",
                        label="Location"
                     ),
                     
                     dateRangeInput
                     (  inputId="dates",
                        label="Date range"
                     ),
                     
                     br(),br(),
                     
                     actionButton
                     (  inputId="ts_button",
                        label="Get time series"
                     )
                  )
               ),
               
               fluidRow
               (  
                  selectInput
                  (  inputId="series",
                     label="Options",
                     choices=c("Time Series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)",
                        "customized R code")
                  ),
                  
                  plotOutput("plot")
               )
            )
         )
      )
   )
)
