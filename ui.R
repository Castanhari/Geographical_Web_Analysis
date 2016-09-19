library(shiny)

shinyUI
(  fluidPage
   (  headerPanel("Time Series"),
   
      sidebarLayout(
         sidebarPanel
         (  #selectInput
            #(  inputId="server",
            #   label="Server address",
            #   choices=c("http://www.dpi.inpe.br/tws/wtss", "http://www.dpi.inpe.br/ts/wtss")
            #),
            
            selectInput
            (  inputId="coverage",
               label="Coverage",
               choices=c("mod13q1_512")
            ),
            
            checkboxGroupInput
            (  inputId="attributes",
               label="Attributes",
               choices=c()
            ),
            
            textInput
            (  inputId="location",
               label="Location"
            ),
            
            dateRangeInput
            (  inputId="dates",
               label="Date range"
            ),
            
            actionButton
            (  inputId="ts_button",
               label="Get time series"
            ),
            
            width=3
         ),
         
         mainPanel
         (  wellPanel
            (  plotOutput("plot"),
               
               br(),
               
               selectInput
               (  inputId="series",
                  label="Options",
                  choices=c("Time Series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)",
                     "customized R code"),
                  width="400px"
               ),
               
               fileInput
               (  inputId="files",
                  label="Upload R files",
                  multiple=TRUE,
                  accept=c("text/plain", ".R")
               ),
               
               #textAreaInput
               #(  inputId="code",
               #   label="Customized R code"
               #)
               tags$textarea(id="code", rows=10, cols=100),
               
               actionButton
               (  inputId="code_button",
                  label="Run code"
               ),
               
               verbatimTextOutput("result")
            ),
            
            width="9"
         )
      )
   )
)

