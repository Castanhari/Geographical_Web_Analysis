library(shiny)
library(wtss.R)
source("helpers.R")

ui <- fluidPage(
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
            
            textInput
            (  inputId="location",
               label="Location"
            ),
            
            uiOutput("dates_interface"),
                        
            actionButton
            (  inputId="ts_button",
               label="Get time series"
            ),
            
            br(), br(),
            
            selectInput
            (  inputId="series",
               label="Options",
               choices=c("Time Series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)")
            ),
            
            plotOutput("plot")
         )
      )
   )
)

server <- function(input, output, session)
{  # Get server
   get_server <- reactive (
     {  WTSS(input$server)  }
   )
   
   # Load coverages from server
   observeEvent (
     input$server,
      {  coverages = listCoverages(get_server())
         updateSelectInput(session, "coverage", "Coverage", coverages, selected=coverages[1])
         
         session$sendCustomMessage("server_handler", input$server)
      },
      priority=100
   )
   
   # Get coverage
   get_coverage <- reactive (
     {  if(nchar(input$coverage) > 0)
         {  describeCoverage(get_server(), input$coverage)  }
      }
   )
   
   # Load attributes, location and dates from coverage
   observeEvent (
     input$coverage,
      {  coverage = get_coverage()
         if(length(coverage) > 0)
         {  # Attributes
            cov_attrs = coverage[[input$coverage]]$attributes$name
            updateSelectInput(session, "attributes", choices=cov_attrs, selected=cov_attrs[1])
            
            #Location
            cov_spatial = coverage[[input$coverage]]$geo_extent$spatial$extent
            cov_x = (cov_spatial$xmin + cov_spatial$xmax)/2
            cov_y = (cov_spatial$ymin + cov_spatial$ymax)/2
            updateTextInput(session, "location", "Location", paste(paste("x=", cov_x, sep=''), paste("y=", cov_y, sep=''), sep='&'))
            
            # Dates
            cov_temporal = coverage[[input$coverage]]$geo_extent$temporal
            output$dates_interface <- renderUI (
              {  if(nchar(cov_temporal$start) == 10)
                  {  dateRangeInput("dates", "Date range", format="yyyy-mm-dd")  }
                  else
                  {  dateRangeInput("dates", "Date range", format="yyyy-mm")  }
               }
            )
            updateDateRangeInput(session, "dates", "Date range", cov_temporal$start, cov_temporal$end, min=cov_temporal$start, max=cov_temporal$end)
            
            session$sendCustomMessage("coverage_handler", input$coverage)
         }
      },
      priority=99
   )
   
   # Send attributes to a custom message handler
   observeEvent (
     input$attributes,
      {  session$sendCustomMessage("attributes_handler", paste(input$attributes, collapse=','))  },
      priority=98
   )
   
   # Send location to a custom message handler
   observeEvent (
     input$location,
      {  x = substr(input$location, 3, regexpr('&', input$location)-1)
         y = substr(input$location, regexpr('&', input$location)+3, nchar(input$location))
         location_str = paste(paste("x=", x, sep=''), paste("y=", y, sep=''), sep='&')
         session$sendCustomMessage("attributes_handler", location_str)
      },
      priority=97
   )
   
   # Format dates according they are in the coverage
   formatted_dates <- reactive (
     {  dates = c(as.character(input$dates[1]), as.character(input$dates[2]))
         coverage = get_coverage()
         if(length(coverage) > 0)
         {  cov = get(input$coverage, coverage)
            if(class(cov) != "try-error")
            {  cov_temporal = cov$geo_extent$temporal
               dates[1] = substr(input$dates[1], 1, nchar(cov_temporal$start))
               dates[2] = substr(input$dates[2], 1, nchar(cov_temporal$end))
            }
         }
         dates
      }
   )
   
   # Send dates to custom message handlers
   observeEvent (
     input$dates,
      {  session$sendCustomMessage("start_handler", formatted_dates()[1])
         session$sendCustomMessage("end_handler", formatted_dates()[2])
      },
      priority=96
   )
   
   # Send series option to a custom message handler
   observeEvent (
     input$series,
      {  session$sendCustomMessage("series_handler", input$series)  },
      priority=95
   )
   
   # Get time series
   get_ts <- eventReactive (
     input$ts_button,
      {  x = substr(input$location, 3, regexpr('&', input$location)-1)
         y = substr(input$location, regexpr('&', input$location)+3, nchar(input$location))
         timeSeries(get_server(), input$coverage, input$attributes, latitude=y, longitude=x, start=formatted_dates()[1], end=formatted_dates()[2])
      }
   )
   
   # Get selected option
   selected_option <- reactive (
     {  time_series = get_attributes(get_ts())
         series <- switch (
           input$series,
            "Time Series" = time_series,
            "bfast01(time_series)" = apply_bfast01(time_series),
            "bfast(time_series)" = apply_bfast(time_series),
            "bfastmonitor(time_series)" = apply_bfastmonitor(time_series),
            "twdtw(time_series)" = apply_twdtw(time_series)
         )
         series
      }
   )
   
   output$plot <- renderPlot (
     {  series = selected_option()
         if(input$series == "twdtw(time_series)")
         {  plot(series, type="alignments")  }
         else
         {  plot(series)  }
      }
   )
   
}

shinyApp(ui, server)

