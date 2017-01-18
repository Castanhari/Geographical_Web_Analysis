library(shiny)
library(wtss.R)
source("helpers.R")

shinyServer
(  function(input, output, session)
   {  # Parse URL
      parse_URL <- function()
      {  args = substr(session$clientData$url_search, 2, nchar(session$clientData$url_search))
         aux = unlist(strsplit(args, '&'))
         titles = c()
         values = c()
         for(i in 1:length(aux))
         {  pair = unlist(strsplit(aux[i], '='))
            titles[i] = pair[1]
            values[i] = pair[2]
         }
         names(values) = titles
         values
      }
      
      # Get server
      get_server <- reactive(WTSS("http://www.dpi.inpe.br/tws/wtss"))
      
      # Get coverage
      get_coverage <- reactive(describeCoverage(get_server(), "mod13q1_512"))
      
      # Format dates according they are in the coverage
      formatted_dates <- reactive(
         {  dates = c(as.character(input$dates[1]), as.character(input$dates[2]))
            coverage = get_coverage()
            if(length(coverage) > 0)
            {  cov_temporal = coverage[[names(coverage)]]$geo_extent$temporal
               dates[1] = substr(input$dates[1], 1, nchar(cov_temporal$start))
               dates[2] = substr(input$dates[2], 1, nchar(cov_temporal$end))
            }
            dates
         }
      )
      
      # Get time series
      get_ts <- reactive(
         {  args = parse_URL()
            dates = formatted_dates()
            timeSeries(get_server(), names(get_coverage()), input$attributes, lat=args['y'], long=args['x'], start=dates[1], end=dates[2])
         }
      )
      
      # Load attributes and dates from coverage
      load_attrs_dates <- reactive(
         {  coverage = get_coverage()
            if(length(coverage) > 0)
            {  cov_name = names(coverage)
               
               # Attributes
               cov_attrs = coverage[[cov_name]]$attributes$name
               updateSelectInput(session, "attributes", choices=cov_attrs, selected=cov_attrs[1])
               
               # Dates
               cov_temporal = coverage[[cov_name]]$geo_extent$temporal
               output$dates_interface <- renderUI(
                  {  if(nchar(cov_temporal$start) == 10)
                     {  dateRangeInput("dates", "Date range", format="yyyy-mm-dd")  }
                     else
                     {  dateRangeInput("dates", "Date range", format="yyyy-mm")  }
                  }
               )
               updateDateRangeInput(session, "dates", "Date range", cov_temporal$start, cov_temporal$end, min=cov_temporal$start, max=cov_temporal$end)
            }
         }
      )
      
      # Trigger the attributes and dates load at the application launch
      observeEvent(get_server(), load_attrs_dates())
      
      # Update time series option
      observeEvent (
        session$clientData,
         {  option = parse_URL()["option"]
            if(is.na(option))
            {  option = "twdtw(time_series)"  }
            items=c("time_series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)")
            updateSelectInput(session, "option", "Filter", items, selected=option)
         }
      )
      
      # Send time series option to a custom message handler, get a time series and show the result
      observe(
         {  session$sendCustomMessage("KVP_handler", paste("&option=", input$option, sep=''))
            
            if(input$option != '')
            {  time_series = tryCatch (
                  {  get_attributes(get_ts())  },
                  error = function(e) {  NULL  }
               )
               if(is.null(time_series))
               {  output$result_interface <- renderUI(textOutput("result"))
                  output$result <- renderText({"There is no data for the selected parameters. Please, try to get a time series with other parameters."})
               }
               else
               {  series <- switch (
                    input$option,
                     "time_series" = time_series,
                     "bfast01(time_series)" = apply_bfast01(time_series),
                     "bfast(time_series)" = apply_bfast(time_series),
                     "bfastmonitor(time_series)" = apply_bfastmonitor(time_series),
                     "twdtw(time_series)" = apply_twdtw(time_series)
                  )
                  output$result_interface <- renderUI(plotOutput("result"))
                  output$result <- renderPlot (
                     {  if(input$option == "twdtw(time_series)")
                        #{  plot(series, type = "alignments")  }
                        {  plot(series, type = "classification", overlap=0.5)  }
                        else
                        {  plot(series,
                              main=paste0("Pixel Center Coordinates Time-Series (lat ", get_ts()[[1]]$center_coordinate$latitude,
                              ", long ", get_ts()[[1]]$center_coordinate$longitude, ")"))
                        }
                     }
                  )
               }
            }
         }
      )
      
   }
)
