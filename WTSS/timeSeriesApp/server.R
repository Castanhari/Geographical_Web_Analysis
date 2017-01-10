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
      get_server <- reactive (
        {  WTSS(parse_URL()["server"])  }
      )
      
      # Get coverage
      get_coverage <- reactive (
        {  cov = parse_URL()["cov"]
            if(nchar(cov) > 0)
            {  describeCoverage(get_server(), cov)  }
         }
      )
      
      # Format dates according they are in the coverage
      formatted_dates <- reactive (
        {  args = parse_URL()
            dates = c(args["start"], args["end"])
            coverage = get_coverage()
            if(length(coverage) > 0)
            {  cov = get(args["cov"], coverage)
               if(class(cov) != "try-error")
               {  cov_temporal = cov$geo_extent$temporal
                  dates[1] = substr(args["start"], 1, nchar(cov_temporal$start))
                  dates[2] = substr(args["end"], 1, nchar(cov_temporal$end))
               }
            }
            dates
         }
      )
      
      # Get time series
      get_ts <- reactive (
         {  args = parse_URL()
            timeSeries(get_server(), args["cov"], args["attrs"], latitude=args['y'], longitude=args['x'], start=formatted_dates()[1], end=formatted_dates()[2])
         }
      )
      
      # Update time series option
      observeEvent (
        session$clientData,
         {  option = parse_URL()["option"]
            if(is.na(option))
            {  option = "time_series"  }
            items=c("time_series", "bfast01(time_series)", "bfast(time_series)", "bfastmonitor(time_series)", "twdtw(time_series)")
            updateSelectInput(session, "option", "Time Series options", items, selected=option)
         }
      )
      
      # Send time series option to a custom message handler, get a time series and show the result
      observeEvent (
        input$option,
         {  session$sendCustomMessage("KVP_handler", paste("&option=", input$option, sep=''))
            
            if(input$option != '')
            {  time_series = tryCatch (
                  {  get_attributes(get_ts())  },
                  error = function(e) {  NULL  }
               )
               if(is.null(time_series))
               {  output$result_interface <- renderUI(textOutput("result"))
                  output$result <- renderText({"There is no data for the selected parameters. Please, try to get a time series with other parameters."})
                  print("nao deu")
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
                        {  plot(series, type = "classification", overlap=0.5)  }
                        else
                        {  plot(series)  }
                     }
                  )
                  print(paste("input$option", input$option))
               }
            }
         }
      )
      
   }
)

# server=http://www.dpi.inpe.br/tws/wtss&cov=mod13q1_512&attrs=ndvi&y=-10&x=-54&start=2000-02-18&end=2016-01-01&option=time_series

