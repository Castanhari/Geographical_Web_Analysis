library(shiny)
library(devtools)
library(wtss.R)
source("helpers.R")

shinyServer
(  function(input, output, session)
   {  # Get server
      get_server <- reactive (
        {  WTSS("http://www.dpi.inpe.br/tws/wtss")  }#{  WTSS(input$server)  }
      )
      
      # Update coverages
      #observeEvent (
      #  input$server,
      #   {  updateSelectInput(session, "coverage", "Coverage", listCoverages(get_server()))  },
      #   priority=100
      #)
      
      # Get coverage
      get_coverage <- reactive (
        {  server = get_server()
            if(nchar(input$coverage) > 0)
            {  describeCoverage(server, input$coverage)  }
            else
            {  NULL  }
         }
      )
      
      # Load coverage values (attributes, location and dates)
      observeEvent (
        input$coverage,
         {  coverage = get_coverage();
            if(length(coverage) > 0)
            {  attrs = coverage[[names(coverage)]]$attributes$name
               updateCheckboxGroupInput(session, "attributes", choices=attrs, selected=attrs[1])
               
               if(nchar(input$location) == 0)
               {  cov_spatial = coverage[[names(coverage)]]$geo_extent$spatial$extent
                  xmid = paste("x=", (cov_spatial$xmin + cov_spatial$xmax)/2, sep='')
                  ymid = paste("y=", (cov_spatial$ymin + cov_spatial$ymax)/2, sep='')
                  updateTextInput(session, "location", "Location", paste(xmid, ymid, sep='&'))
               }
               
               cov_temporal = coverage[[names(coverage)]]$geo_extent$temporal
               updateDateRangeInput(session, "dates", "Date range", cov_temporal$start, cov_temporal$end, min=cov_temporal$start, max=cov_temporal$end)
            }
         },
         priority=99
      )
      
      # Get URL arguments
      get_arguments <- eventReactive (
        session$clientData,
         {  parseQueryString(session$clientData$url_search)  }
      )
      
      # Update location
      observeEvent (
        session$clientData,
         {  arguments <- get_arguments()
            if(length(arguments) > 0)
            {  arg_str = paste(names(arguments), arguments, sep='=', collapse='&')
               updateTextInput(session, "location", "Location", arg_str)
            }
         },
         priority=98
      )
      
      # Get time series
      get_ts <- eventReactive (
        input$ts_button,
         {  x = substr(input$location, 3, regexpr('&', input$location)-1)
            y = substr(input$location, regexpr('&', input$location)+3, nchar(input$location))
            timeSeries(get_server(), names(get_coverage()), input$attributes, latitude=y, longitude=x, start=input$dates[1], end=input$dates[2])
         }
      )
      
      # Upload R files
      observeEvent (
        input$files,
        {  if(!is.null(input$files))
            {  for(i in 1:length(input$files[,1]))
               {  source(input$files[[i, 'datapath']])  }
            }
         }
      )
      
      # Execute customized R code
      run_code <- eventReactive (
        input$code_button,
         {  eval(parse(text=input$code))  }
      )
      
      output$plot <- renderPlot (
        {  time_series = NULL
            if(input$series != "customized R code")
            {  time_series = get_attributes(get_ts())  }
            series <- switch (
              input$series,
               "Time Series" = time_series,
               "bfast01(time_series)" = apply_bfast01(time_series),
               "bfast(time_series)" = apply_bfast(time_series),
               "bfastmonitor(time_series)" = apply_bfastmonitor(time_series),
               "twdtw(time_series)" = apply_twdtw(time_series),
               "customized R code" = run_code()
            )
            if(input$series == "twdtw(time_series)")
            {  plot(series, type="alignments")  }
            else
            {  plot(series)  }
         }
      )
      
      # Update text result
      output$result <- renderPrint (
        {  time_series = NULL
            if(input$series != "customized R code")
            {  time_series = get_attributes(get_ts())  }
            series <- switch (
              input$series,
               "Time Series" = time_series,
               "bfast01(time_series)" = apply_bfast01(time_series),
               "bfast(time_series)" = apply_bfast(time_series),
               "bfastmonitor(time_series)" = apply_bfastmonitor(time_series),
               "twdtw(time_series)" = apply_twdtw(time_series),
               "customized R code" = run_code()
            )
            series
         }
      )
      
   }
)

