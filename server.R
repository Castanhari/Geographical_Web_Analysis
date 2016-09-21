library(shiny)
library(devtools)
library(wtss.R)
source("helpers.R")

shinyServer
(  function(input, output, session)
   {  # Get server
      get_server <- reactive (
        {  WTSS(input$server)  }
      )
      
      # Load coverages
      observeEvent (
        input$server,
         {  coverages = listCoverages(get_server())
            updateSelectInput(session, "coverage", "Coverage", coverages)
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
            {  attrs = coverage[[input$coverage]]$attributes$name
               updateSelectInput(session, "attributes", choices=attrs, selected=attrs[1])
               
               if(nchar(input$location) == 0)
               {  cov_spatial = coverage[[input$coverage]]$geo_extent$spatial$extent
                  xmid = paste("x=", (cov_spatial$xmin + cov_spatial$xmax)/2, sep='')
                  ymid = paste("y=", (cov_spatial$ymin + cov_spatial$ymax)/2, sep='')
                  updateTextInput(session, "location", "Location", paste(xmid, ymid, sep='&'))
               }
               
               cov_temporal = coverage[[input$coverage]]$geo_extent$temporal
               updateDateRangeInput(session, "dates", "Date range", cov_temporal$start, cov_temporal$end, min=cov_temporal$start, max=cov_temporal$end)
            }
         },
         priority=99
      )
      
      # Parse URL
      parse_URL <- function ()
      {  #args = "cov=mod13q1_512&attr=ndvi,evi&x=-53.495&y=-10.408&start=2001-01-01&end=2016-01-01"
         args = substr(session$clientData$url_search, 2, nchar(session$clientData$url_search))
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
      
      # Update coverage, attributes, location and dates from URL
      observeEvent (
        input$coverage,
         {  args = parse_URL()
            covs = listCoverages(get_server())
            if(!is.na(match(args["cov"], covs)))
            {  updateSelectInput(session, "coverage", "Coverage", covs, selected=args["cov"])  }
            
            coverage = get_coverage()
            if(length(coverage) > 0)
            {  cov_attrs = coverage[[input$coverage]]$attributes$name
               URL_attrs = strsplit(args["attr"], ',')[[1]]
               if(!(NA %in% match(URL_attrs, cov_attrs)))
               {  updateSelectInput(session, "attributes", choices=cov_attrs, selected=URL_attrs)  }
               
               cov_temporal = coverage[[input$coverage]]$geo_extent$temporal
               if(!is.na(args["start"]) && !is.na(args["end"]) &&
                  difftime(args["start"], cov_temporal$start, units="days") >= 0 &&
                  difftime(cov_temporal$end, args["end"], units="days") >= 0)
               {  updateDateRangeInput(session, "dates", "Date range", as.Date(args["start"]), as.Date(args["end"]))  }
            }
            
            if(!is.na(args['x']) && !is.na(args['y']))
            {  updateTextInput(session, "location", "Location", paste(paste("x=", args['x'], sep=''), paste("y=", args['y'], sep=''), sep='&'))  }
         },
         priority=98
      )
      
      # Get time series
      get_ts <- eventReactive (
        input$ts_button,
         {  args = parse_URL()
            #timeSeries(get_server(), args["cov"], args["attr"], latitude=args["y"], longitude=args["x"], start=args["start"], end=args["end"])
            
            x = substr(input$location, 3, regexpr('&', input$location)-1)
            y = substr(input$location, regexpr('&', input$location)+3, nchar(input$location))
            timeSeries(get_server(), input$coverage, input$attributes, latitude=y, longitude=x, start=input$dates[1], end=input$dates[2])
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
      
      # Get selected option
      selected_option <- reactive (
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
      
      output$result <- renderPrint (
        selected_option()  )
        
      output$plot <- renderPlot (
        {  series = selected_option()
            if(input$series == "twdtw(time_series)")
            {  plot(series, type="alignments")  }
            else
            {  plot(series)  }
         }
      )
      
   }
)
