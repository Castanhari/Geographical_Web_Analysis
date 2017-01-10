library(shiny)
library(wtss.R)

shinyServer
(  function(input, output, session)
   {  # Get server
      get_server <- reactive (
        {  WTSS(input$server)  }
      )
      
      # Load coverages from server
      observeEvent (
        input$server,
         {  coverages = listCoverages(get_server())
            updateSelectInput(session, "coverage", "Coverage", coverages, selected=coverages[2])
            
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
      
      # Load attributes, latitude, longitude and dates from coverage
      observeEvent (
        input$coverage,
         {  coverage = get_coverage()
            if(length(coverage) > 0)
            {  # Attributes
               cov_attrs = coverage[[input$coverage]]$attributes$name
               updateSelectInput(session, "attributes", choices=cov_attrs, selected=cov_attrs[1])
               
               # Latitude and Longitude
               cov_spatial = coverage[[input$coverage]]$geo_extent$spatial$extent
               cov_x = (cov_spatial$xmin + cov_spatial$xmax)/2
               cov_y = (cov_spatial$ymin + cov_spatial$ymax)/2
               updateNumericInput(session, "latitude", "Latitude", cov_y, min=cov_spatial$ymin, max=cov_spatial$ymax, step=1)
               updateNumericInput(session, "longitude", "Longitude", cov_x, min=cov_spatial$xmin, max=cov_spatial$xmax, step=1)
               
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
      
      # Update latitude and longitude from javascript
      # There is an external javascript code that sends latitude and longitude to this application doing:
      #    Shiny.onInputChange("js_lat", lat_value);
      #    Shiny.onInputChange("js_long", long_value);
      # It creates here, respectively, the variables named "js_lat" and "js_long", which are accessible by input$js_lat and input$js_long
      observeEvent (
        input$js_lat,
         {  coverage = get_coverage()
            if(length(coverage) > 0)
            {  cov_spatial = coverage[[input$coverage]]$geo_extent$spatial$extent
               updateNumericInput(session, "latitude", "Latitude", input$js_lat, min=cov_spatial$ymin, max=cov_spatial$ymax, step=1)
               updateNumericInput(session, "longitude", "Longitude", input$js_long, min=cov_spatial$xmin, max=cov_spatial$xmax, step=1)
            }
         },
         priority=98
      )
      
      # Send attributes to a custom message handler
      observeEvent (
        input$attributes,
         {  session$sendCustomMessage("attributes_handler", paste(input$attributes, collapse=','))  },
         priority=97
      )
      
      # Send latitude to a custom message handler
      observeEvent (
        input$latitude,
         {  session$sendCustomMessage("latitude_handler", input$latitude)  },
         priority=96
      )
      
      # Send longitude to a custom message handler
      observeEvent (
        input$longitude,
         {  session$sendCustomMessage("longitude_handler", input$longitude)  },
         priority=95
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
         priority=94
      )
      
      # Get time series
      observeEvent (
        input$ts_button,
         {  session$sendCustomMessage("apply_handler", '')  }
      )
      
   }
)

