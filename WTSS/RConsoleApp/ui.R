library(shiny)
library(shinyAce)

shinyUI
(  fillPage
   (  tags$script(src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$link(rel="stylesheet", href="//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$script(src="handlers.js"),
      
      fillRow
      (  fillCol
         (  wellPanel
            (  id="left_panel",
               style="height:100%;",
               
               fillRow
               (  id="buttons_row",
                  height="74px",
                  flex=c(2.5, 2.5, 4, 3),
                  
                  actionButton
                  (  inputId="run_button",
                     label="Run this tab (F9)"
                  ),
                  
                  actionButton
                  (  inputId="source_button",
                     label="Import this tab (F10)"
                  ),
                  
                  div
                  (  style="height:34px; line-height:34px; padding:0px 5px; font-weight:bold; color:hsl(0, 0%, 20%); float:right;",
                     "Upload text or .R/RData/RDS"
                  ),
                  
                  fileInput
                  (  inputId="files",
                     label=NULL,
                     multiple=TRUE,
                     accept=c("text/plain", ".R", ".rds", ".RData")
                  )
               ),
               
               uiOutput("tab_panel")
            )
         ),
         
         fillCol
         (  div
            (  id="div_id",
               style="height:100%;",
            
               pre
               (  class="well",
                  style="height:100%; padding-top:0px;",
               
                  uiOutput("result")
               )
            )
         )
      )
   )
)

