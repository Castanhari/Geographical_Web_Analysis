library(shiny)
library(shinyAce)
library(shinyStore)
library(shinyTree)

shinyUI
(  fillPage
   (  tags$script(src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$link(rel="stylesheet", href="//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$script(src="handlers.js"),
      initStore("store", "RConsoleStore"),
      
      fillCol
      (  flex=c(NA, 1),

         wellPanel
         (  style="height:100%; margin:0px;",

            fillRow
            (  height="54px",
               flex=c(NA, 1, NA, 1, NA, 1, NA, 1, 6, 30),
               
               actionButton
               (  inputId="run_button",
                  label="Run (F9)"
               ),

               div(),

               actionButton
               (  inputId="source_button",
                  label="Source (F10)"
               ),

               div(),

               actionButton
               (  inputId="save_button",
                  label="Save (Ctrl+S)"
               ),

               div(),

               actionButton
               (  inputId="close_button",
                  label="Close tab"
               ),

               div(),

               fileInput
               (  inputId="files",
                  label=NULL,
                  multiple=TRUE,
                  accept=c("text/plain", ".R", ".rds", ".RData")
               ),

               div()
            )
         ),

         fillRow
         (  flex=c(2, 7, 7),
         
            fillCol
            (  wellPanel
               (  style="height:100%;",
                  uiOutput("tree_interface")
               )
            ),
            
            fillCol
            (  wellPanel
               (  id="code_panel",
                  style="height:100%;",
                  
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
)

