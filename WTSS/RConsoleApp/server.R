###################################################### WARNING!!! ######################################################
#
# THE FUNCTION source IS BEEN REPLACED BY ITS EQUIVALENT ONE OF THE PACKAGE base FROM THE NEW ***DEVELOPMENT VERSION R 3.4.0 (unstable)***
# THIS NEW VERSION ACCEPTS, BESIDES FILES, DIRECTLY R CODES USING THE exprs ARGUMENT
# FILE GOT FROM https://svn.r-project.org/R/trunk/src/library/base/R/source.R

source("/home/raul/Desktop/Shiny/Apps/WTSS_v14/RConsoleApp/source.R")
#source("/home/shiny/shinyServerApps/components/RConsoleApp/source.R")

########################################################################################################################

library(shiny)
library(shinyAce)
library(shinyStore)
library(shinyTree)

info =
"# To run, don't write multiline commands, like:
#    function(x, y) {
#       print(x+y)
#    }
# Only one-line commands can be run, like:
#    f = function(x, y) { print(x + y) }
#    f(1, 2)
# Running evaluates code line by line.
# To source, any R code is allowed, including multiline commands.
"

shinyServer
(  server <- function(input, output, session)
   {  tab_counter = 1
      opened_files = c(character(0))
      opened_nodes = c()#saved and opened node names
      tab_names = c("Tab_1")#names of all opened tabs (saved or not)
      env = environment()

      output$tab_panel <- renderUI(tabsetPanel(id="tab_set", create_tab("editor_1", "Tab_1", info), tabPanel('+')))
      aceAutocomplete("editor_1")$resume()#enable code auto-complete
      outputOptions(output,"tab_panel", suspendWhenHidden=FALSE)
      output$result <- renderUI("Results go here...")
      
      # Monitor "Run" button
      observeEvent(input$run_button, run_code())
      
      # Monitor F9 key
      observeEvent(input$key_F9, run_code())
      
      # Monitor "Source" button
      observeEvent(input$source_button, import_tab())
      
      # Monitor F10 key
      observeEvent(input$key_F10, import_tab())
      
      # Monitor "Save" button
      observeEvent(input$save_button, save_or_update())

      # Monitor Ctrl+S keys
      observeEvent(input$key_Ctrl_S, save_or_update())
      
      # Monitor "Close" button
      observeEvent(input$close_button, close_tab())

      # Run the code written in the text area and show its results
      run_code <- reactive (
         {  session$sendCustomMessage("close_handler", '')
            textual_result = ''
            last_plot = c('', '')#plot AND points commands (one string), id. of the HTML popup that contains the plot
            editor = input$tab_set
            if(!(input$tab_set %in% opened_nodes))
            {  editor = paste0("editor_", substr(input$tab_set, 5, nchar(input$tab_set))) }
            line = unlist(strsplit(input[[editor]], ";|\n"))
            if(length(line) > 0)
            {  withProgress(
                  message="Processing", value=0,
                  {  for(i in 1:length(line))
                     {  incProgress(1/length(line), detail=paste0("line ", i, " of ", length(line)))
                        plot_file = ''
                        plot_id = ''
                        plot_result = character(0)
                        points_result = character(0)
                        warnings = NULL
                        evaluation = withCallingHandlers(
                           tryCatch(
                              {  if(!grepl("plot", line[i]) && !grepl("points", line[i]))
                                 {  evaluation = capture.output(eval(parse(text=line[i])))  }
                                 else
                                 {  if(!grepl('#', substr(line[i], 1, regexpr("plot", line[i])-1)) && !grepl('#', substr(line[i], 1, regexpr("points", line[i])-1)))
                                    {  plot_file = tempfile(tmpdir="WWW/IMG", fileext=".svg")
                                       plot_id = substr(plot_file, regexpr("/[^/]*$", plot_file)+1, nchar(plot_file)-4)#regex gets the last '/'
                                       svg(plot_file)
                                       if(grepl("plot", line[i]) )
                                       {  plot_result = capture.output(eval(parse(text=line[i])))#once without "print" to catch the correct error if the command is invalid
                                          if(!identical(plot_result, character(0)))#character(0) is success. It is for errors that do not throw exception (ex. "plot#abc")
                                          {  stop(simpleError(paste(plot_result, collapse="\n")))  }
                                          capture.output(eval(parse(text=paste0("print(", line[i], ')'))))#again with "print" to show plots using ggplot inside loop
                                          last_plot = c(paste0("print(", line[i], ')'), plot_id)
                                       }
                                       else #points
                                       {  if(last_plot[1] != '')
                                          {  session$sendCustomMessage("removal_handler", last_plot[2])
                                             capture.output(eval(parse(text=last_plot[1])))
                                          }
                                          points_result = capture.output(eval(parse(text=line[i])))
                                       }
                                       if(!identical(points_result, character(0)))
                                       {  stop(simpleError(paste(points_result, collapse="\n")))  }
                                       dev.off()
                                       if(grepl("points", line[i]))
                                       {  last_plot = c(paste0(last_plot[1], '\n', line[i]), plot_id)  }
                                       request_plot(i, plot_file, line[i], plot_id)
                                    }
                                    evaluation = "NULL"
                                 }
                              },
                              error=function(e)
                              {  if(grepl("plot", line[i]) || grepl("points", line[i]))
                                 {  dev.off()
                                    if(grepl("points", line[i]) && last_plot[1] != '')#exception in "points", "plot" succeeded
                                    {  last_plot[2] <<- plot_id
                                       request_plot(i, plot_file, substr(last_plot[1], 7, nchar(last_plot[1])-1), plot_id)
                                    }
                                 }
                                 message = as.character(e)
                                 tags$span(style="color:red", paste0("Error", substr(message, regexpr(':', message), nchar(message)-1)))
                                 #tags$span(style="color:red", paste0("Error ", conditionMessage(e)))
                              }
                           ),
                           warning = function(w)
                           {  message = as.character(w)
                              warnings <<- tags$span(style="color:hsl(60, 100%, 25%)", paste0("Warning", substr(message, regexpr(':', message), nchar(message)-1)))
                              #warnings <<- tags$span(style="color:red", paste0("Warning ", conditionMessage(w)))
                              invokeRestart("muffleWarning")
                           }
                        )
                        textual_result = paste0(textual_result, line[i], '\n')
                        if(!identical(evaluation, character(0)) && evaluation != "NULL")
                        {  textual_result = paste0(textual_result, paste(evaluation, collapse="\n"), '\n')  }
                        if(!is.null(warnings))
                        {  textual_result = paste0(textual_result, paste(warnings, collapse="\n"), '\n')  }
                     }
                  }
               )
               output$result <- renderUI(HTML(textual_result))
            }
         }
      )
      
      # Register plot to a URL and request it
      request_plot <- function(i, plot_file, title, plot_id)
      {  URL = session$registerDataObj(name=paste0("plot_line", i), data=plot_file,
            filter=function(image_name, req)#"image_name" contains what we pass to "data" in "registerDataObj" above, in this case, "plot_file"
            {  shiny:::httpResponse(200, list("Content-Type"="image/svg+xml"),
                  readBin(image_name, "raw", file.size(image_name))
               )
            }
         )
         #URL = paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, session$clientData$url_pathname, URL)
         session$sendCustomMessage("plot_handler", c(title, URL, plot_id))
      }
      
      # Import tab content and show its objects
      import_tab <- function()
      {  withProgress(
            message="Loading objects", value=1,
            {  incProgress(0, detail="please wait...")
               editor = input$tab_set
               if(!(input$tab_set %in% opened_nodes))
               {  editor = paste0("editor_", substr(input$tab_set, 5, nchar(input$tab_set))) }
               content = import(input[[editor]])
               textual_result = paste0('source(exprs=parse(text=&lt;tab_code&gt;))\n', content, '\n')
               output$result <- renderUI(HTML(textual_result))
            }
         )
      }
      
      # Import objects from R source code
      import <- function(code)
      {  if(length(code) > 0)
         {  temp_env = new.env()
            errors = NULL
            warnings = NULL
            withCallingHandlers(
               tryCatch(
                  {  source(exprs=parse(text=code), local=temp_env)  },
                  error=function(e)
                  {  message = as.character(e)
                     errors <<- tags$span(style="color:red", paste0("Error", substr(message, regexpr(':', message), nchar(message)-1)))
                     #errors <<- tags$span(style="color:red", paste0("Error ", conditionMessage(e)))
                  }
               ),
               warning = function(w)
               {  message = as.character(w)
                  temp = tags$span(style="color:hsl(60, 100%, 25%)", paste0("Warning", substr(message, regexpr(':', message), nchar(message)-1), '\n'))
                  #temp = tags$span(style="color:hsl(60, 100%, 25%)", paste0("Warning ", conditionMessage(w), '\n'))
                  warnings <<- paste0(warnings, temp)
                  invokeRestart("muffleWarning")
               }
            )
            content = capture.output(ls.str(envir=temp_env, all.names=TRUE))
            if(length(content) == 0)
            {  content = "No objects"  }
            else
            {  content = paste(content, collapse='\n')  }
            if(!is.null(errors))
            {  content = paste0(content, '\n', errors)  }
            if(!is.null(warnings))
            {  content = paste0(content, '\n', warnings)  }
            for(object in ls(temp_env, all.names=TRUE))
            {  assign(object, get(object, temp_env), env)  }
            content
         }
      }
      
      # Upload R files and show the content in a tab and the objects in the results panel
      observeEvent (
         input$files,
         {  if(!is.null(input$files))
            {  withProgress(
                  message="Loading objects", value=1,
                  {  incProgress(0, detail="please wait...")
                     textual_result = ''
                     code_list = c(character(0))
                     for(i in 1:length(input$files[, "name"]))
                     {  repeated_file = FALSE
                        for(ii in opened_files)
                        {  if(md5sum(input$files[i, "datapath"]) == md5sum(ii))
                           {  repeated_file = TRUE
                              break
                           }
                        }
                        if(!repeated_file)
                        {  file_name = input$files[i, "name"]
                           extension = substr(file_name, regexpr("\\.[^\\.]*$", file_name), nchar(file_name))#regex gets the last '.'
                           if(tolower(extension) == ".rdata")
                           {  content = import(paste0("load('", input$files[i, "datapath"], "')"))
                              textual_result = paste0(textual_result, 'load("', file_name, '")\n', content, '\n')
                           }
                           else
                           {  if(tolower(extension) == ".rds")
                              {  variable = substr(file_name, 1, regexpr("\\.[^\\.]*$", file_name)-1)
                                 content = import(paste0(variable, " = readRDS('", input$files[i, "datapath"], "')"))
                                 textual_result = paste0(textual_result, 'readRDS("', file_name, '")\n', content, '\n')
                              }
                              else                  
                              {  opened_files <<- append(opened_files, input$files[i, "datapath"])
                                 code = paste(readLines(input$files[i, "datapath"]), collapse='\n')
                                 code_list = append(code_list, code)
                                 if(tolower(extension) == ".r")
                                 {  content = import(code)
                                    textual_result = paste0(textual_result, 'source(exprs=parse(text=&lt;tab_code&gt;))\n', content, '\n')
                                 }
                              }
                           }                     
                        }
                     }
                     if(length(code_list) > 0)
                     {  add_tabs("tab_set", code_list)  }
                     if(textual_result != '')
                     {  output$result <- renderUI(HTML(textual_result))  }
                  }
               )
            }
         }
      )
      
      # Show the tree nodes content
      observeEvent(
         input$data_tree,
         {  node_name = unlist(get_selected(input$data_tree))
            if(!is.null(node_name))
            {  if(!(node_name %in% opened_nodes))
               {  index = match(node_name, names(input$store))
                  add_tabs("tab_set", input$store[index], c(node_name))
               }               
               else
               {  updateTabsetPanel(session, inputId="tab_set", selected=node_name)  }
            }
         }
      )
      
      # Request a tab saving or update (if already saved) the content of a tab
      save_or_update <- function()
      {  tab_index = match(input$tab_set, opened_nodes)
         if(is.na(tab_index))#tab not saved yet
         {  session$sendCustomMessage("save_handler", '')  }
         else
         {  updateStore(session, name=opened_nodes[tab_index], value=isolate(input[[input$tab_set]]))  }
      }

      # Monitor the save_handler response; effectively save a new tree node
      observeEvent(
         input$saved_name,
         {  if(input$saved_name[1] != '')
            {  node_name = gsub("([^a-zA-Z0-9_-])", "", iconv(input$saved_name[1], "UTF-8", "ASCII", ''))
               if(!(node_name %in% names(input$store)))
               {  editor_number = substr(input$tab_set, 5, nchar(input$tab_set))
                  content = input[[paste0("editor_", substr(input$tab_set, 5, nchar(input$tab_set)))]]
                  updateStore(session, name=node_name, value=isolate(content))
                  update_tab(node_name)
               }
               else
               {  session$sendCustomMessage("save_handler", "Name already used; please select another one.")  }
            }
            else
            {  session$sendCustomMessage("save_handler", "Please enter a name not empty.")  }
         }
      )

      # Update the name (and corresponding editor) of a tab (rebuild all tabs)
      update_tab <- function(node_name)
      {  code = ''
         for(i in 1:tab_counter)
         {  if(tab_names[i] %in% opened_nodes)
            {  code = paste0(code, "create_tab('", tab_names[i], "', '", tab_names[i], "', input[['", tab_names[i], "']]), ")  }
            else
            {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
               if(tab_names[i] == input$tab_set)
               {  code = paste0(code, "create_tab('", node_name, "', '", node_name, "', input$editor_", editor_number, "), ")
                  opened_nodes <<- append(opened_nodes, node_name)
                  tab_names[i] <<- node_name
               }
               else
               {  code = paste0(code, "create_tab('editor_", editor_number, "', '", tab_names[i], "', input$editor_", editor_number, "), ")  }
            }
         }
         code = paste0("tabsetPanel(id='tab_set', ", code, "tabPanel('+'))")
         html_code = eval(parse(text=code))
         output$tab_panel = renderUI(html_code)
         for(i in 1:tab_counter)
         {  if(tab_names[i] %in% opened_nodes)
            {  aceAutocomplete(tab_names[i])$resume()
               session$sendCustomMessage("editor_height_handler", tab_names[i])               
            }
            else
            {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
               aceAutocomplete(paste0("editor_", editor_number))$resume()
               session$sendCustomMessage("editor_height_handler", paste0("editor_", editor_number))
            }
         }
         tab_index = match(node_name, tab_names)
         updateTabsetPanel(session, inputId="tab_set", selected=node_name)
      }

      # Monitor '+' tab
      observeEvent (
        input$tab_set,
         {  if(input$tab_set == '+')
            {  add_tabs("tab_set")  }
         }
      )
      
      # Add new tabs to tabset (rebuild all tabs)
      add_tabs <- function(tabset, contents_list=c(''), tab_titles=c(''))
      {  if(length(contents_list) > 0)
         {  code = ''
            for(i in 1:tab_counter)
            {  if(tab_names[i] %in% opened_nodes)
               {  code = paste0(code, "create_tab('", tab_names[i], "', '", tab_names[i], "', input[['", tab_names[i], "']]), ")  }
               else
               {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
                  code = paste0(code, "create_tab('editor_", editor_number, "', '", tab_names[i], "', input$editor_", editor_number, "), ")
               }
            }
            for(i in 1:length(contents_list))
            {  content = ''
               if(contents_list[i] != '')
               {  content = paste0(", contents_list[", i, "]")  }
               if(length(tab_titles) >= i && tab_titles[i] != '')
               {  node_name = gsub("([^a-zA-Z0-9_-])", "", iconv(tab_titles[i], "UTF-8", "ASCII", ''))
                  code = paste0(code, "create_tab('", node_name, "', '", node_name, "'", content, "), ")
                  tab_names <<- append(tab_names, node_name)
                  if(node_name %in% names(input$store))
                  {  opened_nodes <<- append(opened_nodes, node_name)  }
               }
               else
               {  tab_index = length(tab_names)
                  while((tab_index > 0) && (tab_names[tab_index] %in% opened_nodes))
                  {  tab_index = tab_index - 1  }
                  if(tab_index == 0)
                  {  code = paste0(code, "create_tab('editor_1', 'Tab_1'", content, "), ")
                     tab_names <<- append(tab_names, "Tab_1")
                  }
                  else
                  {  tab_index = as.numeric(substr(tab_names[tab_index], 5, nchar(tab_names[tab_index]))) + i
                     code = paste0(code, "create_tab('editor_", tab_index, "', 'Tab_", tab_index, "'", content, "), ")
                     tab_names <<- append(tab_names, paste0("Tab_", tab_index))
                  }
               }
            }
            tab_counter <<- tab_counter + length(contents_list)
            code = paste0("tabsetPanel(id='", tabset, "', ", code, "tabPanel('+'))")
            html_code = eval(parse(text=code))
            output$tab_panel = renderUI(html_code)
            for(i in 1:tab_counter)
            {  if(tab_names[i] %in% opened_nodes)
               {  aceAutocomplete(tab_names[i])$resume()
                  session$sendCustomMessage("editor_height_handler", tab_names[i])
                  
               }
               else
               {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
                  aceAutocomplete(paste0("editor_", editor_number))$resume()
                  session$sendCustomMessage("editor_height_handler", paste0("editor_", editor_number))
               }
            }
            updateTabsetPanel(session, inputId=tabset, selected=tab_names[tab_counter])
         }
      }
      
      # Close a tab (rebuild all tabs)
      close_tab <- function(tab_id=input$tab_set, tabset="tab_set")
      {  tab_index = match(tab_id, tab_names)
         if(!is.na(tab_index))
         {  code = ''
            if(tab_counter == 1)
            {  code = "create_tab('editor_1', 'Tab_1'), "
               tab_names <<- c("Tab_1")
            }
            else
            {  for(i in 1:tab_counter)
               {  if(i != tab_index)
                  {  if(tab_names[i] %in% opened_nodes)
                     {  code = paste0(code, "create_tab('", tab_names[i], "', '", tab_names[i], "', input[['", tab_names[i], "']]), ")  }
                     else
                     {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
                        code = paste0(code, "create_tab('editor_", editor_number, "', '", tab_names[i], "', input$editor_", editor_number, "), ")
                     }
                  }
               }
               tab_names <<- tab_names[-tab_index]
               if(tab_index == tab_counter)
               {  tab_index = tab_counter - 1  }
               tab_counter <<- tab_counter - 1
            }
            node_index = match(tab_id, opened_nodes)
            if(!is.na(node_index))
            {  opened_nodes <<- opened_nodes[-node_index]  }
            code = paste0("tabsetPanel(id='", tabset, "', ", code, "tabPanel('+'))")
            html_code = eval(parse(text=code))
            output$tab_panel = renderUI(html_code)
            for(i in 1:tab_counter)
            {  if(tab_names[i] %in% opened_nodes)
               {  aceAutocomplete(tab_names[i])$resume()
                  session$sendCustomMessage("editor_height_handler", tab_names[i])
               }
               else
               {  editor_number = substr(tab_names[i], 5, nchar(tab_names[i]))
                  aceAutocomplete(paste0("editor_", editor_number))$resume()
                  session$sendCustomMessage("editor_height_handler", paste0("editor_", editor_number))
               }
            }
            updateTabsetPanel(session, inputId=tabset, selected=tab_names[tab_index])#tab next to the closed one
         }
      }
      
      # Generate code for one new tab
      create_tab <- function(editor_id, tab_title, content='', read_only=FALSE)
      {  session$sendCustomMessage("editor_height_handler", editor_id)
         tabPanel(title=tab_title,
            aceEditor
            (  outputId=editor_id,
               value=content,
               mode="r",
               readOnly=read_only,
               height=input$editor_height,
               fontSize=16,
               hotkeys=list(key_F9="F9", key_F10="F10", key_Ctrl_S=list(win="Ctrl-S", mac="CMD-S")),
               autoComplete="live"
            )
         )
      }
      
      # Update the data tree when a node is updated in the store
      observeEvent(
         input$store,
         {  if(length(input$store) > 0)
            {  temp_list = list()
               for(i in 1:length(input$store))
               {  temp_list = append(temp_list, list(structure(isolate(input$store[[i]]), sticon="file")))  }
               names(temp_list) = names(input$store)
               output$tree_interface <- renderUI(
                  shinyTree(
                     outputId="data_tree",
                     search=TRUE,
                     dragAndDrop=TRUE
                  )
               )
               output$data_tree <- renderTree(temp_list)
            }
            else
            {  output$tree_interface <- renderUI("There is no saved file")  }
         }
      )

      session$onSessionEnded(
         function()
         {  tab_counter <<- 1
            opened_files <<- c(character(0))
            opened_nodes <<- c()
            tab_names <<- c()
            rm(list=ls())
         }
      )

   }
)

# IMPLEMENTAR:
#
#    QUANDO TENTAR SE SALVAR UMA ABA COM UM NOME COM CARACTERES ESPECIAIS MOSTRAR UMA CAIXA DE DIÁLOGO AVISANDO QUE APENAS LETRAS, NÚMEROS, "_" E "-" SÃO PERMITIDOS
#    AO INVÉS DE ELIMINAR OS CARACTERES ESPECIAIS AUTOMATICAMENTE DO NOME DADO E SALVAR COM O NOME RESULTANTE
#    
#    MOSTRAR UMA CAIXA DE DIÁLOGO PERGUNTANDO SE DESEJA-SE SALVAR UMA ABA NÃO SALVA E NÃO VAZIA AO FECHÁ-LA, ESTANDO ESSA ABA JÃ REGISTRADA NA ÁRVORE (DESATUALIZADA) OU NÃO
#
#    REMOVER SELEÇÃO DO NÓ DA ÁRVORE QUANDO A ABA CORRESPONDENTE FOR FECHADA E QUANDO SE TROCAR DE ABA (PARA SE PORDER SELECIONÁ-LA NOVAMENTE DEPOIS)
#
#    ATUALIZAR O NOME DA ABA ABERTA QUANDO SE FAZ O UPLOAD DE UM ARQUIVO PARA O NOME DO ARQUIVO
