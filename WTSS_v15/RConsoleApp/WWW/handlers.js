Shiny.addCustomMessageHandler(
   "plot_handler",
   function(plot)
   {  new_div = document.createElement("div")
      new_div.id = plot[2];
      new_div.title = plot[0];
      image = document.createElement("IMG");
      image.setAttribute("src", plot[1]);
      new_div.appendChild(image);
      document.getElementById("div_id").appendChild(new_div);
      $('#'+plot[2]).dialog({height:"auto", width:"auto"});//position:{collision:"flipfit"}
      $('#'+plot[2]).on("dialogclose", function(event)
         {  $('#'+plot[2]).remove();  }
      );
      //var config = { url: plot[1], title: plot[0], index: plot[2] };
      //window.parent.InpeExt.ShinyTabPanel.prototype.openWindow(config);
   }
);

Shiny.addCustomMessageHandler(
   "removal_handler",
   function(plot)
   {  $('#'+plot).remove();  }
   //{  window.parent.InpeExt.ShinyTabPanel.prototype.closeWindow(plot);  }
);

Shiny.addCustomMessageHandler(
   "close_handler",
   function(unused)
   {  }//window.parent.InpeExt.ShinyTabPanel.prototype.closeAllWindow();  }
);

//http://stackoverflow.com/questions/10134724/resize-ajax-org-ace-editor-based-on-browsers-size
function resize(editor)
{  height = 0;
   while(height == 0)
   //{  height = $("#left_panel").height() - $("#buttons_row").height() - $("#tab_set").height();  }
   {  height = $("#code_panel").height() - $("#tab_set").height();  }
   $('#'+editor).css("height", height.toString() + "px");
   return height
};

Shiny.addCustomMessageHandler(
   "editor_height_handler",
   function(editor)
   {  height = resize(editor);
      $(window).on("resize", function() {  resize(editor);  });
      Shiny.onInputChange("editor_height", height.toString() + "px");
   }
);

Shiny.addCustomMessageHandler("log", function(arg){console.log(arg);});

Shiny.addCustomMessageHandler(
   "save_handler",
   function(message)
   {  save_form = document.createElement("form")
      save_form.id = "save";
      save_form.title = "Save tab content";
      if(message != '')
      {
      heading = document.createElement("h4");
      heading.innerHTML = message;
      save_form.appendChild(heading);
      }
      field_label = document.createElement("label");
      field_label.innerHTML = "Name&nbsp;";
      save_form.appendChild(field_label);
      field_input = document.createElement("input");
      field_input.setAttribute("type", "text");
      save_form.appendChild(field_input);
      document.getElementById("div_id").appendChild(save_form);      
      $("#save").dialog(
         {  height:"auto",
            width:"auto",
            modal:true,
            buttons:
            {  Save: function()
               {  Shiny.onInputChange("saved_name", [field_input.value, Math.random()]);
                  console.log(field_input.value)
                  $("#save").dialog("close");
                  $("#save").remove();
               },
               Cancel: function()
               {  $("#save").dialog("close");
                  $("#save").remove();
               }
            }
         }
      );
      $("#save").on(
         "dialogclose",
         function(event)
         {  $("#save").remove();  }
      );
   }
);
