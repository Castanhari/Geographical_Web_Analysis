//window.onload = function()
//{  window.parent.InpeExt.ShinyTabPanel.prototype.setChildPanelReference(window);  };

Shiny.addCustomMessageHandler(
  "server_handler",
   function(server)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setServerUrl("+server+");");  }
);

Shiny.addCustomMessageHandler(
  "coverage_handler",
   function(cov)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setCoverage("+cov+");");  }
);

Shiny.addCustomMessageHandler(
  "attributes_handler",
   function(attrs)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setAttributes("+attrs+");");  }
);

Shiny.addCustomMessageHandler(
  "latitude_handler",
   function(lat)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setLatitude("+lat+");");  }
);

Shiny.addCustomMessageHandler(
  "longitude_handler",
   function(long)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setLongitude("+long+");");  }
);

Shiny.addCustomMessageHandler(
  "start_handler",
   function(start)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setStartDate("+start+");");  }
);

Shiny.addCustomMessageHandler(
  "end_handler",
   function(end)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setEndDate("+end+");");  }
);

Shiny.addCustomMessageHandler(
  "apply_handler",
   function(unused)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.onApply();");  }
);

