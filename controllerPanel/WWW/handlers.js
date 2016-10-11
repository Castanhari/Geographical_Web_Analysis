window.onload = function()
{  window.parent.InpeExt.ShinyTabPanel.prototype.setChildPanelReference(window);  };

Shiny.addCustomMessageHandler(
  "server_handler",
   function(server)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setServerUrl(server);  }
);

Shiny.addCustomMessageHandler(
  "coverage_handler",
   function(cov)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setCoverage(cov);  }
);

Shiny.addCustomMessageHandler(
  "attributes_handler",
   function(attrs)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setAttributes(attrs);  }
);

Shiny.addCustomMessageHandler(
  "latitude_handler",
   function(lat)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setLatitude(lat);  }
);

Shiny.addCustomMessageHandler(
  "longitude_handler",
   function(long)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setLongitude(long);  }
);

Shiny.addCustomMessageHandler(
  "start_handler",
   function(start)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setStartDate(start);  }
);

Shiny.addCustomMessageHandler(
  "end_handler",
   function(end)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.setEndDate(end);  }
);

Shiny.addCustomMessageHandler(
  "apply_handler",
   function(unused)
   {  window.parent.InpeExt.ShinyTabPanel.prototype.onApply();  }
);
