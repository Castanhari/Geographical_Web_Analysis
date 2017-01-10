Shiny.addCustomMessageHandler(
   "KVP_handler",
   function(KVP)
   {  console.log("window.parent.InpeExt.ShinyTabPanel.prototype.setExtraKVPParameters("+KVP+");");  }
);

