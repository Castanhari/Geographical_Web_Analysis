library(rgdal)
library(gdalUtils)

# Remote data source
DSN <- "WFS:http://terrabrasilis.info/geoserver/wms?SERVICE=WMS&request=getCapabilities"

# List layers from remote data source
ogrListLayers(DSN)

###

library(rgeos)

# Convert layer into local shapefile
#ogr2ogr(DSN, "Desktop/Shiny/Apps/desflores", "Prodes:prodes_desflorestamento", spat=c(-58, -12, -55, -9))

# Read layer from local shapefile
layer <- readOGR("Desktop/Shiny/Apps/desflores", "Prodes:prodes_desflorestamento", stringsAsFactors=FALSE)

# Get a random polygon from the layer
feature <- sample(length(layer@polygons), 1)
poly <- layer@polygons[[feature]]
plot(poly@Polygons[[1]]@coords, type="l")

# Get a point inside the polygon
pt = gPointOnSurface(SpatialPolygons(list(poly)))
points(pt)

###

library(wtss.R)
#source("../timeSeriesApp/helpers.R")

#Get the time series of the point
server <- WTSS("http://www.dpi.inpe.br/tws/wtss")
cov_name = listCoverages(server)[2]
cov = describeCoverage(server, cov_name)
cov_attrs = cov[[cov_name]]$attributes$name
cov_temporal = cov[[cov_name]]$geo_extent$temporal
time_series = timeSeries(server, cov_name, cov_attrs, pt@coords[1], pt@coords[2], cov_temporal$start, cov_temporal$end)
ts = get_attributes(time_series)
ts
plot(ts)

###

# Apply BFAST
bfast01 = apply_bfast01(ts)
plot(bfast01)

# Apply TWDTW
twdtw = apply_twdtw(ts)
plot(twdtw, type="classification", overlap=0.5)

###

# Example of user interface components
h3("Title")
selectInput("select", "Select", paste("Option", 1:3))
actionButton("button", "Click")

