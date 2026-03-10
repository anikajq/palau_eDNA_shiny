# mapping
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("sf")
# install.packages("mapview")
# install.packages("webshot")
# webshot::install_phantomjs()

library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(webshot)

dir <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data"
setwd(dir)

locations_salinity <- read.csv("locations_salinity.csv", header = TRUE)
site_locations <- read.csv("site_locations.csv", header = TRUE)

#### Basic Basemap with Stations: 
palau_map <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = 134.4, lat=7.3, zoom=6)

PNMS_outline <- st_read(file.path(dir, "PNMS_shapefiles/PNMS.shp"))
EEZ_outline <- st_read(file.path(dir, "palau_eez/eez.shp"))

# st_geometry_type(PNMS_outline)

EEZ_color <- "#444444"
PNMS_color <- "#E84A5F"
station_color <- "blue"

# Add Paluan EEZ polygon to basemap:
palau_polygons <- palau_map %>% 
  addPolygons(data=EEZ_outline, color=EEZ_color, opacity=0.8, weight=1, fillOpacity=0.3)

# Add PNMS polygon to map: 
palau_polygons <- palau_polygons %>% 
  addPolygons(data=PNMS_outline, color=PNMS_color, opacity=1, weight=1, fillOpacity=0.5) %>% 
  addLegend(
    colors = c(PNMS_color, EEZ_color),
    labels = c("PNMS", "EEZ"),
    # title = "Legend",
    opacity = 1, 
    position = "bottomleft"
  )

# # Add sample stations to map: 
# all_stations <- palau_polygons %>% 
#   # lapply(htmltools::HTML) %>% 
#   addCircles(
#     data = site_locations,
#     # label = paste(site_locations$label, paste("Station Number:", site_locations$station_number), sep= "\n"), 
#     label = station_label, 
#     radius = 1000, #here we could change the radius for offshore vs nearshore if we wanted, via an ifelse statement (e.g. radius = ~ifelse(type == "offshore", 6, 10),)
#     color = station_color,
#     stroke=TRUE,
#     fillOpacity = 0.5
#   )

# all_stations

#THIS NEEDS FIXING STILL (MAPPING SALINITY BY COLOR)

#jittering locations of stations (can be adjusted if desired)
locations_salinity$jittered_lat <- jitter(locations_salinity$latitude, factor = 30)
locations_salinity$jittered_lng <- jitter(locations_salinity$longitude, factor = 30)

# pal <- colorBin(palette = "PuBu", domain = fake_site_sal$fake_sal, bins = 5)
pal <- colorNumeric(palette = "PuBu", domain = locations_salinity$avg_salinity_1_2m)

legend_colors <- pal(locations_salinity$avg_salinity_1_2m)

#USING CIRCLE MARKERS (fixed number of pixels)
####given that we are trying to fix the distance, not the number of pixels, I have commented this out, but it can be uncommented if desired
####for reference, the species map from before uses circle markers with fixed pixel numbers, not fixed distance
# map_fixed_pixels <- palau_polygons %>%
#   addCircleMarkers(
#     data = locations_salinity,
#     lat = ~jittered_lat,
#     lng = ~jittered_lng,
#     label = paste(locations_salinity$label, paste("Station Number:", locations_salinity$station_number), paste("Salinity:", locations_salinity$avg_salinity_1_2m), sep= " | "),
#     radius = 6,
#     color = ~pal(avg_salinity_1_2m),
#     stroke=FALSE,
#     fillOpacity = 0.8,
#   ) %>%
#   addLegend(
#     position = "bottomright",
#     pal = pal,
#     values = locations_salinity$avg_salinity_1_2m,
#     title = "Salinity"
#   )


###USING CIRCLES (fixed size/radius)
#####this produces what was desired from talks with Jeanine/Collin, but not sure what the actual radius is in km/miles.
map_fixed_rad <- palau_polygons %>% 
  addCircles(
    data = locations_salinity, 
    lat = ~jittered_lat,
    lng = ~jittered_lng,
    label = paste(locations_salinity$label, paste("Station Number:", locations_salinity$station_number), paste("Salinity:", locations_salinity$avg_salinity_1_2m), sep= " | "), 
    radius = 9000, #can be adjusted!! 
    color = ~pal(avg_salinity_1_2m),
    stroke=FALSE,
    fillOpacity = 0.8, 
  ) %>% 
  addLegend(
    position = "bottomright",    
    pal = pal,                   
    values = locations_salinity$avg_salinity_1_2m,
    title = "Salinity",     
  )


#Save Maps:
map_name <- "salinity_map"

# TO SAVE MAP AS HTML (preferred, remains zoom-able and interactive):
mapshot(map_fixed_rad, url = file.path(paste("products/", map_name, ".html", sep="")))

# TO SAVE MAP AS PNG:
mapshot(map_fixed_rad, file = file.path(paste("products/", map_name, ".png", sep="")))
