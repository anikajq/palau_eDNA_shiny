# install.packages("shiny")
# install.packages("rsconnect")

library(shiny)
library(rsconnect)
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Anika's Capstone Project"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

# Run the application 
# shinyApp(ui = ui, server = server)


#outputs that are loaded into the app: 

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
  addPolygons(data=EEZ_outline, color=EEZ_color, opacity=0.3, weight=1, fillOpacity=0.3)

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


#commercially important species: 
comm_import_sp <- read_csv("PICRC_commercially_important_fish_copy.csv")
comm_import_sp <- comm_import_sp[1]
View(comm_import_sp)

comm_import_sp_vec <- comm_import_sp$Scientific_name

# shinyApp(
#   ui = fluidPage(
#     selectInput("env_var", "Environmental Variable:",
#                 c("Cylinders" = "cyl",
#                   "Transmission" = "am",
#                   "Gears" = "gear")),
#     tableOutput("data")
#   ),
#   server = function(input, output) {
#     output$data <- renderTable({
#       mtcars[, c("mpg", input$variable), drop = FALSE]
#     }, rownames = TRUE)
#   }
# )

#example/practice for species:
shinyApp(
  ui = fluidPage(
    selectInput("sp_choice", "Species of Interest:",
                comm_import_sp_vec),
    tableOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderTable({
      comm_import_sp[, c(input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
)


############FOR PALAU:

#Palau eDNA visuals:
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Anika's Capstone Project"),
  p(),
  # titlePanel("Salinity"),
  selectInput("env_var", "Environmental Variable:",
              c("Salinity" = "sal", "Temperature" = "temp", "pH" = "ph")),
  checkboxGroupInput("env_var2", "Environmental Variable:", c("Salinity" = "sal", "Temperature" = "temp", "pH" = "ph")),
  selectInput("sp_choice", "Species of Interest:",
              comm_import_sp_vec),
  checkboxGroupInput("sp_choice2", "Species of Interest:", comm_import_sp_vec),
  textOutput("sal_text"),
  leafletOutput("salinity_map", width = "80%", height = 600), #ADJUST WIDtH AND HEIGHT HERE!!!!!
  p(),
  leafletOutput("species_map", width = "80%", height = 600)
  # actionButton("recalc", "New points") # this line generates a button that, A button that, when clicked, triggers regeneration of new random points. (not needed rn, but potentially in the future?)
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  ###The above code is for generating random new points using the new points button from above. Not sure exactly how it works, but seems like it's helpful. 
  
  
  #   output$species_map <- renderLeaflet({
  #     leaflet() %>%
  #       addProviderTiles(providers$CartoDB.Positron,
  #                        options = providerTileOptions(noWrap = TRUE)
  #       ) %>%
  #       addMarkers(data = points())
  #   })
  # }
  
  
  #to make the map in-house:
  #   output$species_map <- renderLeaflet({
  #     leaflet() %>%
  #       addTiles() %>%
  #       setView(lng = 134.4, lat = 7.3, zoom = 6) %>%
  #       addMarkers(data = points())
  #   })
  #   
  # }
  
  output$sal_text <- renderText({ 
    "Salinity Map" 
  })
  
  output$salinity_map <- renderLeaflet({
    map_fixed_rad 
  })
  
  output$species_map <- renderLeaflet({
    map_fixed_rad 
  })
  
}
shinyApp(ui, server)
