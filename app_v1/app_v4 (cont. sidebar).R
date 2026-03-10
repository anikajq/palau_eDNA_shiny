# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("bslib")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("tidyverse")

library(rsconnect)
library(bslib)
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

#outputs that are loaded into the app: 

# Part 1: eDNA data: 

#1.1: load data:

########EDIT HERE!!!!#####################
#Edit the base_dir to wherever the Visual Applications folder is stored on your computer. 
base_dir <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/Data Organization/Visual Applications"
edna_dir <- paste0(base_dir, "/data/eDNA/Nov2021")
other_dir <- paste0(base_dir, "/data/other")
spatial_dir <- paste0(base_dir, "/data/spatial")

setwd(edna_dir)
esv_clean_data_df <- read_csv("esv_read_data_rv.csv")
samp_meta <- read_csv("SampleMetaData.csv") 
samp_meta_df <- data.frame(samp_meta, row.names = 1) # use 1st column as row names
default_esv <- data.frame(esv_clean_data_df)
default_meta <- samp_meta_df %>% rownames_to_column(var="SampleID")

setwd(spatial_dir)
# locations_salinity <- read.csv("locations_salinity.csv", header = TRUE)
site_locations <- read.csv("site_locations.csv", header = TRUE)

setwd(other_dir)
comm_import_sp <- read_csv("PICRC_commercially_important_fish.csv")
comm_import_sp <- comm_import_sp[1]
# View(comm_import_sp)
comm_import_sp_vec <- comm_import_sp$Scientific_name


#1.2: Helper Functions: 

#1.2.1 helper functions from Individual Species Information

species_df <- function(species, df=default_esv) {
  result <- df %>% dplyr::filter(Species == species)
  return(result)
}

combine_esvs <- function(df, species){
  result <- data.frame(colSums(df[, 21:ncol(df)]))
  colnames(result) <- paste("detections of", species, sep=" ")
  return(result)
}

detect_to_bool <- function(df){
  df$detected <- with(df, df[[1]] > 0, TRUE, FALSE)
  df<- df %>% rownames_to_column(var = "SampleID")
  return(df)
}

#joining with metadata set:

filter_join <- function(df, meta=default_meta){
  result <- df %>% 
    dplyr::filter(detected == TRUE) %>% 
    left_join(y=meta, by=c("SampleID" = "SampleID"))
  result <- result %>% 
    dplyr::filter(Control_or_Envtl == "Envtl") # this ensures it only accounts for only envtl data--can be changed/commented out if control data is desired
  return(result)
}

print_zones <- function(df, species, meta=default_meta){
  meta <- meta %>% 
    dplyr::filter(Control_or_Envtl == "Envtl")
  print(paste("Date of Collection:", df$Date[1]))
  print(paste("Stations with ", species, ":", sep=""))
  print(unique(df$Station))
  print("found Nearshore?")
  if ("Nearshore" %in% df$Near_or_Offshore){
    print("yes") }
  else{
    print("no")
  }
  print("found Offshore?")
  if ("Offshore" %in% df$Near_or_Offshore){
    print("yes")}
  else{
    print("no")
  }
  print(paste("Zones in which", species, "is found:"))
  print(unique(df$Zone))
  print(paste("Zones in which", species, "is NOT found:"))
  print(setdiff(unique(meta$Zone), unique(df$Zone)))
  print(paste("directions in which", species, "is found:"))
  print(unique(df$Cardinal_direction))
}


# species_to_zone() takes in a species name (character) and a dataframe with sample data (formatted in the same way/with the same headers as esv_clean_data_rv, or default_esv), and it prints out the relevant information as to which zones it is found in. 
species_to_zone <- function(species, data=default_esv, meta = default_meta){
  result <- species_df(species, data)
  result <- filter_join(df = detect_to_bool(combine_esvs(result, species)), meta = meta)
  print_zones(result, species, meta=meta)
}


#1.2.2 connectivity helper functions:

find_samples <- function(zones, meta=default_meta){
  result <- list()
  meta <- meta %>% 
    dplyr::filter(Control_or_Envtl == "Envtl") # this ensures it only accounts for only envtl data--can be changed/commented out if control data is desired
  for (z in zones){
    df_temp <- meta %>% dplyr::filter(Zone==z)
    result[z] <- list(df_temp$SampleID)
  }
  return(result)
}

find_data <- function(samples, data = default_esv) {
  unlisted_samples <- unlist(samples)
  location_data <- select(data, TestId:PublishStatus, all_of(unlisted_samples)) #takes out all the station names that aren't in vector of samples
  return(location_data)
}

# note: uses the column number 21 because that is what the given esv dataset has, and it assumes that any other/future datasets would have the same number/order of columns. This number can be adjusted in the future as needed though. 
no_zeroes <- function(df){
  if (ncol(df) <21){
    stop("error! stop and see if df inputted into no_zeroes has fewer than 21 columns")
  }
  df_temp <- df[21:(ncol(df))]
  pos_rows <- rowSums(df_temp == 0, na.rm = TRUE) < ncol(df_temp)
  pos_results <- df[pos_rows, ]
  return(pos_results)
}

species <- function(df){
  no_nas <- df %>% 
    dplyr::filter(Species != "")
  vector_of_species <- no_nas$Species
  return(unique(vector_of_species))
}

find_data_compiled <- function(samples_list, zones, data=default_esv){
  result <- list()
  for(z in zones){
    result[z] <- list(species((no_zeroes(find_data(samples_list[[z]], data=data)))))
  }
  return(result)
}

zone_overlap <- function(dict){
  result_species <- dict[[1]]
  for (i in seq(length(dict))){
    result_species <- intersect(dict[[i]], result_species)
  }
  return(result_species)
}


# cosmo_species() finds the cosmopolitan species across different zones
# The input is a vector of strings with the zone names of the options given below, and the output is a vector of the overlapping species between all of those zones. It does not include the cardinal directions or station numbers. This uses all samples from the default_esv and default_meta dataframes unless the arguments are specified
cosmo_species <- function(zones, data=default_esv, meta=default_meta){
  samples <- find_samples(zones, meta=meta)
  species <- find_data_compiled(samples_list = samples, zones=zones, data=data)
  print("cosmopolitan species between")
  print(zones)
  return(zone_overlap(species))
}


# 1.2.3 more cosmopolitan species helper functions across zones + direction:

find_samples_dir <- function(zones, meta=default_meta){
  meta <- meta %>% 
    dplyr::filter(Control_or_Envtl == "Envtl") # this ensures it only accounts for only envtl data--can be changed/commented out if control data is desired
  result <- list()
  for (zone in zones){
    df_temp <- meta %>% dplyr::filter(Zone==zone[1] & Cardinal_direction==zone[2])
    key <- paste(zone[1], "_", zone[2], sep="")
    result[key] <- list(df_temp$SampleID)
  }
  return(result)
}

find_data_compiled_dir <- function(samples_list, zones, data=default_esv){
  result <- list()
  for (loc in names(samples_list)){
    result[loc] <- list(species((no_zeroes(
      find_data(samples_list[[loc]], data=data)
    ))))
  }
  return(result)
}

# cosmo_species_dir() finds the cosmopolitan species across different stations, with each station identifiied by (zone + direction). 
# The input is a list with vectors that contain the zone and directions, and the output is a vector of the overlapping species between all of those zones. Also prints out the number of shared species.
cosmo_species_dir <- function(zones, meta=default_meta, data=default_esv){
  samples <- find_samples_dir(zones, meta)
  species <- find_data_compiled_dir(samples_list=samples, zones = zones, data=data)
  result <- zone_overlap(species)
  print("cosmopolitan species between")
  print(zones)
  print("number of shared species:")
  print(length(result))
  return(result)
}

#1.2.4 cosmo species helper functions across stations: 
find_samples_st <- function(stations, meta=default_meta){
  meta <- meta %>% 
    dplyr::filter(Control_or_Envtl == "Envtl") # this ensures it only accounts for only envtl data--can be changed/commented out if control data is desired
  result <- list()
  for (s in stations){
    df_temp <- meta %>% dplyr::filter(Station==s)
    result[s] <- list(df_temp$SampleID)
  }
  return(result)
}

find_data_compiled_st <- function(samples_list, stations, data=default_esv){
  result <- list()
  for(s in stations){
    result[s] <- list(species((no_zeroes(find_data(samples_list[[s]], data=data)))))
  }
  return(result)
}

# cosmo_species_st() finds the cosmopolitan species across different stations. The input is a vector of strings with the station, and the output is a vector of the overlapping species between all of those zones
cosmo_species_st <- function(stations, meta = default_meta, data = default_esv){
  samples <- find_samples_st(stations, meta = meta)
  species <- find_data_compiled_st(samples, stations, data=data)
  result <- zone_overlap(species)
  print("cosmopolitan species between stations")
  print(stations)
  print("number of shared species:")
  print(length(result))
  return(result)
}

# 1.2.5 EXAMPLE FINDING COSMOPOLITAN SPECIES USING STATION NUMBERS:

# input is a vector of station numbers as characters
example_list_st <- c("11", "20", "41")
cosmo_species_st(example_list_st, meta=default_meta, data=default_esv)

# 1.2.6 more helper functions for filtering through species for visualization:
species_list_df <- function(data=default_esv){
  result <- data.frame(species_name = unique(data$Species))
  result <- result %>% 
    dplyr::filter(species_name != "")
  return(result)
}

species_info_df <- function(species, data = default_esv){
  result <- no_zeroes(species_df(species, data))
  result <- filter_join(detect_to_bool(combine_esvs(result, species)))
  return(result)
}

create_species_summary <- function(data = default_esv){
  species_list <- species_list_df(data) #create list of species for given 
  species_list <- species_list %>% 
    dplyr::filter(species_name != "")
  rownames(species_list) <- species_list[,1] 
  species_list$station_names <- NA #create empty columns
  species_list$num_stations <- NA
  for(species in species_list$species_name){ #go through each species and add to new species_list
    species_info <- species_info_df(species, data=data)
    num_stations <- length(unique(species_info$Station))
    station_names <- paste(unique(species_info$Station), collapse = ", ")
    unique_stations <- unique(species_info$Station[!is.na(species_info$Station)])
    species_list[species, "num_stations"] <- length(unique_stations)
    species_list[species, "station_names"] <- paste(unique_stations, collapse = ", ")
  }
  return(species_list)
}

as_many_stations <- function(data=default_esv, num){
  sp_sum <- create_species_summary(data)
  results <- sp_sum %>% 
    dplyr::filter(num_stations >= num)
  print(paste("number of species found in >=", num, "stations:"))
  print(length(results$species_name))
  return(results[order(results$num_stations), ])
}

as_many_stations2 <- function(df, num){
  results <- df %>%
    dplyr::filter(species_name != "Homo sapiens" & species_name != "Sus scrofa") %>% #can comment out this line if wanted
    dplyr::filter(num_stations >= num)
  print(paste("number of species found in >=", num, "stations (no humans/pigs):"))
  print(length(results$species_name))
  return(results[order(results$num_stations), ])
}

# 1.3 MAPPING: 

#1.3.1 loading basemaps: 

palau_map <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = 134.4, lat=7.3, zoom=6)

PNMS_outline <- st_read(file.path(spatial_dir, "PNMS_shapefiles/PNMS.shp"))
EEZ_outline <- st_read(file.path(spatial_dir, "palau_eez/eez.shp"))


# IF YOU WOULD LIKE TO CUSTOMIZE THE COLOR OF THE POLYGONS, STATION ICONS, ETC, CHANGE THESE VARIABLES:
EEZ_color <- "#444444"
PNMS_color <- "#E84A5F"
station_color <- "blue"

# Add Paluan EEZ polygon to basemap:
palau_polygons <- palau_map %>% 
  addPolygons(data=EEZ_outline, color=EEZ_color, opacity=0.8, weight=1.3, fillOpacity=0)

# Add PNMS polygon to map: 
palau_polygons <- palau_polygons %>% 
  addPolygons(data=PNMS_outline, color=PNMS_color, opacity=1, weight=1, fillOpacity=0.1) %>% 
  addLegend(
    colors = c(PNMS_color, EEZ_color),
    labels = c("PNMS", "EEZ"),
    # title = "Legend",
    opacity = 1, 
    position = "bottomleft"
  )


# Add sample stations to map: (base stations, no data conveyed)
all_stations <- palau_polygons %>% 
  # lapply(htmltools::HTML) %>% 
  addCircleMarkers(
    data = site_locations,
    label = paste(site_locations$label, paste("Station Number:", site_locations$station_number), sep= "\n"), 
    radius = 5,
    color = station_color,
    stroke=FALSE,
    fillOpacity = 0.7
  )



#1.3.2 interactive parts of map HELPER FUNCTIONS:

create_species_summary3 <- function(data=default_esv, species) {
  species_info <- species_info_df(species)
  
  # Initialize an empty data frame to store the results
  result_df <- data.frame(species_name = character(),
                          station = integer(),
                          stringsAsFactors = FALSE)
  
  # Extract unique stations for the species
  unique_stations <- unique(species_info$Station[!is.na(species_info$Station)])
  # Only create a data frame if unique_stations is not empty
  if (length(unique_stations) > 0) {
    # Create a temporary data frame for the species with its stations
    temp_df <- data.frame(species_name = species,
                          station = unique_stations,
                          stringsAsFactors = FALSE)
    # Append the temporary data frame to the summary data frame
    result_df <- rbind(result_df, temp_df)
  }
  return(result_df)
}

stations <- function(species, data=default_esv){
  output_df <- create_species_summary3(data=data, species=species)
  return(output_df$station)
}

sites_by_sp <- function(species, data=default_esv, meta = default_meta){
  filtered_sites <- site_locations %>% 
    dplyr::filter(station_number %in% stations(species, data))
  return(filtered_sites)
}


# map_multi_species() takes in a vector of species (strings), and generates a map of which stations each of those species has been found at, colored by species.

# Input sort is TRUE by default, just makes it so that colors are in order and the species are listed alphabetically
map_multi_species <- function(sp_vec, basemap = palau_polygons, data=default_esv, meta = default_meta, sort = TRUE){
  if (sort == TRUE){
    sp_vec <- sort(sp_vec)
  }
  stations <- data.frame()
  
  if (length(sp_vec) < 1){
    return(basemap)
  }
  
  for (species in unique(sp_vec)){
    sub_stations <- sites_by_sp(species=species, data=data, meta=meta)
    sub_stations$species_name <- species
    stations <- rbind(stations, sub_stations)
  }
  
  stations$jittered_lat <- jitter(stations$latitude, factor = 30)
  stations$jittered_lng <- jitter(stations$longitude, factor = 30)
  
  sp_labels <- unique(stations$species_name)
  pal <- colorFactor(rainbow(length(sp_labels)), domain = sp_labels)
  
  legend_colors <- pal(sp_labels)
  legend_labels <- sp_labels
  
  basemap %>% 
    addCircleMarkers(
      data = stations, 
      lat = ~jittered_lat,
      lng = ~jittered_lng,
      label = paste(stations$species_name, stations$label, paste("Station Number:", stations$station_number), sep= " | "), 
      radius = 6,
      color = ~pal(species_name),
      stroke=FALSE,
      fillOpacity = 0.8, 
    ) %>% 
    addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels,
              title = "Species",
              opacity = 1
    )
}

# Part 2: Environmental Data

# 2.1 loading data:
enviro_dir <- paste0(base_dir, "/data/environmental")
setwd(enviro_dir)
locations_salinity <- read.csv("locations_salinity.csv", header = TRUE)


# 2.2 cleaning and combining csvs: 
file_metadata <- read.csv("AQ Copy of Labels_Coordinates_SampleInventory_May2022.xlsx - Filename metadata.csv", header=TRUE)
# View(file_metadata)

# Now, to combine all 5 csv files for the same station: 

# Group by station number to combine csvs from the same station
# get rid of anything (for now!) where station is not reported: (can comment this line out later if desired)
file_metadata <- file_metadata %>% 
  filter(Station != "")

grouped_files <- file_metadata %>%
  group_by(Station) %>%
  summarise(filenames = list(filename))

# View(grouped_files)

# Iterate over each station group

# input_dir <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/AAQ_raw"
raw_dir <-  paste0(base_dir, "/data/environmental/AAQ_raw")
combined_csvs_dir <- paste0(base_dir, "/data/environmental/combined_csvs")

for (i in seq_len(nrow(grouped_files))) {
  station <- grouped_files$Station[i]
  file_list <- grouped_files$filenames[[i]]
  
  data_list <- lapply(file_list, function(f) {
    file_path <- file.path(raw_dir, f)  # Construct full path
    read.csv(file_path, header = TRUE, row.names = NULL, skip = 65, fileEncoding = "latin1")
  })
  
  # Standardize column names across all dataframes
  common_cols <- Reduce(intersect, lapply(data_list, colnames))  # Find common columns
  data_list <- lapply(data_list, function(df) df[common_cols])  # Keep only common columns
  
  # Combine all standardized dataframes
  combined_df <- do.call(rbind, data_list)
  combined_df$Station <- station #adds the station number as a column
  
  #save new combined csv:
  output_filename <- file.path(combined_csvs_dir, paste0("combined_station_", station, ".csv"))
  
  write.csv(combined_df, output_filename, row.names = FALSE)
  
  print(paste("Saved:", output_filename))
}


##### NEXT SECTION: Using combined csvs to find trends among them (aka averaging everything) 

# Load back all csvs into R: 
folder_path <- combined_csvs_dir
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# data_list <- lapply(csv_files, read_csv)
# View(data_list)

for (file in csv_files) {
  # Get the name of the file without the extension
  file_name <- tools::file_path_sans_ext(basename(file))
  # print(file_name)
  
  # Read the CSV file into a tibble
  data <- read_csv(file)
  
  # Assign the tibble to a variable with the filename as the variable name
  assign(file_name, data)
}

#so now, every station is saved in R under the name "combined_station_[StationNumber]" e.g. combined_station_12
# and they can be adjusted as such: 
# so, to filter out only the first two meters of depth (and not if it == 0) :

file_name <- combined_station_12 #test/example for now

filtered_data <- file_name %>% 
  # filter(`Depth [m]` != 0.000) %>% 
  filter(`Depth..m.` >= 1.000) %>% 
  filter(`Depth..m.` <= 2.000)

# View(filtered_data)
# now we average the filtered data and create a table with the station number and the average salinity
#####for now the variable of interest is salinity but I am working on changing that so it can be easily edited/customized!



######## ADDED: TURN INTO A FUNCTION:
var_to_table <- function(input_var){
  variable_df <- data.frame(matrix(ncol = 2, nrow = 0)) 
  for (file in csv_files) {
    # Get the name of the file without the extension
    file_name <- tools::file_path_sans_ext(basename(file))
    # print(file_name)
    
    # Read the CSV file into a tibble
    data <- read_csv(file)
    
    # Assign the tibble to a variable with the filename as the variable name. This saves it into R to have this name
    assign(file_name, data) #not totally necessary
    
    #filter so that only data between 1 and 2 m is considered (inclusive)--THIS CAN BE ADJUSTED AS NEEDED
    filtered_data <- data %>% 
      filter(`Depth..m.` >= 1.000) %>% 
      filter(`Depth..m.` <= 2.000)
    
    mean_val <- mean(filtered_data[[input_var]]) #currently, averaging salinity values, but this can be adjusted!!! based on different interests of study
    
    station <- data$Station[1]
    
    row_input <- c(station, mean_val)
    
    variable_df <- rbind(variable_df, row_input)
  }
  # renaming the columns:
  col_name <- paste("avg", input_var, "1_2m")
  colnames(variable_df) <- c("station_number", col_name) #can edit column title based on variables and depth of interest
  # return(variable_df)
  
  #now, combine the master_df with the site_locations data: 
  locations_var <- full_join(variable_df, site_locations, by="station_number")
  return(locations_var)
  
}


# test <- var_to_table("Sal.")
# test2 <- var_to_table("Temp...degC.")

possible_vars <- colnames(combined_station_10)

#save the locations_salinity as a csv: 
# setwd("/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files")
# write.csv(locations_salinity, "locations_salinity.csv", row.names = FALSE)


#Part 2.3: function Turning enviro into map: 

translate_variable_name <- function(input_string) {
  name_map <- c(
    "Sal." = "Salinity",
    "Temp...degC." = "Temperature (degree C)",
    "pH" = "pH",
    "Chl.a..µg.l." = "chlorophyll A"
  )
  
  return(name_map[[input_string]])
}


map_variable <- function(var){
  variable <- var
  col_name <- paste("avg", var, "1_2m")
  var_data <- var_to_table(var)
  
  palette_name <- switch(
    var,
    "Sal." = "Blues",
    "Temp...degC." = "YlOrRd",
    "pH" = "Spectral",
    "Chl.a..µg.l." = "YlGn"
  )
  
  
  pal <- colorNumeric(
    palette = palette_name,
    domain = var_data[[col_name]],
    na.color = "transparent"
  )
  
  
  map_fixed_rad <- palau_polygons %>% 
    addCircles(
      data = var_data, 
      # lat = ~jittered_lat,
      # lng = ~jittered_lng,
      lat = var_data$latitude,
      lng = var_data$longitude,
      label = paste(var_data$label, paste("Station Number:", var_data$station_number), paste(translate_variable_name(variable), ":", round(var_data[[col_name]], 3)), sep= " | "), 
      radius = 9000, #can be adjusted!! 
      color = ~pal(var_data[[col_name]]),
      stroke=FALSE,
      fillOpacity = 0.8, 
    ) %>% 
    addLegend(
      position = "bottomright",    
      pal = pal,                   
      values = var_data[[col_name]],
      title = translate_variable_name(var),     
    )
  return(map_fixed_rad)
}

colnames(combined_station_10)

map_variable("Temp...degC.")

#take out species where count is zero: 
all_sp_summary <- create_species_summary()
no_zeroes_comm_import <- all_sp_summary %>% 
  filter(num_stations > 0)

comm_import_sp_vec_filtered <- comm_import_sp_vec[comm_import_sp_vec %in% no_zeroes_comm_import$species_name]

#add in common names: 
setwd(other_dir)
common_name_guide <- read.csv("common_name_guide_rv.csv")
common_name_guide <- common_name_guide %>% 
  select(Species, English_name) %>% 
  na.omit()

# join with other data about species:
comm_import_sp_vec_filtered_df <- data.frame(comm_import_sp_vec_filtered)
colnames(comm_import_sp_vec_filtered_df) <- c("Species")
eng_latin_names_comm_import <- left_join(x=comm_import_sp_vec_filtered_df, y=common_name_guide, by = "Species")
eng_latin_names_comm_import <- distinct(eng_latin_names_comm_import)

eng_latin_names_comm_import$combined <- paste(eng_latin_names_comm_import$Species, "—", eng_latin_names_comm_import$English_name)

#NEXT STEP: TURN TO VECTOR

# 
# filter_join <- function(df, meta=default_meta){
#   result <- df %>% 
#     dplyr::filter(detected == TRUE) %>% 
#     left_join(y=meta, by=c("SampleID" = "SampleID"))
#   result <- result %>% 
#     dplyr::filter(Control_or_Envtl == "Envtl") # this ensures it only accounts for only envtl data--can be changed/commented out if control data is desired
#   return(result)
# }



# Part 3: add to Shiny App: 
ui <- fluidPage(
  titlePanel("Anika's Capstone Project"),
  p(),
  strong("Mapping Environmental Data and Biodiversity in Palau's National Marine Sanctuary"),
  
  br(),
  br(),
 
  card(
    card_header("Environmental Data — May 2022"),
    p(),
    card_body(
      selectInput("env_var", "Environmental Variable:",
                  c("Salinity" = "Sal.", "Temperature" = "Temp...degC.", "pH" = "pH", "chlorophyll A" = "Chl.a..µg.l.")),
      leafletOutput("salinity_map", width = "80%", height = 600)
    )
  ),
  
  br(),
  br(), 
  br(),
  
  card(
    card_header("Species Occurrences — Nov 2021"),
    card_body(
      sidebarLayout(
        position = c("right"),
        sidebarPanel(
          checkboxGroupInput(
            "sp_choice2",
            "Species of Interest:",
            choices = comm_import_sp_vec_filtered,
            inline = FALSE,  # set to FALSE for vertical list; use TRUE if you prefer horizontal
            width = "100%"
          )
        ),
        mainPanel(
          leafletOutput("species_map", height = 600)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$salinity_map <- renderLeaflet({
    map_variable(var = input$env_var)
  })
  
  output$species_map <- renderLeaflet({
    map_multi_species(input$sp_choice2)
  })
  
}

shinyApp(ui = ui, server = server)

#confirming variables: 
# print(exists("ui"))  # Should print TRUE
# print(exists("server"))  # Should print TRUE

