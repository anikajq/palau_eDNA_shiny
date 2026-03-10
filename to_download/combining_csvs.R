library(tidyverse)
# library(readr)

setwd("/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data")

# First, to combine all 5 csv files for each station into 1 csv file per station
#using all the raw data for now! (no averaging)

#load metadata:
file_metadata <- read.csv("AQ Copy of Labels_Coordinates_SampleInventory_May2022.xlsx - Filename metadata.csv", header=TRUE)
# View(file_metadata)

# get rid of anything (for now!) where station is not reported in the metadata: (can comment this line out later if desired)
file_metadata <- file_metadata %>% 
  filter(Station != "")

# Group by station number to combine csvs from the same station
grouped_files <- file_metadata %>%
  group_by(Station) %>%
  summarise(filenames = list(filename))

# View(grouped_files)
#grouped_files is a dataframe that has the station number in one column, and a vector with all the csv file names from that station grouped together

input_dir <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data/AAQ_raw"
output_dir <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data/combined_csvs"

# Iterate over each station group to create a new csv file for each station with each of the 5 casts combined:
for (i in seq_len(nrow(grouped_files))) {
  station <- grouped_files$Station[i]
  file_list <- grouped_files$filenames[[i]]
  
  data_list <- lapply(file_list, function(f) {
    file_path <- file.path(input_dir, f)  # Construct full path
    read.csv(file_path, header = TRUE, row.names = NULL, skip = 65, fileEncoding = "latin1") # this reads in a csv file and cuts out the first 65 rows as the header. 

  })
  
  # Standardize column names across all dataframes
  common_cols <- Reduce(intersect, lapply(data_list, colnames))  # Find common columns
  data_list <- lapply(data_list, function(df) df[common_cols])  # Keep only common columns
  
  # Combine all standardized dataframes
  combined_df <- do.call(rbind, data_list)
  combined_df$Station <- station #adds the station number as a column
  
  #save new combined csv:
  output_filename <- file.path(output_dir, paste0("combined_station_", station, ".csv"))
  
  write.csv(combined_df, output_filename, row.names = FALSE)
  
  print(paste("Saved:", output_filename))
}


##### NEXT SECTION: Using combined csvs to find trends among them (aka averaging everything) 

# Load back all csvs into R: 
folder_path <- "/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data/combined_csvs"
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# View(filtered_data)
# now we average the filtered data and create a table with the station number and the average salinity
#####for now the variable of interest is salinity but I am working on changing that so it can be easily edited/customized!

#first, initializing the data frame: 
salinity_df <- data.frame(matrix(ncol = 2, nrow = 0)) 

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
  
  mean_sal <- mean(filtered_data$Sal.) #currently, averaging salinity values, but this can be adjusted!!! based on different interests of study
  
  station <- data$Station[1]
  
  row_input <- c(station, mean_sal)
  
  salinity_df <- rbind(salinity_df, row_input)
}
# renaming the columns:
colnames(salinity_df) <- c("station_number", "avg_salinity_1_2m") #can edit column title based on variables and depth of interest

View(salinity_df)

#now we have a single master dataframe that has all the station names/numbers and the averaged salinity values

#now, combine the salinity master dataframe with the site_locations data to create a single dataframe with site locations and information as well: 
site_locations <- read.csv("site_locations.csv", header = TRUE)
# View(site_locations)
locations_salinity <- full_join(salinity_df, site_locations, by="station_number")
# View(locations_salinity)

#save the locations_salinity as a csv: 
setwd("/Users/Anika/Desktop/Stanford/senior year/directed_research/working_files/to_download/data")
write.csv(locations_salinity, "locations_salinity.csv", row.names = FALSE)
