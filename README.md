# palau_eDNA_shiny
Scripts and files used to generate Shiny app for the Palau eDNA project, part of the Stanford Center for Ocean Solutions


# Organization:
All data used in the scripts, as well as the scripts themselves, can be found in the Visual Applications folder of the Palau eDNA projects Google Drive. This is the center hub of data–as new data is added, or folders or edited, it will be updated in the Palau eDNA project drive. 
However, there is also a copy of the data in my folder in the COS drive, that contains all scripts and data that are relevant/useful as of June 11, 2025. 

# Thus, to use the app (as of June 11, 2025–not stored on a web server yet): 
Download Visual Applications folder to your computer
Open the app_v4 (cont. sidebar).R file
Uncomment the install.packages lines at the top if needed
Change the base_dir variable on line 23 to match wherever the Visual Applications folder is stored on your computer
Run all lines
Running the line at the end shinyApp(ui = ui, server = server)will open the Shiny app in another window. 
When interacting with maps, hovering over a certain data point will provide more information

# Current Functionality of the Shiny App: 
Currently, the app shows visuals for environmental variables Salinity, Temperature, Chlorophyll A, and pH
These values are from the May 2022 data collection period
It is important to note that these are uncalibrated values. As can be seen, the salinity values are quite high. 
more/different variables for environmental data can be included instead if desired.
They are shown visually as fixed radius, but they can be adjusted to instead be fixed pixels
Biodiversity data is shown for the list of commercially-important species given to us by PICRC
These data are from the Nov 2021 data collection period
Values are jiggled to help keep some level of location anonymity
Species that are not detected in this data collection period are not shown on the app (to make it a little bit cleaner to look at)

# Future Directions (functionality-wise): 
Feedback from PICRC staff
Understanding what else might be useful
Updating the environmental data for Salinity
Overlaying environmental and species data into one map
Add in more years
This will involve editing some of the cleaning scripts for the environmental data, since they currently group csvs, etc by station, not date.
Overlay them on top of each other to show changes over time 
NOAA data
Temp
Currents 
Wind/weather? 
Uploading to web server
Incorporating additional species (not just commercially-important species)?

# Smaller details/design elements:
Including species’ common names (infrastructure is there already to match common to latin name, just need to make it show in the UI)
Including images/icons
Grouping by family
Allowing a “select all” button for species selection


# Notes from Further Shiny App Work: 
Would it be helpful to include a “browse/upload” button to upload any csv file you want?
For including tabs and different pages: https://shiny.posit.co/r/layouts/ 
Action button
Uploading online
More applications of shiny: https://shiny.posit.co/r/gallery/
And generally, layout guidance:  https://mastering-shiny.org/action-layout.html 
