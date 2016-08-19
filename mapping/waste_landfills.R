
library(rgdal)
library(dplyr)
library(leaflet)
library(sp)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(tidyr)
library(webshot)

# library(devtools)
# devtools::install_github('rstudio/leaflet')

# Set working directory
setwd("C:/RWEM_processed_calvin/mapping")
dir <- "C:/RWEM_processed_calvin/mapping/EU_shp"

### shapefile for zones in Sweden
shp_EU <- readOGR(dsn = dir, layer = "Europe_Countries")


# ----- Transform to EPSG 4326 - WGS84 (required)
shp_EU <- spTransform(shp_EU, CRS("+init=epsg:4326"))

# dir <- "C:/RWEM_processed_calvin/mapping"

#### Write GeoJSON file for shp files ############################
# ----- Write data to GeoJSON

# dat <-paste(dir, "/",  ".EU_geojson", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
# writeOGR(shp_EU, dat, layer="", driver="GeoJSON")  

# read GeoJSON-------------------------------------------------------------
# shp_EU <- readOGR(".EU_geojson", "OGRGeoJSON")
# plot(shp_EU)
 
# plot(shp_EU)
# names(shp_EU)
# shp_EU@data

# import dataframe with WASTE Landfill locations
waste_landfills <- read.csv("Waste_landfills.csv")


# Build leafleft map for all Europe ##################################

popup_name <- paste0("<strong><i>",
                     waste_landfills$Landfills_visited)


EU_map <- leaflet(waste_landfills) %>%
  addTiles() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  
  addPolygons(data=shp_EU, weight=2) %>%
  

  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude,
    popup = ~popup_name,
    weight = 3, radius = 5,
    label = ~as.character(waste_landfills$ID),
    labelOptions = labelOptions(noHide = T),
    group = "landfills",
    color = "blue"
  ) %>%
  
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Toner Lite","Road map", "Hydda_Full", "Thunderforest", "Hydda_Base", "Satellite"),
    overlayGroups = c("landfills"),
    options = layersControlOptions(collapsed = TRUE)
  )

EU_map


## This is the png creation part
saveWidget(EU_map, 'EU_map_landfills.html', selfcontained = FALSE)
webshot('EU_map_landfills.html', file='EU_map_landfills.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')



## Bulgaria----------------------------------------------------------

waste_Bulgaria <- waste_landfills %>%
  filter(Member_state == "Bulgaria")

Bulgaria_map <- leaflet(waste_Bulgaria) %>%
  addTiles() %>%
 # setView(25.31, 42.87, 10) %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Esri.WorldImagery", group = "shp_EU") %>%
  
  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude,
    weight = 3, radius = 7,
    label = ~as.character(waste_Bulgaria$Landfills_visited),
    labelOptions = labelOptions(noHide = T),
    group = "landfills",
    color = "blue"
  ) %>%
  
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Road map", "Toner Lite", "Hydda_Full", "Thunderforest", "Hydda_Base", "Satellite"),
    overlayGroups = c("landfills"),
    options = layersControlOptions(collapsed = TRUE)
  )

Bulgaria_map


## This is the png creation part
saveWidget(Bulgaria_map, 'Bulgaria_map_landfills.html', selfcontained = FALSE)
webshot('Bulgaria_map_landfills.html', file='Bulgaria_map_landfills_a.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')


## Cyprus----------------------------------------------------------

waste_Cyprus <- waste_landfills %>%
  filter(Member_state == "Cyprus")

Cyprus_map <- leaflet(waste_Cyprus) %>%
  addTiles() %>%
  # setView(25.31, 42.87, 10) %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Esri.WorldImagery", group = "shp_EU") %>%
  
  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude,
    weight = 3, radius = 7,
    label = ~as.character(waste_Cyprus$Landfills_visited),
    labelOptions = labelOptions(noHide = T),
    group = "landfills",
    color = "blue"
  ) %>%
  
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Road map", "Toner Lite", "Hydda_Full", "Hydda_Base", "Satellite", "Thunderforest"),
    overlayGroups = c("landfills"),
    options = layersControlOptions(collapsed = TRUE)
  )

Cyprus_map


## This is the png creation part
saveWidget(Cyprus_map, 'Cyprus_map_landfills.html', selfcontained = FALSE)
webshot('Cyprus_map_landfills.html', file='Cyprus_map_landfills.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')



## Czech Republic----------------------------------------------------------

waste_Czech_Republic <- waste_landfills %>%
  filter(Member_state == "Czech_Republic")

Czech_Republic_map <- leaflet(waste_Czech_Republic) %>%
  addTiles() %>%
  # setView(25.31, 42.87, 10) %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Esri.WorldImagery", group = "shp_EU") %>%
  
  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude,
    weight = 3, radius = 7,
    label = ~as.character(waste_Czech_Republic$Landfills_visited),
    labelOptions = labelOptions(noHide = T),
    group = "landfills",
    color = "blue"
  ) %>%
  
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Road map", "Toner Lite", "Hydda_Full", "Hydda_Base", "Satellite", "Thunderforest"),
    overlayGroups = c("landfills"),
    options = layersControlOptions(collapsed = TRUE)
  )

Czech_Republic_map


## This is the png creation part
saveWidget(Czech_Republic_map, 'Czech_Republic_map_landfills.html', selfcontained = FALSE)
webshot('Czech Republic_map_landfills.html', file='Czech Republic_map_landfills.png', vwidth = 1300, vheight = 900,
        cliprect = 'viewport')






