library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sp)
library(sf)

# GEOGRAPHY
# downloading geojson and csv of sf pd beats from city open data market
# source https://www.opendataphilly.org/dataset/police-districts
download.file("https://opendata.arcgis.com/datasets/62ec63afb8824a15953399b1fa819df2_0.geojson",
              "data/source/geo/philadelphia_police_districts.geojson")

# Read in geojson and then transform to sf format
# we will use analysis neighborhoods if the crime data comes cleanly that way
beats <- st_read("data/source/geo/philadelphia_police_districts.geojson") %>% st_transform(3857)
# beats <- st_read("data/source/sf/geo/sf_police_analysisneighborhoods.geojson") %>% st_transform(3857)


# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of SFPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "PA",
                        county = c("Philadelphia"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
beats_withpop <- st_interpolate_aw(blocks, beats, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
beats_withpop <- st_drop_geometry(beats_withpop)
# Binds that new population column to the table
beats <- cbind(beats,beats_withpop)
# Cleans up unneeded calculation file
rm(beats_withpop, blocks)

# Check total population assigned/estimated across all beats
# sum(beats$population) 

# Round the population figure; rounded to nearest thousand
beats$population <- round(beats$population,-3)

beats <- beats %>% st_transform(4326)
beats <- st_make_valid(beats)

# saving a clean geojson and separate RDS for use in tracker
st_write(beats,"data/source/geo/beats.geojson")
saveRDS(beats,"scripts/rds/beats.rds")
# add line  below when uploading data for pages
# beats <- st_read("data/source/geo/beats.geojson")



# BARE PRECINCT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,1000, 10000,25000,50000,100000, Inf)
poppal <- colorBin("YlOrRd", beats$population, bins = popbins)
poplabel <- paste(sep = "<br>", beats$district,prettyNum(beats$population, big.mark = ","))

philadelphia_beats_map <- leaflet(beats) %>%
  setView(-75.165, 39.9526, zoom = 10.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
philadelphia_beats_map
