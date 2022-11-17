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
districts <- st_read("data/source/geo/philadelphia_police_districts.geojson") %>% st_transform(3857)
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
districts_withpop <- st_interpolate_aw(blocks, districts, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
districts_withpop <- st_drop_geometry(districts_withpop)
# Binds that new population column to the table
districts <- cbind(districts,districts_withpop)
# Cleans up unneeded calculation file
rm(districts_withpop, blocks)

# Check total population assigned/estimated across all beats
# sum(beats$population) 

# Round the population figure; rounded to nearest thousand
districts$population <- round(districts$population,-3)

districts <- districts %>% select(8,15,16)
districts <- districts %>% rename("district"="DIST_NUMC")

# Quick define of the areas 
districts$placename <- case_when(districts$district == "01"~ "Stadium District, Lower Moyamensing, Girard Estates and Navy Yard",
                                     districts$district == "02"~ "Bustleton",
                                     districts$district == "03"~ "Whitman",
                                     districts$district == "05"~ "Andorra, Roxborough and Manayunk",
                                     districts$district == "06"~ "Center City East, Chinatown, Poplar and Society Hill",
                                     districts$district == "07"~ "Somerton",
                                     districts$district == "08"~ "Parkwood Manor, Byberry, Morrell Park and Millbrook",
                                     districts$district == "09"~ "Center City West, Logan Circle and North Philadelphia",
                                     districts$district == "12"~ "Kingsessing, Eastwick and Elmwood",
                                     districts$district == "14"~ "Germantown, Mt. Airy and Chestnut Hill",
                                     districts$district == "15"~ "Bridesburg, Frankford, Northwood and Wissimoming",
                                     districts$district == "16"~ "Powelton, Mantua and Parkside",
                                     districts$district == "17"~ "South Philadelphia and Grays Ferry",
                                     districts$district == "18"~ "University City and Cobbs Creek",
                                     districts$district == "19"~ "Overbrook, Wynnefield and Overbrook Park",
                                     districts$district == "22"~ "Strawberry Mansion and Stanton",
                                     districts$district == "24"~ "Richmond and Port Richmond",
                                     districts$district == "25"~ "Kensington and Harrowgate",
                                     districts$district == "26"~ "Fishtown",
                                     districts$district == "35"~ "Olney and Logan",
                                     districts$district == "39"~ "East Falls, Tioga and Nicetown",
                                     districts$district == "77"~ "Philadelphia airport",
                                     TRUE ~ "Other/Unknown")

districts <- districts %>% st_transform(4326)
districts <- st_make_valid(districts)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/source/geo/philly_districts.geojson")
st_write(districts,"data/source/geo/philly_districts.geojson")
saveRDS(districts,"scripts/rds/philly_districts.rds")
# add line  below when uploading data for pages
# beats <- st_read("data/source/geo/beats.geojson")

# BARE PRECINCT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,40000,60000,80000,100000,150000, Inf)
poppal <- colorBin("YlOrRd", districts$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts$district,districts$placename,prettyNum(districts$population, big.mark = ","))

philadelphia_districts_map <- leaflet(districts) %>%
  setView(-75.165, 39.9526, zoom = 10.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
philadelphia_districts_map
