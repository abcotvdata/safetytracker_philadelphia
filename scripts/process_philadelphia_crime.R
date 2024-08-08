library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(lubridate)

# load past full years' data file from our archive store
past_philly_crime <- readRDS("data/source/annual/past_philly_crime.RDS")
#past_philly_crime <- rbind(read_csv("data/source/annual/phillycrime19"), read_csv("data/source/annual/phillycrime20"), read_csv("data/source/annual/phillycrime21"), read_csv("data/source/annual/phillycrime22"))
# download the latest data for the current year to date
download.file("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272023-01-01%27%20AND%20dispatch_date_time%20%3C%20%272025-01-01%27","data/source/recent/phillycrime24.csv")
# read in the latest data for the current year to date
philly_crime24 <- read_csv("data/source/recent/phillycrime24.csv") %>% janitor::clean_names() %>% select(4:18)

# Combine past years + recent year file into single df
philly_crime <- rbind(past_philly_crime,philly_crime24)

# Remove duplicate entries and standardize key column names
philly_crime <- philly_crime %>% unique
philly_crime <- philly_crime %>% rename("district"="dc_dist","description"="text_general_code")

# Create cleaned date, month, hour columns for tracker charts
philly_crime$hour <- lubridate::hour(philly_crime$dispatch_date_time)
philly_crime$year <- lubridate::year(philly_crime$dispatch_date_time)
philly_crime$month <- lubridate::floor_date(as.Date(philly_crime$dispatch_date),"month")
philly_crime$date <- philly_crime$dispatch_date
philly_crime$dispatch_date <- NULL
philly_crime$dispatch_date_time <- NULL
philly_crime$dispatch_time <- NULL

philly_crime$category <- case_when(philly_crime$ucr_general == "100" ~ "Murder",
                                   philly_crime$ucr_general == "200" ~ "Sexual Assault",
                                   philly_crime$ucr_general == "300" ~ "Robbery",
                                   philly_crime$ucr_general == "400" ~ "Aggravated Assault",
                                   philly_crime$ucr_general == "500" ~ "Burglary",
                                   philly_crime$ucr_general == "600" ~ "Theft",
                                   philly_crime$ucr_general == "700" ~ "Motor Vehicle Theft",
                                   TRUE ~ "Other")
philly_crime$type <- case_when(philly_crime$ucr_general == "100" ~ "Violent",
                                   philly_crime$ucr_general == "200" ~ "Violent",
                                   philly_crime$ucr_general == "300" ~ "Violent",
                                   philly_crime$ucr_general == "400" ~ "Violent",
                                   philly_crime$ucr_general == "500" ~ "Property",
                                   philly_crime$ucr_general == "600" ~ "Property",
                                   philly_crime$ucr_general == "700" ~ "Property",
                                   TRUE ~ "Other")

# create separate table of crimes from the last full 12 months
philly_crime_last12 <- philly_crime %>% filter(philly_crime$date > max(philly_crime$date)-365)

# TEMP FIX FOR HOMICIDES: filter homicides out of crime data and pull last 12 months
# philly_murder <- philly_crime %>% filter(philly_crime$category == "Murder")
# philly_murder_last12 <- philly_murder %>% filter(philly_murder$date > max(philly_murder$date)-365)

# TEMP FIX FOR HOMICIDES: remove homicides from original crimes over last 12 months & add it back in
# philly_crime_last12 <- philly_crime_last12 %>% filter(philly_crime_last12$category != "Murder")
# philly_crime_last12 <- rbind(philly_crime_last12,philly_murder_last12)
  
### CITYWIDE CRIME 
### TOTALS AND OUTPUT

# Set variable of city population
# likely needs added to the tracker itself
philly_population <- 1576251

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- philly_crime %>%
  group_by(category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_detailed_last12 <- philly_crime_last12 %>%
  group_by(category,description) %>%
  summarise(last12mos = n())
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("category","description"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
rm(citywide_detailed_last12)
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total20+citywide_detailed$total21+citywide_detailed$total22
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to22 <- round(citywide_detailed$total22/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_22tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total22*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/philly_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/philly_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/philly_population*100000,1)
citywide_detailed$rate22 <- round(citywide_detailed$total22/philly_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/philly_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/philly_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- philly_crime %>%
  group_by(category,description,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")

# write to save for charts for special request BURGLARY detailed monthly
burglary_detailed_monthly <- citywide_detailed_monthly %>% 
  ungroup %>%
  filter(category=="Burglary") %>% select(2:4)
burglary_detailed_monthly <- burglary_detailed_monthly %>%
  pivot_wider(names_from = description, values_from = count)

# write to save for charts for special request BURGLARY detailed monthly
write_csv(burglary_detailed_monthly,"data/output/monthly/burglary_detailed_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- philly_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_category_last12 <- philly_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total20+citywide_category$total21+citywide_category$total22
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to22 <- round(citywide_category$total22/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_22tolast12 <- round(citywide_category$last12mos/citywide_category$total22*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/philly_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/philly_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/philly_population*100000,1)
citywide_category$rate22 <- round(citywide_category$total22/philly_population*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/philly_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/philly_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- philly_crime %>%
  group_by(category,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)

# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
citywide_category_monthly %>% filter(category=="Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Motor Vehicle Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Theft") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category=="Aggravated Assault") %>% write_csv("data/output/monthly/assaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Murder") %>% write_csv("data/output/monthly/murders_monthly.csv")

# Calculate of each type of crime CITYWIDE
citywide_type <- philly_crime %>%
  group_by(type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_type_last12 <- philly_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = n())
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total20+citywide_type$total21+citywide_type$total22
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to22 <- round(citywide_type$total22/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_22tolast12 <- round(citywide_type$last12mos/citywide_type$total22*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/philly_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/philly_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/philly_population*100000,1)
citywide_type$rate22 <- round(citywide_type$total22/philly_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/philly_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/philly_population*100000,1)

### PHILLY PD DISTRICT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
districts <- st_read("data/source/geo/philly_districts.geojson")

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_district_category <- crossing(district = unique(philly_crime$district), category = unique(philly_crime$category))
list_district_type <- crossing(district = unique(philly_crime$district), type = unique(philly_crime$type))

# Calculate total of each detailed offense type by community area
district_detailed <- philly_crime %>%
  group_by(district,category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
district_detailed <- district_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
district_detailed_last12 <- philly_crime_last12 %>%
  group_by(district,category,description) %>%
  summarise(last12mos = n())
district_detailed <- left_join(district_detailed,district_detailed_last12,by=c("district","category","description"))
rm(district_detailed_last12)
# add zeros where there were no crimes tallied that year
district_detailed[is.na(district_detailed)] <- 0
# Calculate a total across the 3 prior years
district_detailed$total_prior3years <- district_detailed$total20+district_detailed$total21+district_detailed$total22
district_detailed$avg_prior3years <- round(district_detailed$total_prior3years/3,1)
# calculate increases
district_detailed$inc_19to22 <- round(district_detailed$total22/district_detailed$total19*100-100,1)
district_detailed$inc_19tolast12 <- round(district_detailed$last12mos/district_detailed$total19*100-100,1)
district_detailed$inc_22tolast12 <- round(district_detailed$last12mos/district_detailed$total22*100-100,1)
district_detailed$inc_prior3yearavgtolast12 <- round((district_detailed$last12mos/district_detailed$avg_prior3years)*100-100,0)
# add population for beats
district_detailed <- full_join(districts,district_detailed,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_detailed$rate19 <- round(district_detailed$total19/district_detailed$population*100000,1)
district_detailed$rate20 <- round(district_detailed$total20/district_detailed$population*100000,1)
district_detailed$rate21 <- round(district_detailed$total21/district_detailed$population*100000,1)
district_detailed$rate22 <- round(district_detailed$total22/district_detailed$population*100000,1)
district_detailed$rate_last12 <- round(district_detailed$last12mos/district_detailed$population*100000,1)
# calculate a multiyear rate
district_detailed$rate_prior3years <- round(district_detailed$avg_prior3years/district_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_detailed <- district_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_detailed <- district_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each category of offense BY POLICE BEAT
district_category <- philly_crime %>%
  group_by(district,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
district_category <- left_join(list_district_category,district_category,by=c("district"="district","category"="category"))
# rename the year columns
district_category <- district_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
district_category_last12 <- philly_crime_last12 %>%
  group_by(district,category) %>%
  summarise(last12mos = n())
district_category <- left_join(district_category,district_category_last12,by=c("district","category"))
rm(district_category_last12)
# add zeros where there were no crimes tallied that year
district_category[is.na(district_category)] <- 0
# Calculate a total across the 3 prior years
district_category$total_prior3years <- district_category$total20+district_category$total21+district_category$total22
district_category$avg_prior3years <- round(district_category$total_prior3years/3,1)
# calculate increases
district_category$inc_19to22 <- round(district_category$total22/district_category$total19*100-100,1)
district_category$inc_19tolast12 <- round(district_category$last12mos/district_category$total19*100-100,1)
district_category$inc_22tolast12 <- round(district_category$last12mos/district_category$total22*100-100,1)
district_category$inc_prior3yearavgtolast12 <- round((district_category$last12mos/district_category$avg_prior3years)*100-100,0)
# add population for beats
district_category <- full_join(districts,district_category,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_category$rate19 <- round(district_category$total19/district_category$population*100000,1)
district_category$rate20 <- round(district_category$total20/district_category$population*100000,1)
district_category$rate21 <- round(district_category$total21/district_category$population*100000,1)
district_category$rate22 <- round(district_category$total22/district_category$population*100000,1)
district_category$rate_last12 <- round(district_category$last12mos/district_category$population*100000,1)
# calculate a multiyear rate
district_category$rate_prior3years <- round(district_category$avg_prior3years/district_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_category <- district_category %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_category <- district_category %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each type of crime BY POLICE BEAT
district_type <- philly_crime %>%
  group_by(district,type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every type
district_type <- left_join(list_district_type,district_type,by=c("district"="district","type"="type"))
# rename the year columns
district_type <- district_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
district_type_last12 <- philly_crime_last12 %>%
  group_by(district,type) %>%
  summarise(last12mos = n())
district_type <- left_join(district_type,district_type_last12,by=c("district","type"))
rm(district_type_last12)
# add zeros where there were no crimes tallied that year
district_type[is.na(district_type)] <- 0
# Calculate a total across the 3 prior years
district_type$total_prior3years <- district_type$total20+district_type$total21+district_type$total22
district_type$avg_prior3years <- round(district_type$total_prior3years/3,1)
# calculate increases
district_type$inc_19to22 <- round(district_type$total22/district_type$total19*100-100,1)
district_type$inc_19tolast12 <- round(district_type$last12mos/district_type$total19*100-100,1)
district_type$inc_22tolast12 <- round(district_type$last12mos/district_type$total22*100-100,1)
district_type$inc_prior3yearavgtolast12 <- round((district_type$last12mos/district_type$avg_prior3years)*100-100,0)
# add population for beats
district_type <- full_join(districts,district_type,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_type$rate19 <- round(district_type$total19/district_type$population*100000,1)
district_type$rate20 <- round(district_type$total20/district_type$population*100000,1)
district_type$rate21 <- round(district_type$total21/district_type$population*100000,1)
district_type$rate22 <- round(district_type$total22/district_type$population*100000,1)
district_type$rate_last12 <- round(district_type$last12mos/district_type$population*100000,1)
# calculate a multiyear rate
district_type$rate_prior3years <- round(district_type$avg_prior3years/district_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_type <- district_type %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_type <- district_type %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
district_detailed %>% st_drop_geometry() %>% write_csv("data/output/districts/district_detailed.csv")
district_category %>% st_drop_geometry() %>% write_csv("data/output/districts/district_category.csv")
district_type %>% st_drop_geometry() %>% write_csv("data/output/districts/district_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_district <- district_category %>% filter(category=="Murder")
sexassaults_district <- district_category %>% filter(category=="Sexual Assault")
autothefts_district <- district_category %>% filter(category=="Motor Vehicle Theft")
thefts_district <- district_category %>% filter(category=="Theft")
burglaries_district <- district_category %>% filter(category=="Burglary")
robberies_district <- district_category %>% filter(category=="Robbery")
assaults_district <- district_category %>% filter(category=="Aggravated Assault")
violence_district <- district_type %>% filter(type=="Violent")
property_district <- district_type %>% filter(type=="Property")

# Create same set of tables for citywide figures
murders_city <- citywide_category %>% filter(category=="Murder")
sexassaults_city <- citywide_category %>% filter(category=="Sexual Assault")
autothefts_city <- citywide_category %>% filter(category=="Motor Vehicle Theft")
thefts_city <- citywide_category %>% filter(category=="Theft")
burglaries_city <- citywide_category %>% filter(category=="Burglary")
robberies_city <- citywide_category %>% filter(category=="Robbery")
assaults_city <- citywide_category %>% filter(category=="Aggravated Assault")
violence_city <- citywide_type %>% filter(type=="Violent")
property_city <- citywide_type %>% filter(type=="Property")

# Using hour to identify the hours of day when murders happen
when_murders_happen <- philly_crime %>%
  filter(category=="Murder") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_district %>% st_drop_geometry() %>% write_csv("data/output/districts/murders_district.csv")
sexassaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/sexassaults_district.csv")
autothefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/autothefts_district.csv")
thefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/thefts_district.csv")
burglaries_district %>% st_drop_geometry() %>% write_csv("data/output/districts/burglaries_district.csv")
robberies_district %>% st_drop_geometry() %>% write_csv("data/output/districts/robberies_district.csv")
assaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/assaults_district.csv")
violence_district %>% st_drop_geometry() %>% write_csv("data/output/districts/violence_district.csv")
property_district %>% st_drop_geometry() %>% write_csv("data/output/districts/property_district.csv")

# TEST TEST TEST OF WHETHER RDS WILL WORK FOR TRACKERS IN AUTOMATION
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(robberies_city,"scripts/rds/retailthefts_city.rds")

#saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")

# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(philly_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# additional table exports for specific charts
# when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
#deaths <- read_excel("data/source/health/deaths.xlsx") 
#deaths <- deaths %>% filter(state=="PA")
#deaths$Homicide <- murders_city$rate_last12
#write_csv(deaths,"data/source/health/death_rates.csv")
