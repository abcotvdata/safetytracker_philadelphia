library(tidyverse)
library(readxl)

#download.file("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272019-01-01%27%20AND%20dispatch_date_time%20%3C%20%272020-01-01%27","data/source/annual/phillycrime19.csv")
#download.file("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272020-01-01%27%20AND%20dispatch_date_time%20%3C%20%272021-01-01%27","data/source/annual/phillycrime20.csv")
#download.file("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272021-01-01%27%20AND%20dispatch_date_time%20%3C%20%272022-01-01%27","data/source/annual/phillycrime21.csv")
download.file("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272022-01-01%27%20AND%20dispatch_date_time%20%3C%20%272023-01-01%27","data/source/annual/phillycrime22.csv")

philly_crime19 <- read_csv("data/source/annual/phillycrime19.csv") %>% janitor::clean_names()
philly_crime20 <- read_csv("data/source/annual/phillycrime20.csv") %>% janitor::clean_names()
philly_crime21 <- read_csv("data/source/annual/phillycrime21.csv") %>% janitor::clean_names()
philly_crime22 <- read_csv("data/source/annual/phillycrime22.csv") %>% janitor::clean_names()

past_philly_crime <- rbind(philly_crime19,philly_crime20,philly_crime21,philly_crime22)
saveRDS(past_philly_crime,"data/source/annual/past_philly_crime.RDS")
