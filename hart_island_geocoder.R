library(tidyverse)
library(janitor)
library(ggmap)
library(leaflet)
library(sf)

df <- read_csv('~/Documents/hart_island_geocode/DOC_Hart_Island_Burial_Records.csv') %>% clean_names()

#create temporary data frame with unique locations
#add new york to ensure geographic specificity
tmp <- tibble(location = paste0(unique(df$place_of_death), ", New York"))

#hit api
api <- 'AIzaSyDyhoGlMSwElIT3nz9VTL3bbVd_a2YjxGk'
register_google(key = api)

# #make data frame with lat and lon
geos <- tmp %>% mutate_geocode(location)

#save to csv so it isn't lost, in case of shutdown
write_csv(geos, 'hart_island_geos.csv')



#rename columns for join
colnames(geos)[colnames(geos)=="location"] <- "place_of_death"

#joining the keys and the full dataframe
#adding new column tmp to match the exact string
df2 <- df %>% 
  mutate(tmp = paste0(place_of_death, ", New York")) %>% 
  left_join(geos, by = c("tmp" = 'place_of_death'))

#remove tmp column
df2$tmp <- NULL

#write csv
write_csv(df2, 'hart_island_burial_records_geocoded.csv')







#check for out of state
map <- leaflet(df2) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMarkers(
    clusterOptions = markerClusterOptions())
#addCircleMarkers(fill = TRUE, fillOpacity = .45, stroke = FALSE, popup = ~place_of_death, radius = 2)
map

######determine scope of data quality issue#########

# Load in new york boundaries dataframe
city <- st_read('~/Documents/hart_island_geocode/Borough Boundaries/geo_export_f1dab35b-7e61-450a-9e6d-05e61a66f3ff.shp')

# make df2 copy spacial df
df2_copy <- df2[!is.na(df2$lat),] %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +no_defs")

#create a new logical matrix dataframe to see how many intersect
#note: because each point can intersect with a maximum of 1 borough,
#it's essentially a sparse matrix - though some columns may have no
#TRUE values at all.
tmp2 <- tibble(st_intersects(df2_copy, city, sparse = FALSE))

#create a new dataframe that checks each row of tmp 2 for at least 1 TRUE value.
#if there's at least one, it returns true. otherwise false.
tmp3 <- rowSums(tmp2) > 0L

#determining accuracy of points by taking the number of points within
#the 5 boroughs divided by total length of dataframe
table(tmp3)["TRUE"]/length(df2$last_name)


#result is 0.9756169, or ~2.5 percent of points are not in the boroughs