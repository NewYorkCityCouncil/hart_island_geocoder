library(tidyverse)
library(janitor)
library(ggmap)
library(leaflet)
library(sf)
library(plotly)


#pull in csvs again

df2 <- read_csv('hart_island_burial_records_geocoded.csv') 
df2 <- df2[!is.na(df2$lon),] %>% 
  st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326)




#pull in nyc shapefile

nyc_shape <- read_sf('Borough Boundaries/geo_export_f1dab35b-7e61-450a-9e6d-05e61a66f3ff.shp') %>% 
  st_transform(st_crs(df2)) 




#only points in the 5 boroughs. This will remove approximately 2% of the data

df3 <- df2 %>% 
  filter(st_contains(st_union(st_geometry(nyc_shape)), df2, sparse = FALSE))




#read date column to perform temporal analysis

df3$death_date <- as.Date(df3$death_date, "%m/%d/%Y")




#add in a new frame of all PUBLIC hospitals/facilities

public <- read_csv('~/Documents/hart_island_geocode/Health_and_Hospitals_Corporation__HHC__Facilities.csv') %>% 
  clean_names()




#pull only necessary columns

public_simplified <- select(public, 1,3) 



#create a new column that simply states public

col_public <- rep("public",length(public$facility_type))



#attach new column to public dataframe

public_simplified <- public_simplified %>% 
  add_column(col_public)



#change facility name to uppercase to match on Hart Island dataset

public_simplified$facility_name <- toupper(public_simplified$facility_name)



#map public simplified across HI df3

df3$fac_type <- with(df3, public_simplified$col_public[match(place_of_death, public_simplified$facility_name)])



#fill NA's with 'private' label
df3$fac_type[is.na(df3$fac_type)] <- 'private'

#add year column
df3$year <- format(df3$death_date, '%Y')


#remove data before 1978 - too inconsistently reported
#remove unrealistic ages
df3 <- df3 %>% 
  filter(year >= 1978) %>% 
  filter(age < 115)

write_csv(df3, 'hart_island_updated_data.csv')



#make new dataframe with unique location and count of occurences per location

tmp <- as.data.frame(table(df3$place_of_death), stringsAsFactors = FALSE)
names(tmp) <- c("place_of_death", "count")

#joining the unique count data frame with the geo data frame pulled from the api
#adding new column temp to match the exact string
df4 <- tmp %>% 
  mutate(temp = paste0(place_of_death, ", New York")) %>% 
  left_join(geos, by = c("temp" = 'location'))

#remove temp column
df4$temp <- NULL

#join public hospitals df to the HI df so we can get a sense of which of those facilities are public
df4 <- df4 %>% 
  left_join(public_simplified, by = c('place_of_death' = 'facility_name'))

#if the facility type column is NA, it's a private hospital, facility or residence
df4$facility_type[is.na(df4$facility_type)] <- 'private facility'
df4$col_public[is.na(df4$col_public)] <- 'private facility'

#extract to look exclusively at deaths since beginning of 2007
ten_years <-  df3 %>% filter(death_date >= as.Date("2007-01-01"))





pal <- colorFactor(c("navy", "red"), domain = c("private facility", "public"))


map <- leaflet(df4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  #addMarkers(
  #clusterOptions = markerClusterOptions())
  addCircleMarkers(
    fill = TRUE, fillOpacity = .45, stroke = FALSE, 
    popup = ~place_of_death, radius = ~sqrt(count),
    color = ~pal(col_public))
map
