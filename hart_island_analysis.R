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

public_simplified <- dplyr::select(public, 1,3) 



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


# #remove data before 1978 - too inconsistently reported
# #remove unrealistic ages
# df3 <- df3 %>% 
#   filter(year >= 1978) %>% 
#   filter(age < 115)

write_csv(df3, 'hart_island_updated_data.csv')




#new dataframe where names are corrected so that there's no overlap of points
#that have the same location but different names
df4 <- df3 %>%
  mutate(group_var = paste(round(st_coordinates(geometry)[,1], 6), round(st_coordinates(geometry)[,2], 6))) %>% 
  group_by(group_var) %>%
  count(place_of_death) %>%
  arrange(desc(n)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-n, -group_var) %>%
  st_join(x = df3,y=., left = TRUE, suffix = c("_new", "_old"))

#remove extras
df4 <- df4[ , -which(names(df4) %in% c("group_var","place_of_death_old"))]

#make new dataframe with unique location and count of occurences per location

# 1973
# 1974 - 1977
# 1978 - 1981
# 1982 - 1985
# 1976 - 1989
# 1990 - 1993
# 1994 - 1997
# 1998 - 2001
# 2002 - 2005
# 2006 - 2009
# 2010 - 2013
# 2013


df5 <- as.data.frame(table(df4$place_of_death_new, df4$year), stringsAsFactors = FALSE)
names(df5) <- c("place_of_death", "year", 'count')

df5$range <- cut(as.numeric(df5$year), (0:11*4)+1970)


#joining the unique count data frame with the geo data frame pulled from the api
#adding new column temp to match the exact string
df5 <- tmpdf %>% 
  left_join(df4[, c('place_of_death_new', 'fac_type', 'year', 'geometry')])

#join public hospitals df to the HI df so we can get a sense of which of those facilities are public
df5 <- df5 %>% 
  left_join(public_simplified, by = c('place_of_death' = 'facility_name'))

#if the facility type column is NA, it's a private hospital, facility or residence
df5$facility_type[is.na(df5$facility_type)] <- 'private facility'
df5$col_public[is.na(df5$col_public)] <- 'private facility'
df5$facility_type <- NULL

#extract to look exclusively at deaths since beginning of 2007
ten_years <-  df3 %>% filter(death_date >= as.Date("2007-01-01"))





pal <- colorFactor(c("navy", "red"), domain = c("private facility", "public"))


map <- leaflet(df5) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  #addMarkers(
  #clusterOptions = markerClusterOptions())
  addCircleMarkers(
    fill = TRUE, fillOpacity = .45, stroke = FALSE, 
    popup = ~place_of_death, radius = ~sqrt(count),
    color = ~pal(col_public))
map
