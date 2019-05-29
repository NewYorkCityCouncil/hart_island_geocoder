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

#understand distribution of nulls and of data as a whole
aggregate(age ~ year, data=df3, function(x) {sum(is.na(x))}, na.action = NULL)
table(df3$year)


#remove data before 1978 - too inconsistently reported
#remove unrealistic ages
df3 <- df3 %>%
  filter(year >= 1978 & year <= 2016) %>%
  filter(age < 115)

write_csv(df3, 'hart_island_updated_data.csv')




#new dataframe where names are corrected so that there's no overlap of points
#that have the same location but different names
df4 <- df3 %>%
  mutate(group_var = paste(round(st_coordinates(geometry)[,1], 5), round(st_coordinates(geometry)[,2], 5))) %>% 
  group_by(group_var) %>%
  count(place_of_death) %>%
  arrange(desc(n)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-n, -group_var) %>%
  st_join(x = df3,y=., left = TRUE, suffix = c("_new", "_old"))


#remove extras, rename _new
df4 <- df4[ , -which(names(df4) %in% c("group_var","place_of_death_old"))]
names(df4)[names(df4) == 'place_of_death_new'] <- 'place_of_death'

#geometry to coordinates, to eliminate issues when pulling distinct
latlon <- st_coordinates(df4)
df4 <- cbind(df4,latlon)
df4$geometry <- NULL

#distinct
df4 <- distinct(df4) %>% 
  drop_na(place_of_death, year)

write.csv(df4, 'full_data_hi.csv')

#make new dataframe with unique location and count of occurences per location

#df5 <- as.data.frame(table(df4$place_of_death, df4$year), stringsAsFactors = FALSE)

df5 <- df4 %>%
  st_as_sf(coords = c("Y", "X")) %>% 
  count(place_of_death, year) %>% 
  drop_na(place_of_death, year)
  
names(df5)[names(df5) == 'n'] <- 'count'


#add range
df5$range <- cut(as.numeric(df5$year), (0:11*4)+1977)
df5 <- merge(df5, df4[, c('place_of_death', 'year', 'X', 'Y')])
df5$range <- gsub(".*([0-9]{4}).*([0-9]{4}).*", '\\1-\\2', df5$range)



#join public hospitals df to the HI df so we can get a sense of which of those facilities are public
df5_combined <- df5 %>% 
  left_join(public_simplified, by = c('place_of_death' = 'facility_name'))

#if the facility type column is NA, it's a private hospital, facility or residence
# df5_combined$facility_type[is.na(df5_combined$facility_type)] <- 'private facility'
df5_combined$col_public[is.na(df5_combined$col_public)] <- 'Private Hospital'
df5_combined$facility_type <- NULL



#replace private with residential/other, nursing facility based on strings
df5_combined$col_public[!str_detect(tolower(df5_combined$place_of_death), 'hospital')] <- 'Residential/Other'
df5_combined$col_public[str_detect(tolower(df5_combined$place_of_death), 'nurs')] <- 'Nursing Facility'
df5_combined$col_public[str_detect(tolower(df5_combined$place_of_death), 'medical')] <- 'Private Hospital'
df5_combined$col_public[str_detect(tolower(df5_combined$col_public), 'public')] <- 'Public Hospital'


#grab only distinct columns, so there's only one instance per place of death and year
df5_combined <- distinct(df5_combined) %>% 
  drop_na(place_of_death, year)


#convert back to sf
map_data <- st_as_sf(df5_combined, coords = c('X','Y'), crs =4326)


library(htmltools)
library(htmlwidgets)

map_data <- map_data[order(map_data$range),]

pal <- colorFactor(c('#12B886', "#BE4BDB", '#228AE6', "#F59F00"), domain = unique(map_data$col_public))

pal <- colorFactor(councildown::nycc_pal()(4), domain = unique(map_data$col_public))

#c("Residential/Other", "Private Hospital",  "Nursing Facility",  "Public Hospital"

unique(map_data$col_public)
unique(df5_combined$col_public)

map <- leaflet(map_data) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(fill = TRUE, fillOpacity = .45, stroke = FALSE, 
    popup = ~place_of_death, radius = ~sqrt(count),
    color = ~pal(col_public),
    group = ~range) %>% 
  addLayersControl(baseGroups = ~unique(range), position = 'topright',
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(values = ~col_public, pal = pal) %>% 
  setView(-73.88099670410158,40.72540497175607,  zoom = 10.4) %>%
  identity()
map








map <-leaflet(map_data) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  #addMarkers(
  #clusterOptions = markerClusterOptions())
  addCircleMarkers(
    fill = TRUE, fillOpacity = .45, stroke = FALSE, 
    popup = ~place_of_death, radius = ~sqrt(count),
    color = ~pal(col_public),
    group = ~range)


map



mapno2 <- leaflet() %>% 
  addTiles()

# %>% 
#   addLayersControl(~range)

# add leaflet-timeline as a dependency
#  to get the js and css
mapno2$dependencies[[length(mapno2$dependencies)+1]] <- htmlDependency(
  name = "leaflet-timeline",
  version = "1.0.0",
  src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
  script = "javascripts/leaflet.timeline.js",
  stylesheet = "stylesheets/leaflet.timeline.css"
)


mapno2 %>%
  onRender(sprintf(
    '
    function(el,x){
    var power_data = %s;
    
    var timeline = L.timeline(power_data, {
    pointToLayer: function(data, latlng){
    var hue_min = 120;
    var hue_max = 0;
    var hue = hue_min;
    return L.circleMarker(latlng, {
    radius: 10,
    color: "hsl("+hue+", 100%%, 50%%)",
    fillColor: "hsl("+hue+", 100%%, 50%%)"
    });
    },
    steps: 1000,
    duration: 10000,
    showTicks: true
    });
    timeline.addTo(this);
    }
    ',
    map_data
  ))







class(df5$count)






g <- list(
  scope = 'new york',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray83"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p <- plot_geo(df5, locationmode = 'new york', sizes = c(1, 250), zoom = 20) %>%
  add_markers(
    x = ~X, y = ~Y,
    size = ~count, color = ~col_public, hoverinfo = "text",
    text = ~paste(df5$place_of_death, "<br />", df5$count, 'deaths')
  ) %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

p






#extract to look exclusively at deaths since beginning of 2007
ten_years <-  df3 %>% filter(death_date >= as.Date("2007-01-01"))







nrow(distinct(df5_2[c('place_of_death','year')])) - nrow(distinct(df4[c('place_of_death','year')]))

view(df4[!(duplicated(df4[c('place_of_death','year')]) | duplicated(df4[c('place_of_death','year')], fromLast = TRUE)), ])

