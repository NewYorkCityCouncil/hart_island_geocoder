# HART ISLAND GEOCODING

## Objective

The Hart Island dataset is a dataset of individuals buried at Hart Island. For each deceased person, it lists the location where they passed away. The objective of this project is to geocode those locations, so we may be able to see where those who have passed away came from.

This repo serves to geocode the data, manipulate it, combine it with additional datasources regarding public hospitals, and analyze trends in Hart Island burials since the data became most consistent in 1978.

Because this data relies on using Google's paid API to geocode, the council has uploaded a copy of the data to this repo, under the title "hart_island_burial_records_geocoded.csv". As a result, the file "hart_island_geocoder.R" does not need to be run in order to analyze this dataset. This data will not be regularly updated, however, and further geocoding efforts may fall on the user of this dataset.

### Packages Used

* library(tidyverse) - data frame and mutations
* library(janitor) - cleaning the column names
* library(ggmap) - geocoding
* library(leaflet) - mapping
* library(sf) - working with spatial data frames
* library(ggplot2) - visuals
* library(plotly) - visuals
* library(viridis) - colors
* library(MASS) - KDE
* library(ggridges) - visuals
* library(lubridate) - manipulate date

### Data Source
DOC Hart Island Burial Records
https://data.cityofnewyork.us/City-Government/DOC-Hart-Island-Burial-Records/c39u-es35
