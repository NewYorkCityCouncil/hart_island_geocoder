library(tidyverse)
library(janitor)
library(ggmap)
library(leaflet)
library(sf)
library(ggplot2)
library(viridis)
library(MASS)
library(ggridges)
library(lubridate)


#read in, make date column
full <- read_csv('full_data_hi.csv')
full[,1] <- NULL

#understand distribution of nulls
aggregate(age ~ year, data=full, function(x) {sum(is.na(x))}, na.action = NULL)

#understand distribution of nulls
null_dist <- as.data.frame(aggregate(age ~ year, data=full, function(x) {sum(is.na(x))}, na.action = NULL))
names(null_dist) <- c('year', 'count')
null_dist$status <- 'missing val'

#understand total distribution including nulls
total_dist <- as.data.frame(aggregate(age ~ year, data=full, function(x) {sum(x)}))
names(total_dist) <- c('year', 'count')
total_dist$status <- 'has val'

#combine null and total distributions
all_dists <- rbind(total_dist, null_dist)

#histogram of distributions
ggplot(all_dists, aes(x=year, fill = status)) +
  geom_histogram(binwidth = .5, position = 'identity')

ggplot(all_dists,aes(x=year)) + 
  geom_histogram(all_dists$count_total,fill = "red", alpha = 0.2) +
  geom_histogram(all_dists$count_null,fill = "blue", alpha = 0.2)


#remove data before 1978 - too inconsistently reported
#remove unrealistic ages
full <- full %>% 
  filter(year >= 1978 & year <= 2016) %>% 
  filter(age < 115)


#deaths per year line chart
ggplot(count(full,year), aes(as.numeric(year), n)) +
  geom_line()

#new data frame of deaths per year as csv
count_per_yr <- as.data.frame(count(full, year))
write_csv(count_per_yr, 'deaths_per_year_hi.csv')

# #ridges graph age per year
# ggplot(df3, aes(age, year)) +
#   geom_density_ridges() +
#   coord_flip()


#histograms of potentially key periods
p <- ggplot(full,aes(x=age)) + 
  geom_histogram(data=full[full[, 'year'] < 1991,],fill = "green", alpha = 0.3, binwidth = 1) +
  geom_histogram(data=full[full$year < 2014 & full$year >1990,],fill = "blue", alpha = 0.2, binwidth = 1) +
  geom_histogram(data=full[full[, 'year'] > 2013,],fill = "red", alpha = 0.4, binwidth = 1)
p  


#age histogram
age_hist <- ggplot(full, aes(x=age)) +
  geom_histogram()
age_hist

#create duplicate column in full for this next function
full$dummy <- full$place_of_death

#check count for place of death
place_count <- aggregate(dummy ~ place_of_death + fac_type + year, data = full, FUN = length)
names(place_count)[names(place_count)=='dummy'] <- 'count'


#replace private with residential/other, nursing facility based on strings
place_count$fac_type[!str_detect(tolower(place_count$place_of_death), 'hospital')] <- 'Residential/Other'
place_count$fac_type[str_detect(tolower(place_count$place_of_death), 'nurs')] <- 'Nursing Facility'
place_count$fac_type[str_detect(tolower(place_count$place_of_death), 'medical')] <- 'Private Hospital'
place_count$fac_type[str_detect(tolower(place_count$fac_type), 'private')] <- 'Private Hospital'
place_count$fac_type[str_detect(tolower(place_count$fac_type), 'public')] <- 'Public Hospital'





#sum counts of facility types per year
bd_fac_type <- aggregate(count ~ year + fac_type, data=place_count, FUN = sum)
names(bd_fac_type) <- c('year', 'fac_stat', 'count')

write_csv(bd_fac_type, 'facility_count_py.csv')

#plot line graph of count of each facility type per year
ggplot(bd_fac_type, aes(x=year, y =count, color = fac_stat)) +
  geom_line()

#to_csv
write_csv(bd_fac_type, 'fac_type_breakdown.csv')


#same as above but with older data source
plot_ly(df3) %>% 
  add_histogram(x = df3[df3[, 'year'] < 1991,]$age) %>%
  add_histogram(x = df3[df3$year < 2014 & df3$year >1990,]$age) %>%
  add_histogram(x = df3[df3[, 'year'] > 2013,]$age)



# Get density of points in 2 dimensions.
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


#calculate density via KDE
full$density <- get_density(
  as.numeric(
    as.POSIXct(full$death_date, format='%Y-%m-%d')
    ), full$age, n = 100)


#GAM model fitting to scatterplot  
gams <- ggplot(full, aes(x=death_date, y=age)) +
  geom_point(aes(color = density)) + 
  scale_color_viridis() +
  geom_smooth(show.legend = TRUE, n=100)
gams 

#make dataframe of quantiles
quants <- full %>%
  tbl_df() %>%
  nest(-year) %>%
  mutate(Quantiles = map(data, ~ quantile(.$age)),
         Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather() )) %>% 
  unnest(Quantiles) %>% 
  clean_names()
quants$data <- NULL
quants$year <- as.numeric(quants$year)
quants<-quants[!(quants$key=="0%" | quants$key=='100%'),]


#plot quantiles
ggplot(quants, aes(x = year, y=value, color = key)) + 
  geom_line()

#create new dataframe, side-by-side comparison of mean life expectancies
life_expec <- read.csv('us_life_expec.csv') %>% 
  filter(year >= 1978 & year <= 2016) %>% merge(quants[quants[, 'key']=='50%',])
life_expec$key <- NULL
names(life_expec)[names(life_expec)=='value'] <- 'hi_mean_age'

write_csv(life_expec, 'mean_death_age_comp.csv')


#line graph of mean age at death over time, hart island/USA comparison
mean_age_plot = ggplot() + 
  geom_line(data = life_expec, aes(x = year, y = us_mean_age), color = "blue") +
  geom_line(data = life_expec, aes(x = year, y = hi_mean_age), color = "red") +
  xlab('year') +
  ylab('avg age at death')
mean_age_plot
