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
df3 <- read_csv('hart_island_updated_data.csv')
df3$year <- format(df3$death_date, '%Y')


#remove data before 1978 - too inconsistently reported
#remove unrealistic ages
df3 <- df3 %>% 
  filter(year >= 1978) %>% 
  filter(age < 115)


#deaths per year line chart
ggplot(count(df3,year), aes(as.numeric(year), n)) +
  geom_line()

#ridges graph age per year
ggplot(df3, aes(age, year)) +
  geom_density_ridges() +
  coord_flip()


#histograms of potentially key periods
p <- ggplot(df3,aes(x=age)) + 
  geom_histogram(data=df3[df3[, 'year'] < 1991,],fill = "green", alpha = 0.3, binwidth = 1) +
  geom_histogram(data=df3[df3$year < 2014 & df3$year >1990,],fill = "blue", alpha = 0.2, binwidth = 1) +
  geom_histogram(data=df3[df3[, 'year'] > 2013,],fill = "red", alpha = 0.4, binwidth = 1)
p  

#same as above
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
df3$density <- get_density(
  as.numeric(
    as.POSIXct(df3$death_date, format='%Y-%m-%d')
    ), df3$age, n = 100)


#GAM model fitting to scatterplot  
gams <- ggplot(df3, aes(x=death_date, y=age)) +
  geom_point(aes(color = density)) + 
  scale_color_viridis() +
  geom_smooth(show.legend = TRUE, n=100)
gams 

#make dataframe of quantiles
quants <- df3 %>%
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


