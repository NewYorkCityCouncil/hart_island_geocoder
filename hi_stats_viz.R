library(tidyverse)
library(janitor)
library(ggmap)
library(leaflet)
library(sf)
library(ggplot2)
library(viridis)
library(MASS)

df3 <- read_csv('hart_island_updated_data.csv')
df3$year <- format(df3$death_date, '%Y')


#remove data before 1978 - too inconsistently reported
#remove unrealistic ages
df3 <- df3 %>% 
  filter(year >= 1978) %>% 
  filter(age < 115)



# Get density of points in 2 dimensions.
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


x <- as.POSIXct("9/27/2011  15:33:00", format="%m/%d/%Y  %H:%M:%S",tz="EST")
> as.POSIXct(as.numeric(x), origin="1970-01-01",tz="EST") # as.numeric(x)=1317155580
[1] "2011-09-27 15:33:00 EST"

df3$density <- get_density(
  as.numeric(
    as.POSIXct(df3$death_date, format='%Y-%m-%d')
    ), df3$age, n = 100)
ggplot(dat) + geom_point(aes(x, y, color = density)) + scale_color_viridis()


p <- ggplot(df3,aes(x=age)) + 
  geom_histogram(data=df3[df3[, 'year'] < 1991,],fill = "green", alpha = 0.3, binwidth = 1) +
  geom_histogram(data=df3[df3$year < 2014 & df3$year >1990,],fill = "blue", alpha = 0.2, binwidth = 1) +
  geom_histogram(data=df3[df3[, 'year'] > 2013,],fill = "red", alpha = 0.4, binwidth = 1)
p  
  
lows <- ggplot(df3, aes(x=death_date, y=age)) +
  geom_point(aes(color = density)) + 
  scale_color_viridis() +
  geom_smooth(show.legend = TRUE,)
lows 
  
plot_ly(df3) %>% 
add_histogram(x = df3[df3[, 'year'] < 1991,]$age) %>%
add_histogram(x = df3[df3$year < 2014 & df3$year >1990,]$age) %>%
add_histogram(x = df3[df3[, 'year'] > 2013,]$age)
  
  

p

df3[df3[, 'year'] < 1991,]$age
