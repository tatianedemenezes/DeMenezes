rm(list = ls())
library(geog4ga3)
library(tidyverse)
library(readr)
library(sf)
library(leaflet)
library(magrittr)
s <- st_read("COVID-19/shape state/state.shp")
brazil_covidm1 <- read_csv("COVID-19/brazil_covid19_m.csv")

ggplot(s) + geom_sf(fill = "gray", color = "black", alpha = .3, size = .3)
ggplot(s) + geom_sf(aes(fill = cut_number(brazil_covidm1$cases3,5)), color = "black", alpha = 1, size = .3) + scale_fill_brewer(palette = "Reds") + labs(fill = "CASOS")
ggplot(s) + geom_sf(aes(fill = cut_number(brazil_covidm1$cases2,1)), color = "black", alpha = 1, size = .3) + scale_fill_brewer(palette = "Reds") + labs(fill = "CASOS")

ggplot() + geom_point(data = brazil_covidm1 , aes(x = x_st, y = y_st, color = cases3), shape = 17, size = 3) 
ggplot() + geom_point(data = brazil_covidm1 , aes(x = x_st, y = y_st, color = cases3, size = cases3), shape = 17) 

m <- leaflet(data = brazil_covidm1 ) %>%  setView(lng = -49.2, lat = -16.6, zoom = 3)
m <- m %>% addTiles()
m <- m %>% addMarkers(~x_st, ~y_st, clusterOptions = markerClusterOptions(), group = "cases3")


ggplot(s) + geom_sf(fill = "grey", color = "black", alpha = .3, size = .3) + geom_point(data = brazil_covidm1 , aes(x = x_st, y = y_st, size = cases3), color = "black", shape = 16) + geom_point(data = brazil_covidm1 , aes(x = x_st, y = y_st, size = suspects2), color = "green", shape = 16)




rm(list = ls())
library(tidyverse)
library(readr)
CVLI_no_buffer <- read_csv("iara/CVLI no buffer.csv")
library(leaflet)
library(magrittr)
m <- leaflet(read_csv("iara/CVLI no buffer.csv")) %>%  setView(lng =  -34.8, lat =  -8.05, zoom = 16)
m <- m %>% addTiles()
m %>% addMarkers(data = CVLI_no_buffer, ~g_lon, ~g_lat)

library(geog4ga3)
library(tidyverse)
library(readr)
library(sf)
s <- st_read("iara/CentroRecife.shp")
head(s)  
class(s)
summary(s)
CVLI_no_buffer <- read_csv("iara/CVLI no buffer.csv")
ggplot(s) + geom_sf(fill = "gray", color = "black", alpha = .3, size = .3)

ggplot() + geom_point(read_csv("iara/CVLI no buffer.csv"), aes(x = g_lon, y = g_lat), color = "blue", shape = 16) + geom_sf(fill = "gray", color = "black", alpha = .3, size = .3) + geom_sf(fill = "gray", color = "black", alpha = .3, size = .3)