setwd("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry")
library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(rgdal)
library('ncdf4')
library('ereefs')
library(grid)

#This script plots the GBR4 grid resolution, GBRMPA, Central GBR and selected Queensland towns

AUS_shp <- read_sf("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry/aust_shapefiles/aus_2016_aust_shape","AUS_2016_AUST")
towns <- data.frame(latitude = c(-15.47027987, -19.26639219, -21.15345122, -26.18916037),
                    longitude = c(145.2498605, 146.805701, 149.1655418, 152.6581893),
                    town = c('Cooktown', 'Townsville', 'Mackay', 'Gympie'))

towns1 <- data.frame(latitude = c(-19.26639219, -26.18916037),
                    longitude = c(146.805701, 152.6581893),
                    town = c('Townsville', 'Gympie'))


input_file  <- substitute_filename("http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dnrt/gbr4_bgc_simple_2018-03.nc")
nc <- nc_open(input_file)
latitude <- ncvar_get(nc, 'latitude')
longitude <- ncvar_get(nc, 'longitude')
botz <- ncvar_get(nc, 'botz')
nc_close(nc)
lat = c(latitude)
long = c(longitude)
depth = c(botz)
dat = data.frame(long = long, lat = lat, depth = depth)
dat = dat[complete.cases(dat),]
central_gbr = readRDS(file = "C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry/gbr_central.rds")
map <- readOGR('C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry/Great_Barrier_Reef_Marine_Park_Boundary.shp')  #read gbrmpa boundary shapefile
map.df <- fortify(map)   # convert data to dataframe
map.df <- subset(map.df, select=c("long", "lat"))


p = ggplot() +   
  geom_point(data= dat,aes(x = long, y=lat),colour="#E69F00") +
  geom_polygon(data = map.df, aes(x = long, y=lat), fill = "#56B4E9") +
  geom_polygon(data = central_gbr, aes(x = long, y=lat), fill = "#009E73") +
  geom_sf(data=AUS_shp ) +
  #geom_sf(data=AUS_SA2_VIC_shp,fill = "#A5D46A")+
  #ggtitle("Australian SA2 map") +
  ggplot2::geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), nudge_x=-0.1) +
  ggplot2::geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude)) +
  xlab("Longitude") +   scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) +
  xlim(142,158) + # ylim(-30,-5) +
  ylab("Latitude") + coord_sf() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  
p1 = ggplot() +   
  geom_sf(data=AUS_shp ) +
  ggplot2::geom_text(data=towns1, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), nudge_x=-0.1) +
  ggplot2::geom_point(data=towns1, ggplot2::aes(x=longitude, y=latitude)) +
  xlim(114,154) + 
  xlab("") + 
  ylab("") + coord_sf() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank())

vp_inset <- grid::viewport(width = 0.25, height = 0.25, x = 0.77, y = 0.98, just = c("right", "top"))
print(p)
print(p1, vp = vp_inset)


