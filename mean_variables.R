setwd("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry")
library(ggplot2)
library(sf)
library("patchwork")
library(rgdal)

#This script plots spatially-resolved mean Trichodesmium variables

map <- readOGR('Great_Barrier_Reef_Marine_Park_Boundary.shp')  #read gbrmp boundary shapefile
map.df <- fortify(map)   # convert data to dataframe
map.df <- subset(map.df, select=c("long", "lat"))


trichoN_summer = readRDS("trichoN_summer.rds")
trichoN_spring = readRDS("trichoN_spring.rds")

temp_summer = readRDS("temp_summer.rds")
temp_spring = readRDS("temp_spring.rds")

dip_summer = readRDS("dip_summer.rds")
dip_spring = readRDS("dip_spring.rds")
towns <- data.frame(latitude = c(-15.47027987, -19.26639219, -21.15345122, -26.18916037),
                    longitude = c(145.2498605, 146.805701, 149.1655418, 152.6581893),
                    town = c('Cooktown', 'Townsville', 'Mackay', 'Gympie'))


box_bounds = c(min(trichoN_summer$x), max(trichoN_summer$x), min(trichoN_summer$y), max(trichoN_summer$y))
#scale_lim <- c(min(trichoN_summer$value, na.rm=TRUE), max(trichoN_summer$value, na.rm=TRUE))
p1 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = trichoN_summer) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(trichoN_summer$value, na.rm=TRUE), max(trichoN_spring$value, na.rm=TRUE)), 
                       name=expression(mg~N~m^-3), 
                       oob=scales::squish) + ggtitle(expression(italic(Trichodesmium))) + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(),
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

p2 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = temp_summer) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(temp_spring$value, na.rm=TRUE), max(temp_summer$value, na.rm=TRUE)), 
                       name='°C', 
                       oob=scales::squish) + ggtitle('Temperature') + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(),
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

p3 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = dip_summer) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(dip_summer$value, na.rm=TRUE), 25), 
                       name=expression(mg~P~m^-3), 
                       oob=scales::squish) + ggtitle('DIP') + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(), 
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

p = p1 + p2 + p3   #plot_layout(guides = "collect")
p

ggsave(p, file= "mean_summer.png" )


p4 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = trichoN_spring) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(trichoN_summer$value, na.rm=TRUE), max(trichoN_spring$value, na.rm=TRUE)), 
                       name=expression(mg~N~m^-3), 
                       oob=scales::squish) + ggtitle(expression(italic(Trichodesmium))) + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(),
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

p5 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = temp_spring) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(temp_spring$value, na.rm=TRUE), max(temp_summer$value, na.rm=TRUE)), 
                       name='°C', 
                       oob=scales::squish) + ggtitle('Temperature') + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(),
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

p6 <- ggplot() + geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = dip_spring) + 
  geom_path(data = map.df, aes(x = long,y = lat), size=0.5, color = "black") +
  geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), size = 3.0, nudge_x=-0.1) +
  geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude), color = "magenta") +
  scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=c(min(dip_spring$value, na.rm=TRUE), 25), 
                       name=expression(mg~P~m^-3), 
                       oob=scales::squish) + ggtitle('DIP') + 
  xlab('') + ylab('') + scale_x_continuous(breaks = c(142, 146, 150, 154, 158), limits = c(142, 157)) +
  scale_y_continuous(limits = c(-30, -7)) + coord_map(xlim = box_bounds[1:2], ylim=box_bounds[3:4]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(), 
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
                     axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position= "bottom")

P = p4 + p5 + p6   #plot_layout(guides = "collect")
P

ggsave(P, file= "mean_spring.png" )

