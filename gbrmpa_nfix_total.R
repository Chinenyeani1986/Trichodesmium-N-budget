setwd("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry")
library(rgdal)
library(ggplot2)
library('ereefs')
library('ncdf4')
library(maps)
library(dplyr)
library(sp)
library(splancs)
library(tidyr)

map <- readOGR('Great_Barrier_Reef_Marine_Park_Boundary.shp')  #read gbrmpa boundary shapefile
map.df <- fortify(map)   # convert data to dataframe
map.df <- subset(map.df, select=c("long", "lat"))
input_file  <- substitute_filename("http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dnrt/gbr4_bgc_simple_2018-03.nc")
input_file1  <- substitute_filename('gbr4_bgc_H2p0_BGC3p1_7639p5_init.nc')
nc <- nc_open(input_file)
nc1 <- nc_open(input_file1)
cell_width <- ncvar_get(nc1, 'h1acell')
cell_height <- ncvar_get(nc1, 'h2acell')
botz <- ncvar_get(nc, 'botz')
latitude <- ncvar_get(nc, 'latitude')
longitude <- ncvar_get(nc, 'longitude')
nc_close(nc)
nc_close(nc1)

area =c(cell_width)*c(cell_height)
c_length = c(cell_height)
c_width = c(cell_width)
depth = c(botz)
lat = c(latitude)
long = c(longitude)
dat = data.frame(long = long, lat = lat, area, depth = depth, c_length, c_width)
dat = dat[complete.cases(dat),]

datshape <- dat[inout(dat[, 1:2], map.df, bound=TRUE), ]  #select GBR4 geolocations that are in the GBRMP boundary
p <- ggplot(dat, aes(x = long, y=lat)) + geom_point(colour="grey") + geom_polygon(data = map.df, fill = "black") + coord_map() + theme_bw() + 
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(p)
datshape <- mutate(datshape, volume = area * depth, area = area)
#saveRDS(datshape, file = "gbrmp_bdry.rds")


summer <- readRDS(file = "nfix_data_summer.rds")  #read Rdata containing total seasonal fixed-nitrogen loads
autumn <- readRDS(file = "nfix_data_autumn.rds")  
winter <- readRDS(file = "nfix_data_winter.rds")
spring <- readRDS(file = "nfix_data_spring.rds")
summer <- mutate(summer, volume = datshape$volume, nfix_total = nfix * volume, depth = datshape$depth, area = datshape$area)
winter <- mutate(winter, volume = datshape$volume, nfix_total = nfix * volume, depth = datshape$depth, area = datshape$area)
autumn <- mutate(autumn, volume = datshape$volume, nfix_total = nfix * volume, depth = datshape$depth, area = datshape$area)
spring <- mutate(spring, volume = datshape$volume, nfix_total = nfix * volume, depth = datshape$depth, area = datshape$area)
nfix_MT <- data.frame(summer = summer$nfix_total, autumn = autumn$nfix_total, winter = winter$nfix_total, spring = spring$nfix_total) * 86400 / 1e15


# reshape
nfix <- gather(nfix_MT, "season", "nfix_total")
nfix_total_MT <- sum(nfix$nfix_total)

p2 <- ggplot(nfix, aes(x=season, y=nfix_total)) + 
      geom_col(colour="cornflowerblue") + ggtitle("Total fixed nitrogen = 0.5 MT") +  theme_bw() +
      ylab("total load of nitrogen from nitrogen fixation (MT)")
ggsave(p2, file="nfix_total.png", width = 6)

#calculate total N for central GBR
central_gbr = readRDS(file = "gbr_central.rds") #read Central GBR boundary data
summer_central <- summer[inout(summer[, 2:1], central_gbr, bound=TRUE), ] #select GBR4 geolocations that are in the CentralGBR boundary
autumn_central <- autumn[inout(autumn[, 2:1], central_gbr, bound=TRUE), ]
winter_central <- winter[inout(winter[, 2:1], central_gbr, bound=TRUE), ] 
spring_central <- spring[inout(spring[, 2:1], central_gbr, bound=TRUE), ] 
nfix_MT.central <- sum(summer_central$nfix_total, autumn_central$nfix_total, winter_central$nfix_total, spring_central$nfix_total) * 86400 / 1e15
nfix_MT_central <- data.frame(summer = summer_central$nfix_total, autumn = autumn_central$nfix_total, winter = winter_central$nfix_total, spring = spring_central$nfix_total) * 86400 / 1e15
nfix.central <- gather(nfix_MT_central, "season", "nfix_total")
p_cent <- ggplot(nfix.central, aes(x=season, y=nfix_total)) + 
  geom_col(colour="cornflowerblue") + ggtitle("Total fixed nitrogen = 0.2 MT") +  theme_bw() +
  ylab("total load of nitrogen from nitrogen fixation (MT)")
ggsave(p_cent, file="nfix_total_central.png", width = 6)


#combined central GBR and GBR N fixation plot
n_sum = data.frame(Season = 'summer', nfix_total = sum(summer$nfix_total)* 86400 / 1e15, Region = "GBR")
n_aut = data.frame(Season = 'autumn', nfix_total = sum(autumn$nfix_total)* 86400 / 1e15, Region = "GBR") 
n_win = data.frame(Season = 'winter', nfix_total = sum(winter$nfix_total)* 86400 / 1e15, Region = "GBR")
n_spr = data.frame(Season = 'spring', nfix_total = sum(spring$nfix_total)* 86400 / 1e15, Region = "GBR") 
n_all = rbind(n_sum,n_aut,n_win,n_spr)
n_sum_cen = data.frame(Season = 'summer', nfix_total = sum(summer_central$nfix_total)* 86400 / 1e15, Region = "Central GBR")
n_aut_cen = data.frame(Season = 'autumn', nfix_total = sum(autumn_central$nfix_total)* 86400 / 1e15, Region = "Central GBR") 
n_win_cen = data.frame(Season = 'winter', nfix_total = sum(winter_central$nfix_total)* 86400 / 1e15, Region = "Central GBR")
n_spr_cen = data.frame(Season = 'spring', nfix_total = sum(spring_central$nfix_total)* 86400 / 1e15, Region = "Central GBR") 
n_all_cen = rbind(n_sum_cen,n_aut_cen,n_win_cen,n_spr_cen)

nfix_total_gbr <- rbind(n_all,n_all_cen)
p_cent <- ggplot(nfix_total_gbr, aes(fill=Region, y=nfix_total, x=Season)) +
  geom_bar(position='dodge', stat='identity') +
  xlab('Season') + 
  ylab('Total load of nitrogen from nitrogen fixation (MT)') +  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        legend.title=element_text(size=14),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text=element_text(size=12),
        panel.background = element_blank())
ggsave(p_cent, file="nfix_all.png", width = 8)


# select cross-shelf waterbodies
spring_inshelf <- spring[(spring$depth < 20),]
spring_midshelf <- spring[(spring$depth >= 20 & spring$depth < 40 ),]
spring_outshelf <- spring[(spring$depth >= 40 & spring$depth <= 80 ),]
summer_inshelf <- summer[(summer$depth < 20),]
summer_midshelf <- summer[(summer$depth >= 20 & summer$depth < 40 ),]
summer_outshelf <- summer[(summer$depth >= 40 & summer$depth <= 80 ),]
autumn_inshelf <- autumn[(autumn$depth < 20),]
autumn_midshelf <- autumn[(autumn$depth >= 20 & autumn$depth < 40 ),]
autumn_outshelf <- autumn[(autumn$depth >= 40 & autumn$depth <= 80 ),]
winter_inshelf <- winter[(winter$depth < 20),]
winter_midshelf <- winter[(winter$depth >= 20 & winter$depth < 40 ),]
winter_outshelf <- winter[(winter$depth >= 40 & winter$depth <= 80 ),]

#make sure the waterbodies have no intersecting points
spring_inshelf <- spring_inshelf[!inout(spring_inshelf[,2:1],spring_midshelf[2:1],bound=TRUE),]
spring_midshelf <- spring_midshelf[!inout(spring_midshelf[,2:1],spring_outshelf[2:1],bound=TRUE),]
summer_inshelf <- summer_inshelf[!inout(summer_inshelf[,2:1],summer_midshelf[2:1],bound=TRUE),]
summer_midshelf <- summer_midshelf[!inout(summer_midshelf[,2:1],summer_outshelf[2:1],bound=TRUE),]
autumn_inshelf <- autumn_inshelf[!inout(autumn_inshelf[,2:1],autumn_midshelf[2:1],bound=TRUE),]
autumn_midshelf <- autumn_midshelf[!inout(autumn_midshelf[,2:1],autumn_outshelf[2:1],bound=TRUE),]
winter_inshelf <- winter_inshelf[!inout(winter_inshelf[,2:1],winter_midshelf[2:1],bound=TRUE),]
winter_midshelf <- winter_midshelf[!inout(winter_midshelf[,2:1],winter_outshelf[2:1],bound=TRUE),]


#Calculate cross-shelf waterbodies' fixed N in kilotonnes
nfix_summer1 <- data.frame(season = 'summer', shelf = 'inner-shelf', nfix_total =sum(summer_inshelf$nfix_total) * 86400 / 1e12)
nfix_summer2 <- data.frame(season = 'summer', shelf = 'mid-shelf', nfix_total =sum(summer_midshelf$nfix_total) * 86400 / 1e12)
nfix_summer3 <- data.frame(season = 'summer', shelf = 'outer-shelf', nfix_total =sum(summer_outshelf$nfix_total) * 86400 / 1e12)
nfix_summer <- rbind(nfix_summer1, nfix_summer2, nfix_summer3)

nfix_spring1 <- data.frame(season = 'spring', shelf = 'inner-shelf', nfix_total =sum(spring_inshelf$nfix_total) * 86400 / 1e12)
nfix_spring2 <- data.frame(season = 'spring', shelf = 'mid-shelf', nfix_total =sum(spring_midshelf$nfix_total) * 86400 / 1e12)
nfix_spring3 <- data.frame(season = 'spring', shelf = 'outer-shelf', nfix_total =sum(spring_outshelf$nfix_total) * 86400 / 1e12)
nfix_spring <- rbind(nfix_spring1, nfix_spring2, nfix_spring3)

nfix_autumn1 <- data.frame(season = 'autumn', shelf = 'inner-shelf', nfix_total =sum(autumn_inshelf$nfix_total) * 86400 / 1e12)
nfix_autumn2 <- data.frame(season = 'autumn', shelf = 'mid-shelf', nfix_total =sum(autumn_midshelf$nfix_total) * 86400 / 1e12)
nfix_autumn3 <- data.frame(season = 'autumn', shelf = 'outer-shelf', nfix_total =sum(autumn_outshelf$nfix_total) * 86400 / 1e12)
nfix_autumn <- rbind(nfix_autumn1, nfix_autumn2, nfix_autumn3)

nfix_winter1 <- data.frame(season = 'winter', shelf = 'inner-shelf', nfix_total =sum(winter_inshelf$nfix_total) * 86400 / 1e12)
nfix_winter2 <- data.frame(season = 'winter', shelf = 'mid-shelf', nfix_total =sum(winter_midshelf$nfix_total) * 86400 / 1e12)
nfix_winter3 <- data.frame(season = 'winter', shelf = 'outer-shelf', nfix_total =sum(winter_outshelf$nfix_total) * 86400 / 1e12)
nfix_winter <- rbind(nfix_winter1, nfix_winter2, nfix_winter3)

nfix_all <- rbind(nfix_summer, nfix_spring, nfix_autumn, nfix_winter)
nfix_inner <- sum(nfix_summer1$nfix_total, nfix_autumn1$nfix_total, nfix_winter1$nfix_total, nfix_spring1$nfix_total)
nfix_mid <- sum(nfix_summer2$nfix_total, nfix_autumn2$nfix_total, nfix_winter2$nfix_total, nfix_spring2$nfix_total)
nfix_out <- sum(nfix_summer3$nfix_total, nfix_autumn3$nfix_total, nfix_winter3$nfix_total, nfix_spring3$nfix_total)


p3 <- ggplot(nfix_all, aes(fill=shelf, y=nfix_total, x=season)) +
    geom_bar(position='dodge', stat='identity') +
    xlab('Season') + 
    ylab('Total load of nitrogen from nitrogen fixation (KT)') +  theme_bw() + 
                                                                 theme(axis.line = element_line(colour = "black"),
                                                                       legend.title=element_text(size=14),
                                                                       axis.text=element_text(size=16),
                                                                       axis.title=element_text(size=16),
                                                                       legend.text=element_text(size=12),
                                                                       panel.background = element_blank())  
ggsave(p3, file="nfix_shelf.png")

#Calculate cross-shelf waterbodies' fixed N in kg/m^2
nfix_summer1a <- data.frame(season = 'summer', shelf = 'inner-shelf', nfix_total =sum(summer_inshelf$nfix_total)/ sum(summer_inshelf$area) * 86400/1e6)
nfix_summer2a <- data.frame(season = 'summer', shelf = 'mid-shelf', nfix_total =sum(summer_midshelf$nfix_total)/ sum(summer_midshelf$area) * 86400/1e6)
nfix_summer3a <- data.frame(season = 'summer', shelf = 'outer-shelf', nfix_total =sum(summer_outshelf$nfix_total)/ sum(summer_outshelf$area) * 86400/1e6)
nfix_summer_area <- rbind(nfix_summer1a, nfix_summer2a, nfix_summer3a)

nfix_spring1a <- data.frame(season = 'spring', shelf = 'inner-shelf', nfix_total =sum(spring_inshelf$nfix_total)/ sum(spring_inshelf$area) * 86400/1e6)
nfix_spring2a <- data.frame(season = 'spring', shelf = 'mid-shelf', nfix_total =sum(spring_midshelf$nfix_total)/ sum(spring_midshelf$area) * 86400/1e6)
nfix_spring3a <- data.frame(season = 'spring', shelf = 'outer-shelf', nfix_total =sum(spring_outshelf$nfix_total)/ sum(spring_outshelf$area) * 86400/1e6)
nfix_spring_area <- rbind(nfix_spring1a, nfix_spring2a, nfix_spring3a)

nfix_autumn1a <- data.frame(season = 'autumn', shelf = 'inner-shelf', nfix_total =sum(autumn_inshelf$nfix_total)/ sum(autumn_inshelf$area) * 86400/1e6)
nfix_autumn2a <- data.frame(season = 'autumn', shelf = 'mid-shelf', nfix_total =sum(autumn_midshelf$nfix_total)/ sum(autumn_midshelf$area) * 86400/1e6)
nfix_autumn3a <- data.frame(season = 'autumn', shelf = 'outer-shelf', nfix_total =sum(autumn_outshelf$nfix_total)/ sum(autumn_outshelf$area) * 86400/1e6)
nfix_autumn_area <- rbind(nfix_autumn1a, nfix_autumn2a, nfix_autumn3a)

nfix_winter1a <- data.frame(season = 'winter', shelf = 'inner-shelf', nfix_total =sum(winter_inshelf$nfix_total)/ sum(winter_inshelf$area) * 86400/1e6)
nfix_winter2a <- data.frame(season = 'winter', shelf = 'mid-shelf', nfix_total =sum(winter_midshelf$nfix_total)/ sum(winter_midshelf$area) * 86400/1e6)
nfix_winter3a <- data.frame(season = 'winter', shelf = 'outer-shelf', nfix_total =sum(winter_outshelf$nfix_total)/ sum(winter_outshelf$area) * 86400/1e6)
nfix_winter_area <- rbind(nfix_winter1a, nfix_winter2a, nfix_winter3a)

nfix_all_area <- rbind(nfix_summer_area, nfix_spring_area, nfix_autumn_area, nfix_winter_area)
nfix_inner_area <- sum(nfix_summer1a$nfix_total, nfix_autumn1a$nfix_total, nfix_winter1a$nfix_total, nfix_spring1a$nfix_total)
nfix_mid_area <- sum(nfix_summer2a$nfix_total, nfix_autumn2a$nfix_total, nfix_winter2a$nfix_total, nfix_spring2a$nfix_total)
nfix_out_area <- sum(nfix_summer3a$nfix_total, nfix_autumn3a$nfix_total, nfix_winter3a$nfix_total, nfix_spring3a$nfix_total)


p4 <- ggplot(nfix_all_area, aes(fill=shelf, y=nfix_total, x=season)) +
  geom_bar(position='dodge', stat='identity') +
  xlab('Season') +
  ylab(expression(Total~load~of~nitrogen~from~nitrogen~fixation~(kg~m^-2))) + theme_bw()+
                                                                              theme(axis.line = element_line(colour = "black"),
                                                                                    axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=16),
                                                                                    #legend.position="none",
                                                                                    legend.text=element_text(size=12),
                                                                                    legend.title=element_text(size=14),
                                                                                    panel.background = element_blank()) 
ggsave(p4, file="nfix_shelf_area.png")


slice_x = c(148.5,150.5)
slice_y = c(-20,-20)
slice = data.frame(long= slice_x, lat=slice_y) #transect on cross-shelf waters

#create GBR cross-shelf image
p5 = ggplot() + geom_path(data = map.df, aes(x = long,y = lat), size=1, color = "grey") +
                geom_point(spring_inshelf, mapping = aes(x = longitude, y=latitude, colour = 'inner-shelf')) +
                geom_point(spring_midshelf, mapping=aes(x = longitude, y=latitude, colour= "mid-shelf")) +
                geom_point(spring_outshelf, mapping = aes(x = longitude, y=latitude, colour= "outer-shelf")) + 
                geom_path(data = slice, aes(x = long,y = lat), size=1.5, color = "black") +
                coord_map() +
                theme_bw() + theme(panel.background = element_blank(),
                                   panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),
                                   axis.line = element_line(colour = "black"),
                                   axis.text=element_text(size=12),
                                   axis.title=element_text(size=14),
                                   legend.text=element_text(size=12),
                                   legend.position = c(0.7, 0.85)) +
                                   #legend.key.size = unit(0.5, 'cm')) +
                scale_color_manual("", 
                                   breaks = c("inner-shelf", "mid-shelf", "outer-shelf"),
                                   values = c("green", "blue", "orange"))
                  
p5

ggsave(p5, file="gbr_shelf.png")





