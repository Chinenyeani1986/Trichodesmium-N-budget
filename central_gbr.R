setwd("C:/Users/cani/OneDrive - Australian Institute of Marine Science/Desktop/gbrmpa_bdry/")
library(ncdf4) 
library(ggplot2)
library(dplyr)
library(sp) # Spatial Polygons
library(lubridate)
library(stringi)
library(stringr)
library(gridExtra)
library(readxl)
library(maptools)
library(rgdal)

#install.packages('maptools')

# Load the geographic polygons defining the regions
load("Polys.Rdata")
spatial = read.csv('spatial.csv', strip.white=TRUE)
str(Polys)

#res$area2 = sapply(slot(res,"polygons"), function(x) slot(x, "area"))
a =Polys[2]@polygons[[1]]@Polygons[[1]]@coords # enclosed mackay-whitsundays
b =Polys[5]@polygons[[1]]@Polygons[[1]]@coords # enclosed dry tropics 
c =Polys[4]@polygons[[1]]@Polygons[[1]]@coords# enclosed wet tropics
enclosed_central = rbind(a,b,c)

d =Polys[8]@polygons[[1]]@Polygons[[1]]@coords # open mackay-whitsundays
e =Polys[11]@polygons[[1]]@Polygons[[1]]@coords # open dry tropics 
f =Polys[10]@polygons[[1]]@Polygons[[1]]@coords# open wet tropics
open_central = rbind(d,f,e)

g =Polys[14]@polygons[[1]]@Polygons[[1]]@coords # midshelf mackay-whitsundays
h =Polys[17]@polygons[[1]]@Polygons[[1]]@coords # midshelf dry tropics 
i =Polys[16]@polygons[[1]]@Polygons[[1]]@coords# midshelf wet tropics
midshelf_central = rbind(g,h,i)

j =Polys[20]@polygons[[1]]@Polygons[[1]]@coords # offshore mackay-whitsundays
k =Polys[23]@polygons[[1]]@Polygons[[1]]@coords # offshore dry tropics 
l =Polys[22]@polygons[[1]]@Polygons[[1]]@coords# offshore wet tropics
offshore_central = data.frame(rbind(j,k,l))
central_gbr = data.frame(rbind(enclosed_central,open_central,midshelf_central,offshore_central))
colnames(central_gbr) = c("long","lat")

map <- readOGR('Great_Barrier_Reef_Marine_Park_Boundary.shp')  #read gbrmpa boundary shapefile
map.df <- fortify(map)   # convert data to dataframe
map.df <- subset(map.df, select=c("long", "lat"))

#select and save central gbr coordinates
gbr_central = map.df[inout(map.df,central_gbr, bound=TRUE),]
saveRDS(gbr_central, file = "gbr_central.rds")



  









