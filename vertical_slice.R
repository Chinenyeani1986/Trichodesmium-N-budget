setwd("/export/home/a-e/cani/eReefs/tran_outputs")
#library(dplyr)
library(ggplot2)
library('ereefs')
library(ncdf4)

#This script extracts and plots the vertical distribution of Trichodesmium along a transect on the GBR cross-shelf waters.
slice_data = get_ereefs_slice(var_names=c('Tricho_N', 'Nfix'),
                             location_latlon=data.frame(latitude=c(-20, -20), longitude=c(150.5, 148.5)),
                             target_date = c(2011, 08, 10),
                             input_file = "out_simple.nc",
                             input_grid = "out_std.nc",
                             eta_stem = NA,
                             robust = FALSE,
                             override_positive = FALSE)

#saveRDS(slice_data, file = "shelf_slice.rds")

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

plot_ereefs_slice <- function(slice, var_name='Chl_a_sum', scale_lim=NA, var_units="") {
  numprofiles <- dim(slice$values)[2]
  layers <- length(slice$z_grid) - 1
  zmin <- array(slice$z_grid[1:layers], c(layers, numprofiles))
  zmax <- array(slice$z_grid[2:(layers+1)], c(layers, numprofiles))
  d <- rep(NA, numprofiles)
  for (i in 1:numprofiles) {
    zmin[zmin[,i]<slice$botz[i],i] <- slice$botz[i]
    zmin[zmin[,i]>slice$eta[i],i] <- slice$eta[i]
    zmax[zmax[,i]<slice$botz[i],i] <- slice$botz[i]
    zmax[zmax[,i]>slice$eta[i],i] <- slice$eta[i]
    d[i] <- earth.dist(slice$cell_intersections[i,'longitude'],slice$cell_intersections[i,'latitude'], slice$cell_intersections[i+1,'longitude'], slice$cell_intersections[i+1,'latitude']) 
  }
  
  d <- cumsum(d)
  dmin <- c(0, d[1:(numprofiles-1)])
  dmax <- d[1:numprofiles]
  dmin <- t(array(dmin, c(numprofiles, layers)))
  dmax <- t(array(dmax, c(numprofiles, layers)))
  
  ind <- which(!is.na(slice$values[, , var_name]))
  values <- slice$values[,, var_name]
  if (length(scale_lim)==1) {
    scale_lim[1] <- min(c(values[ind]))
    scale_lim[2] <- max(c(values[ind]))
  }
  
  mydata <- data.frame(xmin=dmin[ind], xmax=dmax[ind], ymin=zmin[ind], ymax=zmax[ind], z=values[ind])
  p <- ggplot2::ggplot(data=mydata,ggplot2:: aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, fill=z)) + 
    ggplot2::geom_rect() +
    ggplot2::ylab('Metres above mean surface layer') +
    ggplot2::xlab('Kilometres from start of transect')
  p = p + ggplot2::scale_fill_distiller(palette = 'Spectral',
                       na.value="transparent", 
                       guide="colourbar", 
                       limits=scale_lim, 
                       name=var_units, 
                       oob=scales::squish) + theme_bw() # + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  plot(p)
  return(p)
}

#vert_slice = readRDS("shelf_slice.rds")
p1 = plot_ereefs_slice(slice_data, var_name='Tricho_N', scale_lim=NA, var_units= expression(mg~N~m^-3)) 

ggsave(p1, file= "tricho_profile.png" )

  
