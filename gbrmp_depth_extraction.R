setwd("/export/home/a-e/cani/eReefs/tran_outputs")

library('ereefs')
library('ncdf4')
library('ggplot2')
library('chron')

set.seed(23)


input_file <- substitute_filename('out_simple.nc')
gbr_bdry<- readRDS(file = "gbrmp_bdry.rds")
n=15000
rand_bdry <- gbr_bdry[sample(nrow(gbr_bdry), size=n), ]
depth = rand_bdry$depth
times_avail <- chron(chron('2012-11-30',origin=c(1990,01,01),format='y-m-d'):chron('2010-12-02',
                    origin=c(1990,01,01),format='y-m-d'),origin=c(1990,01,01),format='y-m-d')
time_rand <- sample(times_avail, size=n, replace=TRUE)

ereefs_data <- data.frame(date = time_rand,
                          depth = depth,
                          latitude = rand_bdry$lat,
                          longitude = rand_bdry$lon,
                          Tricho_I = NA*depth,
                          Tricho_Chl = NA*depth,
                          temperature = NA*depth,
                          Tricho_NR = NA*depth,
                          Tricho_PR = NA*depth,
                          Tricho_N = NA*depth,
                          salt = NA*depth,
                          PAR = NA*depth,
                          DIP = NA*depth,
                          DIN = NA*depth,
			  Nfix = NA*depth,
                          Chl_a_sum = NA*depth)

var_names <-c('Tricho_I', 'Tricho_Chl', 'temp', 'Tricho_NR', 'Tricho_PR', 'Tricho_N', 'salt', 'PAR', "DIP", "DIN", "Nfix", "Chl_a_sum")

arch <- 1:n
pb <- txtProgressBar(min = 0, max = n, style = 3)
for (i in arch) {
  ereefs_data[i, 5:dim(ereefs_data)[2]] <-  get_ereefs_ts(var_names, 
                                                          location_latlon = c(ereefs_data$latitude[i], ereefs_data$longitude[i]),
                                                          layer = 'integrated',
                                                          start_date = ereefs_data$date[i], 
                                                          end_date = ereefs_data$date[i], 
                                                          input_file = input_file,
                                                          verbosity = 0)[,-1]
  setTxtProgressBar(pb,i)
}
close(pb)

#saveRDS(ereefs_data, file = "gbr4_data.rds")

write.csv(ereefs_data, "gbr4_data.csv", row.names = F, quote = F) # write output to a csv file
