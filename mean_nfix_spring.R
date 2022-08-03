setwd("/export/home/a-e/cani/eReefs/tran_outputs")
library('ereefs')
library('ncdf4')
library(ggplot2)
library('chron')

input_file  <- substitute_filename('out_simple.nc') 
gbrmpa_bdry <- read.csv("gbrmpa-bdry.csv")  
times_avail <- chron(chron('2011-09-01',origin=c(1990,01,01),format='y-m-d'):chron('2011-11-30',
                      origin=c(1990,01,01),format='y-m-d'),origin=c(1990,01,01),format='y-m-d')
nfix_total = data.frame(latitude = gbrmpa_bdry$latitude, longitude = gbrmpa_bdry$longitude, nfix= 0*gbrmpa_bdry$latitude)
nfix = data.frame(time = 0*gbrmpa_bdry$latitude, nfix = 0*gbrmpa_bdry$latitude)
for (k in 1:length(times_avail)) {
  for (i in 1:dim(gbrmpa_bdry)[1]) {
          nfix[i,] = get_ereefs_ts(c('mean_Nfix'), 
                                        location_latlon = c(gbrmpa_bdry[i,2],gbrmpa_bdry[i,1]), 
                                        layer = 'integrated',
                                        start_date = times_avail[k], 
                                        end_date = times_avail[k], 
                                        input_file = input_file,
                                        verbosity = 0)
        
  }
  nfix_total$nfix = nfix_total$nfix + nfix$nfix
}
  
saveRDS(nfix_total, file = "nfix_data_spring.rds")
  
