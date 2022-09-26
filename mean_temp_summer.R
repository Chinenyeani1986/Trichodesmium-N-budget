setwd("/export/home/a-e/cani/eReefs/tran_outputs")
library(dplyr)
library(ggplot2)
#library('ereefs')
library(ncdf4)

#' Determines whether the file provided looks like a THREDDS catalog, or an example of daily (e.g. GBR1), 
#' monthly (e.g. GBR4) or other (e.g. RECOM) netcdf file output
#'
get_ereefs_case <- function(filename) {
  # If the filename ends in ".nc.html", truncate to remove the ".html" part
  if (stringr::str_ends(filename, stringr::fixed(".nc.html"))) filename <- stringr::str_sub(filename, 1, -6)
  
  if (stringr::str_ends(filename, stringr::fixed(".nc"))) {
    ereefs_case <- "nc"
  } else if (stringr::str_ends(filename, stringr::fixed(".mnc"))) {
    ereefs_case <- "mnc"
    warn(".mnc files not yet implemented.")
  } else if (stringr::str_ends(filename, "catalog.html")) {
    ereefs_case <- c("thredds_catalog", "nci")
  } else if (stringr::str_ends(filename, ".ncml")) {
    ereefs_case <- c("ncml", "unknown")
  } else {
    stop(paste("Filename format (", filename, ") is not recognised as a netcdf or THREDDS catalog file"))
  }
  
  if (ereefs_case[1] == "nc") {
    lastfew <- stringr::str_sub(filename, start=-13, end=-3)
    dashcount <- stringi::stri_count_fixed(lastfew, '-')
    if (dashcount==2) {
      ereefs_case <- c(ereefs_case, '1km')
    } else if (dashcount==1) {
      ereefs_case <- c(ereefs_case, '4km')
    } else {
      ereefs_case <- c(ereefs_case, "recom")
    }
  }
  return(ereefs_case)
}

get_ereefs_case("out_simple.nc")[2]


#' Returns the 'stem' of a filename (i.e. the filename with the date and extension cut off the end)
get_file_stem <- function(filename) {
  case <- get_ereefs_case(filename)
  if (case[1]!='nc') {
    return(NA)
  }
  if (case[2]=="recom") {
    # A netcdf filename, but not obviously 1km or 4km eReefs. Might be RECOM or another application
    file_stem <- substr(filename, start=1, stop=stringi::stri_locate_last(filename, regex='.nc')[1]-1)
  } else if (case[2]=='1km') {
    file_stem <- substr(filename, start=1, stop=stringi::stri_locate_last(filename, regex='.nc')[1]-11)
  } else {
    # must be 4km
    file_stem <- substr(filename, start=1, stop=stringi::stri_locate_last(filename, regex='.nc')[1]-8)
  }
  return(file_stem)
}


#' Opens the netcdf file, input_file, and extracts the origin (reference date/time) and time dimension.
#'
get_origin_and_times <- function(input_file, as_chron="TRUE") {
  nc <- nc_open(input_file)
  if (!is.null(nc$var[['t']])) { 
    posix_origin <- stringi::stri_datetime_parse(ncdf4::ncatt_get(nc ,'t'), "'days since 'yyyy-MM-dd HH:mm:ss")[1]
    ereefs_origin <- as.numeric(as.Date('1990-01-01') - as.Date(posix_origin), origin=c(year=1990, month=1, day=1))-1 
    ds <- ncvar_get(nc, "t") 
  } else { 
    posix_origin <- stringi::stri_datetime_parse(ncdf4::ncatt_get(nc ,'time'), "'days since 'yyyy-MM-dd HH:mm:ss")[1]
    ereefs_origin <- as.numeric(as.Date('1990-01-01') - as.Date(posix_origin), origin=c(year=1990, month=1, day=1))-1 
    ds <- ncvar_get(nc, "time") 
  }
  ncdf4::nc_close(nc) 
  if (as_chron) {
    ereefs_origin <- ereefs_origin + chron::chron('1990-01-01', origin=c(year=1990, month=1, day=1), format='y-m-d')
    ds <- ds + ereefs_origin
  } else {
    ds <- as.Date(ds, origin = as.Date("1990-01-01"))
  }
  return(list(ereefs_origin, ds))
}


#' Returns an appropriate x_grid, y_grid and z_grid
#'
get_ereefs_grids <- function(filename, input_grid=NA) {
  if (!is.na(input_grid)) {
    # The user has provided the name of a netcdf file from which to read the grids
    nc <- nc_open(input_grid)
    x_grid <- ncvar_get(nc, 'x_grid')
    y_grid <- ncvar_get(nc, 'y_grid')
    z_grid <- ncvar_get(nc, 'z_grid')
    ncdf4::nc_close(nc)
  } else {
    nc <- nc_open(filename)
    if (!is.null(nc$var[['x_grid']])) { 
      # filename is in standard EMS netcdf output format. x_grid, y_grid and z_grid are provided.
      x_grid <- ncvar_get(nc, 'x_grid')
      y_grid <- ncvar_get(nc, 'y_grid')
      z_grid <- ncvar_get(nc, 'z_grid')
    } else {
      # simple format netcdf file. grids are not provided. Check the dimensions to work out 
      # whether it looks like GBR4 or GBR1 and if so, load x-grid and y_grid from saved data. 
      # Otherwise, the user must provide another filename (input_grid) that contains x_grid, 
      # y_grid and z_grid. 
      if (!is.null(nc$dim[['i']])) {
        ilen <- nc$dim[['i']][['len']]
        jlen <- nc$dim[['j']][['len']]
        klen <- nc$dim[['k']][['len']]
      } else {
        ilen <- nc$dim[['i_grid']][['len']]
        jlen <- nc$dim[['j_grid']][['len']]
        klen <- nc$dim[['k_grid']][['len']]
      }
      if ((ilen==510)&(jlen==2389)) {
        x_grid <- gbr1_x_grid
        y_grid <- gbr1_y_grid
        z_grid <- gbr1_z_grid
      } else if ((ilen==600)&(jlen==180)) {
        x_grid <- gbr4_x_grid
        y_grid <- gbr4_y_grid
        z_grid <- gbr4_z_grid
      } else {
        stop(paste('x_grid, y_grid and z_grid not found and grid size not recognised as GBR1 or GBR4.',
                   'Plotting using cell centres (latitude and longitude) has not yet been implemented.',
                   'Please specify input_grid (a standard format EMS file or other netcdf file that ',
                   'contains x_grid, y_grid and z_grid).'))
        
      }
    }
    ncdf4::nc_close(nc)
  } 
  return(list(x_grid=x_grid, y_grid=y_grid, z_grid=z_grid))
}


#' Check whether the given filename is a shortcut and if so, set the full filename
#'
substitute_filename <- function(input_file = "menu") {
  choices <- NA
  if ((input_file == "menu")|(input_file == "old_menu")|(input_file == "choices") | is.numeric(input_file)) {
    # Set up the menu of choices to display for interactive input, or to select from if the user has specified an option number
    if (input_file == "old_menu") {
      choices  <- c("GBR4-v2.0",
                    "GBR4_BGC-v2.0 Chyb Dcrt",
                    "GBR4_BGC-v2.0 Chyb Dnrt",
                    "GBR4_BGC-v2.0 Cpre Dcrt",
                    "GBR4 rivers_2.0  Dnrt   GBR4 passive",
                    "GBR1-v2.0",
                    "GBR1 rivers_2.0  Dnrt",
                    "GBR4-v1.85",
                    "GBR4_BGC-v926",
                    "GBR4_BGC-v924",
                    "GBR1-v1.71",
                    "GBR1_BGC-v924",
                    "GBR4_BGC-v3.0 Dcrt",
                    #"GBR4_BGC-v3.0 Dnrt",
                    "GBR4_BGC-v3.1",
                    "GBR4_BGC-v3.0",
                    "GBR1_BGC-v3p2surf",
                    "GBR1_BGC-v3p2",
                    "GBR4_BGC-v3p2nrtsurf",
                    "GBR4_BGC-v3p2nrt",
                    "menu")
    } else {
      # Get the list of eReefs data sevices from the NCI server:
      services <- thredds::tds_list_datasets("https://dapds00.nci.org.au/thredds/catalogs/fx3/catalog.html")
      # Trim the list to only show the catalogues
      services <- services[which(services$type=="catalog"), ]
      choices <- services$dataset
      #choices[length(choices) + 1] <- "menu"
      # I'm probably missing something, but the following returns paths that will work:
      paths <- stringr::str_replace(services$path, "catalogs/fx3//thredds/", "") 
    }
  }
  if ((stringr::str_ends(input_file, "catalog.html"))|((stringr::str_starts(input_file, "http")&(stringr::str_ends(input_file, "/"))))) {
    services <- thredds::tds_list_datasets(input_file)
    # Trim the list to only show the catalogues
    services <- services[which(services$type=="catalog"), ]
    choices <- services$dataset
    #choices[length(choices) + 1] <- "menu"
    # I'm probably missing something, but the following returns paths that will work:
    paths <- stringr::str_replace(services$path, "catalogs/fx3//thredds/", "") 
  } else if (input_file == "choices") {
    # The user just wants a list of options
    print(choices)
    stop()
  } else if (input_file=="old_menu") {
    selection <- utils::menu(c("Latest release 4km grid hydrodynamic model (Sept 2010-pres.)", 
                               "Archived 4km biogeochemical model hindcast v2.0 (Sept 2010 - Oct 2016)",
                               "Archived 4km biogeochemical model near real time v2.0 (Oct 2016 - Nov 2019)",
                               "Archived Pre-industrial catchment scenario 4km BGC (Sept 2010 - Oct 2016)",
                               "Latest release passive river tracers (Sept 2010 - pres.)",
                               "Latest release 1km grid hydrodynamic model (Dec 2014 - pres.)",
                               "Latest release 1km grid passive river tracers (Dec 2014 - pres.)",
                               "Archived 4km hydro (v 1.85, Sept 2010-pres.)",
                               "Archived 4km bgc (v926, Sept 2010 - Dec 2014)",
                               "Archived 4km bgc (v924, Sept 2010 - Sept 2017)",
                               "Archived 1km hydro (v 1.71, Dec 2014 - Apr 2016)",
                               "Archived 1km bgc (v924, Dec 2014 - Nov 2019)",
                               "Archived 4km biogeochemical model hindcast v3.0 (Dec 2010 - Oct 2018)",
                               "Latest release 4km biogeochemical model v3.1 (Dec 2010 - Apr 2019)",
                               "CSIRO login required: GBR1 NRT BGC 3p0 3D (2018-09-02 to 2019-01-30)",
                               "CSIRO login required: Latest release GBR1 NRT BGC 3p2 surface (16 Oct 2019 - pres., 3x/day)",
                               "CSIRO login required: Latest release GBR1 NRT BGC 3p2 3D (16 Oct 2019 - pres., daily)",
                               "CSIRO login required: Latest release GBR4 NRT BGC surface (Oct 2019 - May 2020, 4x/day)",
                               "CSIRO login required: Latest release GBR4 NRT BGC 3D (Oct 2019 - May 2020, daily)"
    ))
    input_file <- choices[selection]
  } else if ((input_file == "menu")|(input_file == length(choices))) {
    # We are using the NCI catalog to provide options
    print("Refer to https://research.csiro.au/ereefs/models/models-about/biogeochemical-simulation-naming-protocol/ for naming conventions.")
    selection <- utils::menu(choices)
    input_file  <- paths[selection]
  }
  if (is.numeric(input_file)) {
    input_file <- choices[input_file]
  }
  # Perhaps the user has manually specified (or selected from old_menu) one of the (obsolescent) official run labels from ereefs.info
  input_file <- dplyr::case_when(
    input_file == "GBR4-v2.0" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_v2/gbr4_simple_2018-10.nc",
    input_file == "GBR4_BGC-v2.0 Chyb Dcrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dcrt/gbr4_bgc_simple_2016-07.nc",
    input_file == "GBR4_BGC-v2.0 Chyb Dnrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dnrt/gbr4_bgc_simple_2017-11.nc",
    input_file == "GBR4_BGC-v2.0 Cpre Dcrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Cpre_Dcrt/gbr4_bgc_simple_2016-06.nc",
    input_file == "GBR4 rivers_2.0  Dnrt   GBR4 passive" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_2.0_rivers/gbr4_rivers_simple_2018-07.nc",
    input_file == "GBR1-v2.0" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1_2.0/gbr1_simple_2018-09-23.nc",
    input_file == "GBR1 rivers_2.0  Dnrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1_2.0_rivers/gbr1_rivers_simple_2018-03.nc",
    input_file == "GBR4-v1.85" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4/gbr4_simple_2016-03.nc.html",
    input_file == "GBR4_BGC-v926" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_926/gbr4_bgc_simple_2014-11.nc",
    input_file == "GBR4_BGC-v924" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_924/gbr4_bgc_simple_2016-09.nc",
    input_file == "GBR1-v1.71" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1/gbr1_simple_2016-03-25.nc",
    input_file == "GBR1_BGC-v924" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1_bgc_924/gbr1_bgc_simple_2018-08-21.nc",
    input_file == "GBR4_BGC-v3.0 Dcrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B3p0_Chyd_Dcrt/gbr4_bgc_all_simple_2018-10.nc",
    input_file == "GBR4_BGC-v3.1" ~ "https://regional-models.ereefs.info/thredds/dodsC/GBR4_H2p0_B3p1_Cq3b_Dhnd/all/gbr4_bgc_all_simple_2012-10.nc",
    input_file == "GBR4_BGC-v3.0" ~ "http://oa-62-cdc.it.csiro.au:8087/opendap/cache/gbr1/bgc/3.0/all/gbr1_bgc_all_2018-09-02.nc",
    input_file == "GBR1_BGC-v3p2surf" ~ "http://oa-62-cdc.it.csiro.au:8087/opendap/cache/gbr1/bgc/nrt/surf/gbr1_bgc_surf_2019-10-16.nc",
    input_file == "GBR1_BGC-v3p2" ~ "http://oa-62-cdc.it.csiro.au:8087/opendap/cache/gbr1/bgc/nrt/all/gbr1_bgc_all_2019-10-16.nc",
    input_file == "GBR4_BGC-v3p2nrtsurf" ~ "http://oa-62-cdc.it.csiro.au:8087/opendap/cache/gbr4/bgc/nrt/gbr4_bgc_surf_2019-10.nc",
    input_file == "GBR4_BGC-v3p2nrt" ~ "http://oa-62-cdc.it.csiro.au:8087/opendap/cache/gbr4/bgc/nrt/gbr4_bgc_all_2019-10.nc",
    # additional shortcuts
    input_file == "GBR4HD" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_v2/gbr4_simple_2018-10.nc",
    input_file == "hd" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_v2/gbr4_simple_2018-10.nc",
    input_file == "GBR4BGC" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dcrt/gbr4_bgc_simple_2016-07.nc",
    input_file == "bgc" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dcrt/gbr4_bgc_simple_2016-07.nc",
    input_file == "GBR4NRT" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dnrt/gbr4_bgc_simple_2017-11.nc",
    input_file == "nrt" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_bgc_GBR4_H2p0_B2p0_Chyd_Dnrt/gbr4_bgc_simple_2017-11.nc",
    input_file == "rivers" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr4_2.0_rivers/gbr4_rivers_simple_2018-07.nc",
    input_file == "GBR1HD" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1_2.0/gbr1_simple_2018-09-23.nc",
    input_file == "GBR1BGC" ~ "http://dapds00.nci.org.au/thredds/dodsC/fx3/gbr1_bgc_924/gbr1_bgc_simple_2018-08-21.nc",
    TRUE ~ input_file
  )
  # If necessary, remove trailing ".html" from an input file name that is not a catalog.
  if (stringr::str_ends(input_file, ".nc.html")) input_file <- stringr::str_sub(input_file, 1, -6)
  
  return (input_file)
}

# Calculate the number of days in the month of the specified date
daysIn <- function (d) {
  fom <- as.Date(cut(d, "month"))
  fomp1 <- as.Date(cut(fom+32, "month"))
  return(as.integer(fomp1 - fom))
}

#' Calculate and return the plume optical class from eReefs model output..
plume_class <- function(rsr) {
  xdim <- nrow(rsr[[1]])
  ydim <- ncol(rsr[[1]])
  cl <- list(c(0.0064, 0.0093, 0.0147, 0.0242, 0.0286, 0.0245, 0.0240, 0.0050),
             c(0.0032, 0.0046, 0.0097, 0.0160, 0.0188, 0.0113, 0.0113, 0.0027),
             c(0.0031, 0.0043, 0.0092, 0.0151, 0.0178, 0.0105, 0.0105, 0.0025),
             c(0.0027, 0.0037, 0.0076, 0.0119, 0.0137, 0.0064, 0.0063, 0.0014),
             c(0.0040, 0.0045, 0.0064, 0.0065, 0.0062, 0.0013, 0.0012, 0.0002),
             c(0.0054, 0.0057, 0.0071, 0.0055, 0.0045, 0.0005, 0.0005, 0.0001),
             c(0.0130, 0.0110,  0.0070, 0.0040, 0.0030, 0.0010, 0.0010, 0.0010))
  
  rms <- array(0, dim=c(xdim, ydim, 7, 7))
  
  # Inefficient looped code. Vectorise this if it is too slow.
  for (i in 1:7) {
    for (j in 1:7) {
      rms[,,i,j] <- cl[[i]][j] - rsr[[j]]
    }
  }
  rms <- rms^2
  rmse <- rowSums(rms, dim=3)
  rmse[is.na(rmse)] <- 999999
  plume_class <- apply(rmse, c(1,2), which.min)
  plume_class[is.na(rsr[[1]])] <- NA
  return(plume_class)
}


map_ereefs_movie <- function(var_name = "true_colour", 
                             start_date = c(2015,12,1), 
                             end_date = c(2016,3,31), 
                             layer = 'surface', 
                             output_dir = 'ToAnimate', 
                             Land_map = FALSE, 
                             input_file = "menu",
                             input_grid = NA, 
                             scale_col = c('ivory', 'coral4'), 
                             scale_lim = c(NA, NA), 
                             zoom = 6, 
                             box_bounds = c(NA, NA, NA, NA), 
                             suppress_print=FALSE, 
                             stride = 'daily',
                             verbosity=0, 
                             label_towns = TRUE,
                             strict_bounds = FALSE,
                             mark_points = NULL,
                             gbr_poly = FALSE,
                             add_arrows = FALSE,
                             max_u = NA,
                             scale_arrows = NA,
                             show_bathy=FALSE,
                             contour_breaks=c(5,10,20))
{
  plot_eta <- FALSE
  input_file <- substitute_filename(input_file)
  if (verbosity > 1) print(paste('After substitute_filename() input_file = ', input_file))
  
  # Check whether this is a locally-stored netcdf file or a web-served file
  if (substr(input_file, 1, 4)=="http") {
    local_file = FALSE
  } else local_file = TRUE
  
  # Check whether this is a GBR1 or GBR4 ereefs file, a THREDDS catalog or something else
  ereefs_case <- get_ereefs_case(input_file) 
  if (verbosity > 1) print(paste('ereefs_case = ', ereefs_case[1], ereefs_case[2]))
  if (ereefs_case[2]=='1km') warning('Assuming that only one timestep is output per day/file') # find matching commented warning to fix this
  input_stem <- get_file_stem(input_file)
  if (verbosity > 1) print(paste('input_stem = ', input_stem))
  #check_platform_ok(input_stem)
  
  towns <- data.frame(latitude = c(-15.47027987, -16.0899, -16.4840, -16.92303816, -19.26639219, -20.0136699, -20.07670986, -20.40109791, -21.15345122, -22.82406858, -23.38031858, -23.84761069, -24.8662122, -25.54073075, -26.18916037),
                      longitude = c(145.2498605, 145.4622, 145.4623, 145.7710, 146.805701, 148.2475387, 146.2635394, 148.5802016, 149.1655418, 147.6363616, 150.5059485, 151.256349, 152.3478987, 152.7049316, 152.6581893),
                      town = c('Cooktown', 'Cape Tribulation', 'Port Douglas', 'Cairns', 'Townsville', 'Bowen', 'Charters Towers', 'Prosperine', 'Mackay', 'Clermont', 'Rockhampton', 'Gladstone', 'Bundaberg', 'Maryborough', 'Gympie'))
  
  # Dates to map:
  if (is.vector(start_date)) {
    if ((length(start_date==2)) && is.character(start_date[1])) { 
      start_date <- chron::chron(start_date[1], start_date[2], format=c('d-m-y', 'h:m:s'), 
                                 origin=c(year=1990, month=1, day=1))
    } else if (length(start_date==3)) { 
      # Set time to midday
      if (verbosity > 1) {
        print('Setting time to midday. start_date = ')
        print(start_date)
      }
      start_date <- chron::chron(paste(start_date[3], start_date[2], start_date[1], sep = '-'), 
                                 "12:00:00", format=c('d-m-y', 'h:m:s'), 
                                 origin=c(year=1990, month=1, day=1))
    } else if (length(start_date==4)) {
      if (!is.character(start_date[4])) start_date[4] <- paste0(start_date[4], ':00')
      start_date <- chron::chron(paste(start_date[3], start_date[2], start_date[1], sep = '-'), 
                                 start_date[4], format=c('d-m-y', 'h:m:s'), 
                                 origin=c(year=1990, month=1, day=1)) 
    } else {
      stop("start_date format not recognised")
    }
  } else if (is.character(start_date)) {
    start_date <- chron::chron(start_date, "12:00:00", format=c('d-m-y', 'h:m:s'), 
                               origin=c(year=1990, month=1, day=1))
  } else if (class(start_date)[1] == "Date") {
    start_date <- chron::as.chron(start_date)
  }
  start_day <- as.integer(chron::days(start_date))
  start_tod <- as.numeric(start_date) - as.integer(start_date)
  start_month <- as.integer(months(start_date))
  start_year <- as.integer(as.character(chron::years(start_date)))
  
  if (is.vector(end_date)) {
    if (length(end_date==2) && is.character(end_date[1])) { 
      end_date <- chron::chron(end_date[1], end_date[2], format=c('d-m-y', 'h:m:s'), 
                               origin=c(year=1990, month=1, day=1))
    } else if (length(end_date==3)) { 
      # Set time to midday
      end_date <- chron::chron(paste(end_date[3], end_date[2], end_date[1], sep = '-'), "12:00:00", format=c('d-m-y', 'h:m:s'), 
                               origin=c(year=1990, month=1, day=1))
    } else if (length(end_date==4)) {
      if (!is.character(end_date[4])) end_date[4] <- paste0(end_date[4], ':00')
      end_date <- chron::chron(paste(end_date[3], end_date[2], end_date[1], sep = '-'), end_date[4], format=c('d-m-y', 'h:m:s'), 
                               origin=c(year=1990, month=1, day=1)) 
    } else {
      stop("end_date format not recognised")
    }
  } else if (is.character(end_date)) {
    end_date <- chron::chron(end_date, "12:00:00", format=c('d-m-y', 'h:m:s'), 
                             origin=c(year=1990, month=1, day=1))
  } else if (class(end_date)[1] == "Date") {
    end_date <- chron::as.chron(end_date)
  }
  end_day <- as.integer(chron::days(end_date))
  end_month <- as.integer(months(end_date))
  end_year <- as.integer(as.character(chron::years(end_date)))
  
  if (start_date > end_date) {
    stop('start_date must preceed end_date')
  }
  
  if (!is.null(mark_points)) {
    # If mark_points is a vector, change it into a data frame
    if (is.null(dim(mark_points))) {
      mark_points <- data.frame(latitude = mark_points[1], longitude = mark_points[2])
    }
    eta_data <- get_ereefs_ts(var_name='eta', input_file = input_file, start_date=start_date, end_date = end_date, location_latlon = mark_points)
    names(eta_data) <- c('date', 'eta')
    eta_plot <- ggplot2::ggplot(eta_data, ggplot2::aes(x=date, y=eta)) + ggplot2::geom_line() + ggplot2::ylab('surface elevation (m)')
    plot_eta <- TRUE
  }
  
  if (suppress_print) Land_map <- FALSE
  
  if (start_year==end_year) {
    mths <- start_month:end_month
    years <- rep(start_year, length(mths))
  } else if ((start_year + 1) == end_year) {
    mths <- c(start_month:12, 1:end_month)
    years <- c(rep(start_year, 12 - start_month + 1), rep(end_year, end_month))
  } else {
    mths <- c(start_month:12, rep(1:12, end_year - start_year - 1), 1:end_month)
    years <- c(rep(start_year, 12 - start_month + 1), 
               rep((start_year + 1) : (end_year - 1), each=12),
               rep(end_year, end_month))
  }
  
  
  # Allow for US English:
  if (var_name == "true_color") {
    var_name <- "true_colour"
  }
  
  # Main routine
  ndims <- 0
  icount <- 0
  mcount <- 0
  pb <- txtProgressBar(min = 0, max = 1, style = 3)
  for (month in mths) {
    mcount <- mcount + 1
    year <- years[mcount]
    
    if (mcount == 1) {
      from_day <- start_day
      if (ereefs_case[2] == "4km") { 
        input_file <- paste0(input_stem, format(as.Date(paste(year, month, 1, sep="-")), '%Y-%m'), '.nc')
      } else if (ereefs_case[2] == "1km") {
        input_file <- paste0(input_stem, format(as.Date(paste(year, month, from_day, sep="-")), '%Y-%m-%d'), '.nc')
      } else if (ereefs_case[1] == "thredds_catalog") {
        if (verbosity != 70) { # Hidden option to use if we are repeatedly querying the same catalog for the same time-period but different variables
          # (We are currently in map_ereefs_movie())
          catalog_list <- thredds::tds_list_datasets(input_file)
          catalog_list <- catalog_list[stringr::str_ends(catalog_list$path, "nc"), ]$path
          catalog_list <- stringr::str_replace(catalog_list, "catalog/.*dataset=fx3-", stringr::fixed("dodsC/fx3/"))
          
          # Special case to remove unwanted additional files from certain catalogues
          in_range <- stringr::str_which(catalog_list, "recom_wc", negate=TRUE)
          catalog_list <- catalog_list[in_range]
          catalog_startdates <- chron::chron(rep(1, length(catalog_list)), origin=c(year=1990,month=1,day=1), format="y-m-d")
          catalog_enddates <- catalog_startdates
          
          # Pare down the catalog_list to include only those that cover the date range of interest, and set up a list of
          # the times of outputs in each of these relevant files. This is slow.
          # We could save this time by saving the results to sysdata.rda so we can look them up for known catalogs and only 
          # doing this if we encounter an unknown catalog. Alternatively, we could save some time by assuming that the list is
          # in temporal order and only looking for the first and last file needed.
          if (verbosity>0) print("Finding out which files in the catalog are relevant to the time range...")
          catalog_times <- vector("list", length(catalog_list))
          
          in_range <- rep(FALSE, length(catalog_list))
          for (i in 1:length(catalog_list)) {
            if (verbosity >1) print(paste("File", i, catalog_list[i]))
            catalog_times[[i]] <- get_origin_and_times(catalog_list[i])[[2]]
            catalog_startdates[i] <- catalog_times[[i]][1]
            catalog_enddates[i] <- catalog_times[[i]][length(catalog_times[[i]])]
            dum1 <- length(which((catalog_times[[i]] >= start_date)&(catalog_times[[i]] <= end_date)))
            if (dum1 >0) {
              in_range[i] <- TRUE
            }
          }
          
          # Let's make sure these are in temporal order. Note that sort.chron() ignores index.return so we need to convert to numeric.
          ix <- sort(as.numeric(catalog_startdates[in_range]), index.return=TRUE)$ix
          ix <- which(in_range)[ix]
          catalog_list <- catalog_list[ix]
          catalog_times <- catalog_times[ix]
          catalog_startdates <- catalog_startdates[ix]
          catalog_enddates <- catalog_enddates[ix]
          save(file='tmp.rda', list=c('catalog_list', 'catalog_startdates', 'catalog_enddates', 'catalog_times')) 
        } else {
          load('tmp.rda')
        }
        icatalog <- 1
        input_file <- catalog_list[icatalog]
        ds <- catalog_times[[icatalog]]
      } else {
        #input_file <- input_file
        ds<- get_origin_and_times(input_file)[[2]]
      }
      grids <- get_ereefs_grids(input_file, input_grid)
      x_grid <- grids[['x_grid']]
      y_grid <- grids[['y_grid']]
      
      # Allow user to specify a depth below MSL by setting layer to a negative value
      if (layer<=0) {
        z_grid <- grids[['z_grid']]
        layer <- which.min(z_grid<layer)
      }
      
      dims <- dim(x_grid) - 1
      
      # Work out which parts of the grid are within box_bounds and which are outside
      outOfBox <- array(FALSE, dim=dim(x_grid))
      if (!is.na(box_bounds[1])) {
        outOfBox <- apply(x_grid,2,function(x){ (x<box_bounds[1]|is.na(x)) } )
      }
      if (!is.na(box_bounds[2])) {
        outOfBox <- outOfBox | apply(x_grid,2,function(x){(x>box_bounds[2]|is.na(x))})
      }
      if (!is.na(box_bounds[3])) {
        outOfBox <- outOfBox | apply(y_grid,2,function(x){(x<box_bounds[3]|is.na(x))})
      }
      if (!is.na(box_bounds[4])) {
        outOfBox <- outOfBox | apply(y_grid,2,function(x){(x>box_bounds[4]|is.na(x))})
      }
      
      # Find the subset of x_grid and y_grid that is inside the box and crop the grids
      # to the box_bounds
      if (is.na(box_bounds[1])) { 
        xmin <- 1
      } else {
        xmin <- which(apply(!outOfBox, 1, any))[1]
        if (length(xmin)==0) xmin <- 1
      }
      if (is.na(box_bounds[2])) {
        xmax <- dims[1]
      } else {
        xmax <- which(apply(!outOfBox, 1, any))
        xmax <- xmax[length(xmax)]
        if ((length(xmax)==0)|(xmax > dims[1])) xmax <- dims[1]
      }
      if (is.na(box_bounds[3])) { 
        ymin <- 1
      } else {
        ymin <- which(apply(!outOfBox, 2, any))[1]
        if (length(ymin)==0) ymin <- 1
      }
      if (is.na(box_bounds[4])) {
        ymax <- dims[2]
      } else {
        ymax <- which(apply(!outOfBox, 2, any))
        ymax <- ymax[length(ymax)]
        if ((length(ymax)==0)|(ymax > dims[2])) ymax <- dims[2]
      }
      
      x_grid <- x_grid[xmin:(xmax+1), ymin:(ymax+1)]
      y_grid <- y_grid[xmin:(xmax+1), ymin:(ymax+1)]
      
      nc <- nc_open(input_file)
      if (is.null(nc$var[['latitude']])) {
        # Standard EMS output file
        latitude <- ncvar_get(nc, "y_centre")
        longitude <- ncvar_get(nc, "x_centre")
        botz <- ncvar_get(nc, 'botz')
      } else { 
        # Simple format netcdf file
        latitude <- ncvar_get(nc, "latitude")
        longitude <- ncvar_get(nc, "longitude")
        botz <- NULL
        if (show_bathy) warning('Can not show bathymetry: simple format netcdf file does not contain botz')
      }
      ncdf4::nc_close(nc)
      
      if (add_arrows) {
        idim <- dim(latitude)[1]
        jdim <- dim(latitude)[2]
        max_arrow <- max(max(abs(longitude[idim, jdim] - longitude[1,1])/idim), max(abs(latitude[idim, jdim] - latitude[1,1])/jdim))
      }
      
      # Set up the polygon corners. 4 per polygon.
      a <- xmax - xmin + 1
      b <- ymax - ymin + 1
      
      gx <- c(x_grid[1:a, 1:b], x_grid[2:(a+1), 1:b], x_grid[2:(a+1), 2:(b+1)], x_grid[1:a, 2:(b+1)])
      gy <- c(y_grid[1:a, 1:b], y_grid[2:(a+1), 1:b], y_grid[2:(a+1), 2:(b+1)], y_grid[1:a, 2:(b+1)])
      gx <- array(gx, dim=c(a*b,4))
      gy <- array(gy, dim=c(a*b,4))
      
      # Find and exclude points where not all corners are defined
      gx_ok <- !apply(is.na(gx),1, any)
      gy_ok <- !apply(is.na(gy),1, any)
      gx <- c(t(gx[gx_ok&gy_ok,]))
      gy <- c(t(gy[gx_ok&gy_ok,]))
      longitude <- c(longitude)[gx_ok&gy_ok]
      latitude <- c(latitude)[gx_ok&gy_ok]
      
    } else {
      from_day <- 1
      start_tod <- 0
    }
    
    if ((start_year==end_year)&&(start_month==end_month)) {
      day_count <- end_day - start_day + 1
    } else if (mcount == 1) {
      day_count <- daysIn(as.Date(paste(year, month, 1, sep='-'))) - start_day + 1
    } else if (mcount == (length(mths))) {
      day_count <- end_day
    } else {
      day_count <- daysIn(as.Date(paste(year, month, 1, sep='-')))
    }
    
    if (ereefs_case[2] == '4km') { 
      fileslist <- 1
      input_file <- paste0(input_stem, format(as.Date(paste(year, month, 1, sep="-")), '%Y-%m'), '.nc')
      ds <- get_origin_and_times(input_file)[[2]]
      if ((ds[length(ds)] - ds[1]) > 31.5) warning('Filename looks like a monthly output file (i.e. contains two dashes) but file contains more than a month of data.')
      if ((ds[length(ds)] - ds[1]) < 27) warning('Filename looks like a monthly output file (i.e. contains two dashes) but file contains less than a month of data.')
      if(ds[2]==ds[1]) stop(paste('Error reading time from', input_file, '(t[2]==t[1])'))
      tstep <- as.numeric(ds[2]-ds[1])
      day_count <- day_count / tstep
      if (day_count > length(ds)) {
        warning(paste('end_date', end_date, 'is beyond available data. Ending at', ds[length(ds)]))
        day_count <- length(ds)
      }
      #dum1 <- as.integer((from_day - 0.4999999)/tstep + 1)
      #dum2 <- as.integer((day_count - 1) / tstep) +1
      #ds <- ds[seq(from=dum1, by=as.integer(1/tstep), to=(dum1+dum2))]
      #start_array <- c(xmin, ymin, dum1)
      #count_array <- c(xmax-xmin, ymax-ymin, dum2)
      ds <- ds[from_day:day_count]
      start_array <- c(xmin, ymin, from_day)
      count_array <- c(xmax-xmin, ymax-ymin, day_count)
      fileslist <- 1
    } else if (ereefs_case[2] == '1km') { 
      fileslist <- from_day:(from_day+day_count-1)
      from_day <- 1
      day_count <- 1
      
      input_file <- paste0(input_stem, format(as.Date(paste(year, month, from_day, sep="-")), '%Y-%m-%d'), '.nc')
      ds <- get_origin_and_times(input_file, as_chron="FALSE")[[2]]
      dum1 <- which.min(abs(as.numeric(ds - (from_day + 0.4999999))))
      start_array <- c(xmin, ymin, dum1) 
      count_array <- c(xmax-xmin, ymax-ymin, dum1)
      tstep <- 1
    } else if ((ereefs_case[2] == "recom")|(ereefs_case[1] == "ncml")) {
      # Everything is in one file but we are only going to read a month at a time
      # Output may be more than daily, or possibly less
      # input_file <- paste0(input_stem, '.nc') # input_file has been set previously 
      tstep <- as.numeric(ds[2]-ds[1])
      day_count <- day_count / tstep
      if (day_count > length(ds)) {
        warning(paste('end_date', end_date, 'is beyond available data. Ending at', ds[length(ds)]))
        day_count <- length(ds)
      }
      from_day <- (as.numeric(chron::chron(paste(year, month, from_day, sep = '-'), format=c('y-m-d'),
                                           origin=c(year=1990, month=1, day=1)) - ds[1]) + 
                     start_tod) / tstep + 1 
      if (from_day<1) from_day <-1
      start_array <- c(xmin, ymin, from_day)
      count_array <- c(xmax-xmin, ymax-ymin, as.integer(day_count/tstep))
      fileslist <- 1
    } else if (ereefs_case[1] == "thredds_catalog") { 
      tstep <- as.numeric(ds[2]-ds[1])
      month_startdate <- chron::chron(paste(year, month, 1, sep = '-'), format = 'y-m-d',
                                      origin=c(year=1990, month=1, day=1))
      month_enddate <- chron::chron(paste(year, month, daysIn(as.Date(paste(year, month, 1, sep='-'))) , sep = '-'), format = 'y-m-d',
                                    origin=c(year=1990, month=1, day=1))
      ix <- which((catalog_startdates >= month_startdate) & (catalog_enddates <= (month_enddate + 0.999)))
      icatalog <- ix[1]
      fileslist <- catalog_list[ix]
      if (end_date > catalog_enddates[length(catalog_enddates)]) {
        warning(paste('end_date', end_date, 'is beyond available data. Ending at', catalog_enddates[length(catalog_enddates)]))
      }
    } else stop("Shouldn't happen: ereefs_case not recognised")
    
    if (stride == 'daily') {
      if (tstep <= 1.0) {
        stride <- 1/tstep
      } else {
        stop("Minimum timestep in netcdf file is greater than daily.")
      }
    } 
    stride <- as.integer(stride)
    
    for (dcount in 1:length(fileslist)) {
      if (ereefs_case[2] == '1km') { 
        input_file <- paste0(input_stem, format(as.Date(paste(year, month, fileslist[dcount], sep="-")), '%Y-%m-%d'), '.nc') 
        #ds <- as.Date(paste(year, month, fileslist[dcount], sep="-", '%Y-%m-%d'))
      } else if (ereefs_case[1] == "thredds_catalog") {
        icatalog <- ix[dcount]
        input_file <- catalog_list[icatalog]
        ds <- catalog_times[[icatalog]]
        if (length(ds)>1) {
          tstep <- as.numeric(ds[2] - ds[1])
          first_day <- as.integer((as.numeric(chron::chron(paste(year, month, from_day, sep = '-'), format='y-m-d',
                                                           origin=c(year=1990, month=1, day=1)) - catalog_startdates[icatalog]) +
                                     start_tod) / tstep + 1 )
          if (first_day<1) first_day <- 1
          last_day <- min(length(ds) - first_day, as.integer(day_count/tstep))
          if (last_day <1) last_day <- 1
        } else {
          tstep <- 1
          first_day <- 1
          last_day <- 1
        }
        start_array <- c(xmin, ymin, first_day)
        count_array <- c(xmax-xmin, ymax-ymin, last_day)
      }
      if (verbosity>0) print(input_file)
      if (var_name=="plume") {
        if (!local_file) {
          slice <- paste0('[', start_array[3]-1, ':', stride, ':', start_array[3] + count_array[3] - 2, ']', # time
                          '[', start_array[2]-1, ':', start_array[2] + count_array[2] - 1, ']', # y
                          '[', start_array[1]-1, ':', start_array[1] + count_array[1] - 1, ']') # x
          input_file <- paste0(input_file, '?R_412', slice, ',R_443', slice, ',R_488', slice, ',R_531', slice, ',R_547', slice, ',R_667', slice, ',R_678', slice)
        } else input_file <- input_file
        nc <- nc_open(input_file)
        R_412 <- ncvar_get(nc, "R_412")
        R_443 <- ncvar_get(nc, "R_443")
        R_488 <- ncvar_get(nc, "R_488")
        R_531 <- ncvar_get(nc, "R_531")
        R_547 <- ncvar_get(nc, "R_547")
        R_667 <- ncvar_get(nc, "R_667")
        R_678 <- ncvar_get(nc, "R_678")
        if (local_file) {
          R_412 <- R_412[(start_array[1] - 1) : (start_array[1] + count_array[1] - 1),
                         (start_array[2] - 1) : (start_array[2] + count_array[2] - 1),
                         seq(from = start_array[3] - 1, to = start_array[3] + count_array[3] - 2, by = stride)]
        }
        ems_var <- NA*R_678
        if (ereefs_case[2] == '4km') {
          for (day in 1:dim(R_412)[3]) {
            rsr <- list(R_412[,,day], R_443[,,day], R_488[,,day], R_531[,,day], R_547[,,day], R_667[,,day], R_678[,,day])
            ems_var[,,day] <- plume_class(rsr)
          }
        } else {
          rsr <- list(R_412, R_433, R_488, R_532, R_547, R_668, R_678)
          ems_var <- plume_class(rsr)
        }
        dims <- dim(ems_var)
        var_longname <- "Plume optical class"
        var_units <- ""
      } else if (var_name=="true_colour") {
        slice <- paste0('[', start_array[3]-1, ':', stride, ':', start_array[3] + count_array[3] - 2, ']', # time
                        '[', start_array[2]-1, ':', start_array[2] + count_array[2] - 1, ']', # y
                        '[', start_array[1]-1, ':', start_array[1] + count_array[1] - 1, ']') # x
        if (!local_file) {
          input_file <- paste0(input_file, '?R_470', slice, ',R_555', slice, ',R_645', slice)
        } else input_file <- input_file
        nc <- nc_open(input_file)
        TCbright <- 10
        R_470 <- ncvar_get(nc, "R_470") * TCbright
        R_555 <- ncvar_get(nc, "R_555") * TCbright
        R_645 <- ncvar_get(nc, "R_645") * TCbright
        R_470[R_470>1] <- 1
        R_555[R_555>1] <- 1
        R_645[R_645>1] <- 1
        
        unscaledR = c(0, 30, 60, 120, 190, 255)/255;
        scaledR = c(1, 110, 160, 210, 240, 255)/255;
        scalefun <- approxfun(x=unscaledR, y=scaledR, yleft=1, yright=255)
        red <- scalefun(R_645)
        green <-scalefun(R_555)
        blue <- scalefun(R_470)
        red[is.na(red)] <- 0
        green[is.na(green)] <- 0
        blue[is.na(blue)] <- 0
        
        ems_var <- rgb(red, green, blue)
        ems_var[ems_var=="#000000"] <- NA
        ems_var <-array(as.character(ems_var), dim=dim(R_645))
        dims <- dim(ems_var)
        
        var_longname <- "Simulated true colour"
        var_units <- ""
        
      } else { 
        
        if (ndims == 0) {
          nc <- nc_open(input_file)
          # We don't yet know the dimensions of the variable, so let's get them
          if (var_name == "speed") {
            dims <- nc$var[['u']][['size']]
          } else if (var_name == "ZooT") {
            dims <- nc$var[['ZooL_N']][['size']]
          } else { 
            dims <- nc$var[[var_name]][['size']]
          }
          if (is.null(dims)) stop(paste(var_name, ' not found in netcdf file.')) 
          ndims <- length(dims)
          # If there's only one layer (e.g. a surf.nc file) then we want to reduce ndims accordingly, but not if there is only one time-step
          if ((length(dims[dims!=1])!=ndims)&&(dims[length(dims)]!=1)) ndims <- ndims - 1
          if ((ndims > 3) && (layer == 'surface')) layer <- dims[3]
          ncdf4::nc_close(nc)
        }
        
        if (verbosity>1) {
          print(paste('variable has ', ndims, 'dimensions'))
          print('start_array = ')
          print(start_array)
          print('count_array = ')
          print(count_array)
        }
        
        if (ndims == 4) {
          slice <- paste0('[', start_array[3]-1, ':', stride, ':', start_array[3] + count_array[3] - 2, ']', # time
                          '[', layer-1, ']',                                                    # layer
                          '[', start_array[2]-1, ':', start_array[2] + count_array[2] - 1, ']', # y
                          '[', start_array[1]-1, ':', start_array[1] + count_array[1] - 1, ']') # x
        } else {
          slice <- paste0('[', start_array[3]-1, ':', stride, ':', start_array[3] + count_array[3] - 2, ']', # time
                          '[', start_array[2]-1, ':', start_array[2] + count_array[2] - 1, ']', # y
                          '[', start_array[1]-1, ':', start_array[1] + count_array[1] - 1, ']') # x
        }
        if (verbosity>1) print(paste('slice = ', slice))
        if (var_name == "speed") { 
          if (!local_file) {
            input_file <- paste0(input_file, '?u', slice, ',v', slice)
          } else input_file <- input_file
          nc <- nc_open(input_file)
          ems_var <- sqrt(ncvar_get(nc, 'u')^2 + ncvar_get(nc, 'v')^2)
          vat <- ncdf4::ncatt_get(nc, 'u')
          var_longname <- 'Current speed'
          var_units <- vat$units
        } else if (var_name == "ZooT") {
          if (!local_file) {
            input_file <- paste0(input_file, '?ZooL_N', slice, ',ZooS_N', slice)
          } else input_file <- input_file
          nc <- nc_open(input_file)
          ems_var <- ncvar_get(nc, 'ZooL_N') + ncvar_get(nc, 'ZooS_N')
          vat <- ncdf4::ncatt_get(nc, 'ZooL_N')
          var_longname <- 'Total Zooplankton'
          var_units <- vat$units
        } else {
          if (!local_file) {
            if (add_arrows) {
              input_file <- paste0(input_file, '?',var_name, 'u', 'v', slice)
            } else {
              input_file <- paste0(input_file, '?',var_name, slice)
            }
          } else input_file <- input_file
          if (verbosity>1) print(paste("Before nc_open, input_file = ", input_file))
          nc <- nc_open(input_file)
          ems_var <- ncvar_get(nc, var_name)
          if (add_arrows) {
            current_u <- ncvar_get(nc, 'u1')
            current_v <- ncvar_get(nc, 'u2')
            if ((dcount==1)&(is.na(max_u))) {
              max_u <- max(max(abs(c(current_u)), na.rm = TRUE), max(abs(c(current_v)), na.rm=TRUE)) 
              if (!is.na(scale_arrows)) max_u <- max_u / scale_arrows
            }
          }
          vat <- ncdf4::ncatt_get(nc, var_name)
          var_longname <- vat$long_name
          var_units <- vat$units
        }
      }
      if (local_file) { 
        if (ndims == 3) {
          ems_var <- ems_var[start_array[1] : (start_array[1] + count_array[1]),
                             start_array[2] : (start_array[2] + count_array[2]),
                             seq(from = start_array[3], to = start_array[3] + count_array[3] - 1, by = stride)] 
        }  else {
          ems_var <- ems_var[start_array[1] : (start_array[1] + count_array[1]),
                             start_array[2] : (start_array[2] + count_array[2]),
                             layer,
                             seq(from = start_array[3], to = start_array[3] + count_array[3] - 1, by = stride)] 
        }
        ds <- ds[seq(from = start_array[3], to = start_array[3] + count_array[3] - 1, by = stride)] 
        if (add_arrows) {
          current_u <- current_u[start_array[1] : (start_array[1] + count_array[1]),
                                 start_array[2] : (start_array[2] + count_array[2]),
                                 seq(from = start_array[3], to = start_array[3] + count_array[3] - 1, by = stride)] 
          current_v <- current_v[start_array[1] : (start_array[1] + count_array[1]),
                                 start_array[2] : (start_array[2] + count_array[2]),
                                 seq(from = start_array[3], to = start_array[3] + count_array[3] - 1, by = stride)] 
        }
      }
      #ds <- as.Date(ncvar_get(nc, "time"), origin = as.Date("1990-01-01"))
      
      dum1 <- length(dim(ems_var))
      if (dum1==2) {
        ems_var <- array(ems_var, dim=c(dim(ems_var), 1))
        if (add_arrows) {
          current_u <- array(current_u, dim=c(dim(current_u), 1))
          current_v <- array(current_v, dim=c(dim(current_v), 1))
        }
      }
      if (add_arrows) {
        del_u <- current_u / max_u * max_arrow
        del_v <- current_v / max_u * max_arrow
      }
      
      ncdf4::nc_close(nc)
      
      d <- dim(ems_var)[3]
      n_all <- {}
      for (jcount in 1:d) {
        ems_var2d <- ems_var[, , jcount]
        if (add_arrows) {
          del_u2d <- del_u[, , jcount]
          del_v2d <- del_v[, , jcount]
        }
        # Values associated with each polygon at chosen timestep
        n <- c(ems_var2d)[gx_ok&gy_ok]
        if (icount==0) {
          if (var_name=='true_colour') {
            temporal_sum <- col2rgb(n)^2
            n_all[[1]] <- col2rgb(n)^2
          } else { 
            temporal_sum <- n
            n_all[[1]] <- n
          }
        } else {
          if (var_name=='true_colour') {
            temporal_sum <- col2rgb(n)^2 + temporal_sum
            n_all[[icount+1]] <- col2rgb(n)^2
          } else { 
            temporal_sum <- temporal_sum + n
            n_all[[icount+1]] <- n
          }
        }
        
        # Unique ID for each polygon
        id <- as.factor(1:length(n))
        values <- data.frame(id = id, value = n)
        positions <- data.frame(id=rep(id, each=4), x = gx, y = gy)
        datapoly <- merge(values, positions, by = c("id"))
        
        if (!suppress_print) {
          if ((var_name!="true_colour")&&(is.na(scale_lim[1]))) { 
            scale_lim <- c(min(n, na.rm=TRUE), max(n, na.rm=TRUE))
          }
          
          if (Land_map) {
            p <- ggplot2::gplot() + geom_polygon(data = map.df, colour = "black", fill="lightgrey", size=0.5, aes(x = long, y=lat, group=group))
          } else {
            p <- ggplot2::ggplot()
          }
          p <- p + ggplot2::geom_polygon(ggplot2::aes(x=x, y=y, fill=value, group=id), data = datapoly)
          if (var_name=="true_colour") { 
            p <- p + ggplot2::scale_fill_identity()
          } else if (scale_col[1] == 'spectral') { 
            p <- p + ggplot2::scale_fill_distiller(palette = 'Spectral',
                                                   na.value="transparent", 
                                                   guide="colourbar", 
                                                   limits=scale_lim, 
                                                   name=var_units, 
                                                   oob=scales::squish) 
          } else if (length(scale_col)<3) { 
            if (length(scale_col) == 1) scale_col <- c('ivory', scale_col)
            p <- p + ggplot2::scale_fill_gradient(low=scale_col[1],
                                                  high=scale_col[2],
                                                  na.value="transparent", 
                                                  guide="colourbar",
                                                  limits=scale_lim,
                                                  name=var_units,
                                                  oob=scales::squish)
          } else {
            p <- p + ggplot2::scale_fill_gradient2(low=scale_col[1],
                                                   mid=scale_col[2],
                                                   high=scale_col[3],
                                                   na.value="transparent", 
                                                   midpoint=(scale_lim[2] - scale_lim[1])/2,
                                                   space="Lab",
                                                   guide="colourbar",
                                                   limits=scale_lim,
                                                   name=var_units,
                                                   oob=scales::squish)
          }
          
          if (add_arrows) {
            arrow_df <- data.frame(latitude = c(latitude), longitude = c(longitude), uend = c(del_u2d) + c(longitude), vend = c(del_v2d) + c(latitude))
            p <- p + ggplot2::geom_segment(arrow_df, mapping = ggplot2::aes(x = longitude, y = latitude, xend = uend, yend = vend), arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm")))
          }
          if ((show_bathy)&!is.null(botz)) {
            bathy_df <- data.frame(latitude = c(latitude), longitude = c(longitude), depth = c(-botz))
            reg <- marmap::griddify(bathy_df, as.integer(idim/2), as.integer(jdim/2))
            bathy <- marmap::as.bathy(reg)
            bathy_df <- marmap::as.xyz(bathy)
            names(bathy_df) <- c('latitude', 'longitude', 'depth')
            p <- p + ggplot2::geom_contour(bathy_df, mapping = ggplot2::aes(x = longitude, y = latitude, z = depth), colour='black', breaks=contour_breaks)
          }
          
          if (label_towns) {
            towns <- towns[towns$latitude>=min(gy, na.rm=TRUE),]
            towns <- towns[towns$latitude<=max(gy, na.rm=TRUE),]
            towns <- towns[towns$longitude>=min(gx, na.rm=TRUE),]
            towns <- towns[towns$longitude<=max(gx, na.rm=TRUE),]
            if (dim(towns)[1]>0) p <- p + ggplot2::geom_text(data=towns, ggplot2::aes(x=longitude, y=latitude, label=town, hjust="right"), nudge_x=-0.1) +
              ggplot2::geom_point(data=towns, ggplot2::aes(x=longitude, y=latitude))
          }
          #p <- p + ggplot2::ggtitle(paste(var_longname, format(chron::chron(as.double(ds[jcount])+0.000001), "%Y-%m-%d %H:%M")))
          p <- p + ggplot2::ggtitle(paste(var_longname, format(ds[jcount], format=c('d-m-yyyy', 'h:m'))))
          p <- p + ggplot2::xlab("longitude") + ggplot2::ylab("latitude")
          if (!is.null(mark_points)) {
            p <- p + ggplot2::geom_point(data=mark_points, ggplot2::aes(x=longitude, y=latitude), shape=4)
            if (plot_eta) {
              dind <- which(ds[jcount]==eta_data$date)
              p2 <- eta_plot + ggplot2::geom_point(data = eta_data[dind,], ggplot2::aes(x=date, y=eta), size=2, color='red')
            }
          }
          if (gbr_poly) {
            p <- p + ggplot2::geom_path(data=sdf.gbr, ggplot2::aes(y=lat, x=long, group=group)) 
          }
          if (all(is.na(box_bounds))) { 
            p <- p + ggplot2::coord_map(xlim = c(min(gx, na.rm=TRUE), max(gx, na.rm=TRUE)), ylim = c(min(gy, na.rm=TRUE), max(gy, na.rm=TRUE)))
          } else {
            p <- p + ggplot2::coord_map(xlim = box_bounds[1:2], ylim = box_bounds[3:4]) + 
              ggplot2::theme(panel.border = ggplot2::element_rect(linetype = "solid", colour="grey", fill=NA))
          }
          
          icount <- icount + 1
          
          if (plot_eta) {
            p <- cowplot::plot_grid(p, p2, ncol=1, rel_heights=c(4,1), axis='lr', align='v')
          }
          
          if (!file.exists(output_dir)) {
            dir.create(output_dir)
          }
          fname <- paste0(output_dir, '/', var_name, '_', 100000 + icount, '.png', collapse='')
          ggplot2::ggsave(fname, p, dpi=100)
          #rm('p')
        }  else {
          icount <- icount + 1
          p <- NULL
        }
        setTxtProgressBar(pb,icount/as.integer(end_date-start_date)/tstep*stride)
      } # end jcount loop
    } # end fileslist loop
  } # end month loop
  close(pb)
  if (var_name=='true_colour') {
    temporal_sum <- rgb(sqrt(temporal_sum)/icount, maxColorValue=255)
    values <- data.frame(id = id, value = temporal_sum)
  } else {
    values <- data.frame(id = id, value = temporal_sum/icount)
  }
  datapoly <- merge(values, positions, by = c("id"))
  return(list(p=p, all_values=n_all, datapoly=datapoly, longitude=longitude, latitude=latitude))
  
}

datapoly1 = map_ereefs_movie (var_name = c("temp"), 
                             start_date = c(2010,12,1), 
                             end_date = c(2011,01,31), 
                             layer = 'surface', 
                             output_dir = 'ToAnimate', 
                             Land_map = FALSE, 
                             input_file = "out_simple.nc",
                             input_grid = "out_std.nc", 
                             scale_col = c('ivory', 'coral4'), 
                             scale_lim = c(NA, NA), 
                             zoom = 6, 
                             box_bounds = c(NA, NA, NA, NA), 
                             suppress_print=TRUE, 
                             stride = 'daily',
                             verbosity=0, 
                             label_towns = TRUE,
                             strict_bounds = FALSE,
                             mark_points = NULL,
                             gbr_poly = FALSE,
                             add_arrows = FALSE,
                             max_u = NA,
                             scale_arrows = NA,
                             show_bathy=FALSE,
                             contour_breaks=c(5,10,20))

datapol1 = map_ereefs_movie (var_name = c("temp"), 
                             start_date = c(2011,02,1), 
                             end_date = c(2011,02,28), 
                             layer = 'surface', 
                             output_dir = 'ToAnimate', 
                             Land_map = FALSE, 
                             input_file = "out_simple.nc",
                             input_grid = "out_std.nc", 
                             scale_col = c('ivory', 'coral4'), 
                             scale_lim = c(NA, NA), 
                             zoom = 6, 
                             box_bounds = c(NA, NA, NA, NA), 
                             suppress_print=TRUE, 
                             stride = 'daily',
                             verbosity=0, 
                             label_towns = TRUE,
                             strict_bounds = FALSE,
                             mark_points = NULL,
                             gbr_poly = FALSE,
                             add_arrows = FALSE,
                             max_u = NA,
                             scale_arrows = NA,
                             show_bathy=FALSE,
                             contour_breaks=c(5,10,20))
  

  

datapoly <- datapoly1$datapoly
datapoly1 <- datapol1$datapoly

datapoly$value <- (datapoly$value +datapoly1$value) / 2

saveRDS(datapoly, file = "temp_summer.rds")
