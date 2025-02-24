# Abbreviated Code Base For Cleaning Geo-Data
# Michael Fichman & Brad Mattan / Falk Lab / Geoscanning
# 4 / 18 / 2024

# Load and clean geotracking data
# Apply space-time functions and NYU stay events
# Load Retailers and join to observations
# Join observations to census tracts

# Load Libraries

#library(plyr) # Load this if you need to run the nyu functions from our code base instad of from devtools
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(lubridate)
library(jsonlite)
library(devtools)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)

devtools::install_github("nyu-mhealth/Mobility")
library(Mobility)

# Install functions from R folder in the geoscanning repo

source("R/uploadGeodata.R")
source("R/cleanDates.R")
source("R/spaceTimeLags.R") 
source("R/intakeRetailers.R")
source("R/bufferAndJoin.R")
source("R/joinTracts.R")
source("R/indirectMLM.R")
source("R/geotrackingLeaflet.R")
source("R/retailersLeaflet.R")
source("R/intakeSummary.R")
source("R/graphicsFunctions.R")
source("R/exposureLeaflet.R")
source("R/removeDuplicates.R")

# Upload data and add space/time indicators
# Specify the following parameters:
# 1. uploadGeodata - specify the filepath (default is our test data)
# 2. Add a "cleanDates" call for each user with a time window specifying beginning and end times
# using a YYYY-MM-DD HH:MM:SS format
# 3. Specify the stayevent parameters - dist.threshold in meters, time.units in minutes
# 4. Specify the time window for measuring radiusofgyration
# 5. Specify a coordinate system for spaceTimeLags in feet. Default is 2272 for South PA

# Output is an sf object

# Note that if NYU mobility package is inoperable - use sourced NYU functions
# This will require loading packages DataCombine, zoo, geosphere and perhaps others
# DataCombine seems nonfunctional as of 4/18/2024, so the relevant function 'slide'
# Is replicated below - it requires plyr, which needs to be loaded before tidyverse

library(zoo)
library(geosphere)

source("R/nyu_functions/radiusofgyration.R")
source("R/nyu_functions/stayevent.R")
source("R/nyu_functions/groupdist.R")
source("R/nyu_functions/grouptime.R")
source("R/nyu_functions/DataCombine/utils.R")
source("R/nyu_functions/DataCombine/slide.R")
source("R/nyu_functions/seqgroup.R")
source("R/nyu_functions/mergewithorder.R")
source("R/nyu_functions/sdspatialdatapoints.R")

cleanData <- uploadGeodata("~/GitHub/geoscanning/Data/Geotracking/multi_json_test") %>%
  cleanDates(., "2018-01-01 21:30:45", "2022-07-22 15:17:47", addFlags = FALSE, "Records.json")

cleanData <- uploadGeodata("~/GitHub/geoscanning/Data/Geotracking/multi_json_test") %>%
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", addFlags = FALSE, "file1.json") %>% # parameter
  cleanDates(., "2019-04-22 21:30:45", "2019-08-05 15:17:47", addFlags = FALSE, "file2.json") %>% # parameter
  stayevent(., 
            coor = c("lon","lat"), 
            time = "datetime", 
            dist.threshold = 100/3.28084, # conversion to feet PARAMETER
            time.threshold = 5, # PARAMETER
            time.units = "mins", 
            groupvar = "filename") %>%
  mutate(rg_hr = radiusofgyration(., 
                                  coor = c("lon","lat"), 
                                  time = "datetime", 
                                  time.units = "hour", # PARAMETER
                                  groupvar = "filename")) %>%
  spaceTimeLags(., 2272) %>%
  intakeSummary(.)

# BM: Warning message from NYU function:
# `group_by_()` is deprecated as of dplyr 0.7.0.
# Please use `group_by()` instead.
# See vignette('programming') for more help

######

# Inspect Data Using A Leaflet map

# Parameters - dataSet, stayEvents
# If stayEvents is TRUE, the map outputs stay events
# otherwise it shows all geotracking observations
# A leaflet map will pop up in your R Studio Viewer
# Toggle the data by user in the app menu at top right
# BM: If you're working with more than 10,000 observations, downsample to ~10,000 before plotting.

geotrackingLeaflet(cleanData %>% 
                     filter(filename == "file1.json") %>% # option to select subject(s) in advance
                     sample_n(., 10000), 
                   stayEvents = FALSE)

#######

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# write.csv(cleanData %>% as.data.frame(), "yourfile_path/fileName.csv")

#######

# Load Retailers

# Parameters:
# 1. specify the location of the most recent retailer data set

retailers <- intakeRetailers("Data/Retailers/all_Retailers_10_20_20.csv")

# Visualize the retailers using a leaflet map

retailersLeaflet(retailers)

# Associate retailers and Census tract info to geotracking observations

# Parameters:
# bufferAndJoin takes four parameters:
# 1. a retailer database (retailers)
# 2. the geotracking data (here as `.`)
# 3. A crs (coordinate reference system) - keep default 2272 for Philadelphia area (linear unit - feet)
# 4. A buffer size in the linear units of the crs - (defaulted below to 100 feet)

cleanData_Retailers_Tracts <- cleanData %>%
  bufferAndJoin(retailers, ., 2272, 100) %>%
  joinTracts(.)

# exposureLeaflet visualizes geotracking observations by stay event (size)
# and by exposures under 30mph (color)
exposureLeaflet(cleanData_Retailers_Tracts)

# Output workspace, shp, csv

# Specify a filepath outside the geoscanning repo (on your machine perhaps)
# to write out a workspace and/or csv and shapefile versions of your data

# save.image("your_file_path/fileName.RData")
# write.csv(cleanData_Retailers_Tracts %>% as.data.frame(), "yourfile_path/fileName.csv")
  