#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: DEM Inundate
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/4/2020
#Purpose: Create initial inundation maps of Sipsey River
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Semi code:
#1) Inundate Floodplain --> create stage/area relationship
#2) Tie inundation to stage measurements at USGS gage
#3) Analyze USGS gage data (i.e., return intervals)
#4) Create rasters of inundation at 2,5,10,25, and 50 year return intervals
#5) Create interactive map and post online

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(fasterize)
library(sf)
library(raster)
library(whitebox)
library(tidyverse)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir<-"C:\\Workspace\\sipsey\\dump\\"

#Load relevant data
dem<-raster(paste0(spatial_dir,"II_Work\\sipsey_dem.tif"))
flowlines<-st_read(paste0(spatial_dir,"II_Work\\flowlines.shp")) %>% st_zm()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Define channel-------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export files to scratch workspace
writeRaster(dem,paste0(workspace_dir,"dem.tif"), overwrite=T)
st_write(flowlines,paste0(workspace_dir,"flowlines.shp"))

#Resample dem [to lower res]
wbt_aggregate_raster(
  input = "dem.tif",
  output = "dem_10.tif",
  agg_factor = 10,
  wd = workspace_dir
)

#Convert stream to raster 
wbt_rasterize_streams(
  streams = 'flowlines.shp',
  base = 'dem_10.tif',
  output = 'streams.tif',
  wd = workspace_dir
)  

#Add dem value to stream raster
wbt_multiply(
  input1 = "streams.tif",
  input2 = "dem_10.tif",
  output = "stream_ele.tif",
  wd = workspace_dir
)

#Convert raster to point
wbt_raster_to_vector_points(
  input = "stream_ele.tif",
  output = "streams_pnts.shp",
  wd = workspace_dir
)

#Complete IDW interp
wbt_idw_interpolation(
  input = "streams_pnts.shp",
  field = "VALUE", 
  output = "idw.tif",
  base =  "dem_10.tif",
  radius = 2000,
  weight = 4.2,
  wd = workspace_dir
)


