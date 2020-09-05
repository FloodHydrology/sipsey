#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: DEM Inundate
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/4/2020
#Purpose: Create initial inundation maps of Sipsey River
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Convert USGS NAD27 datum to NAD83


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(dataRetrieval)
library(fasterize)
library(sf)
library(raster)
library(whitebox)
library(MASS)
library(lubridate)
library(tidyverse)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir<-"C:\\Workspace\\sipsey\\dump\\"

#Load relevant data
dem<-raster(paste0(spatial_dir,"II_Work\\sipsey_dem.tif"))
flowlines<-st_read(paste0(spatial_dir,"II_Work\\flowlines.shp")) %>% st_zm()
gage<-st_read(paste0(spatial_dir,"II_Work\\gage_point.shp")) %>% st_zm()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Streamflow stats-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Download USGS data --------------------------------------------------------
#Retreive daily data
df<-readNWISdv(
  siteNumbers = '02446500',
  parameterCd = '00060') #Daily flow

#Retreive sd_curve
sd_curve<-readNWISrating('02446500', "base")

#Flood Frequency Analysis ------------------------------------------------------
#Isolate annual peak flood events
df<-df %>% 
  #Convert to tibble
  as_tibble() %>% 
  #select cols of interest
  dplyr::select(date = Date, q = X_00060_00003) %>% 
  #estimate annual max
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(q_max = max(q))

#Fit distribution to data
fit_params<-fitdistr(df$q_max, 'lognormal')

#Add probability to df
df<-df %>% 
  mutate(p = plnorm(q_max, 
                    fit_params$estimate['meanlog'], 
                    fit_params$estimate['sdlog']))

#Estimate flood frequency
df<-tibble(
 ri = c(2,5,10,25, 50, 100),
 q  = qlnorm((1-(1/ri)), fit_params$estimate['meanlog'], fit_params$estimate['sdlog'])
)

#2.3 Convert flood freq Flow values to Stage------------------------------------
#Clean up sd curve
sd_curve<-sd_curve %>% 
  dplyr::mutate(
    depth_ft = INDEP,
    depth_m  = depth_ft*0.3048,
    flow_cfs = DEP) %>% 
  dplyr::select(flow_cfs, depth_m)

#Create interpolation function
sd_interp<-approxfun(sd_curve$flow_cfs, sd_curve$depth_m)

#Add depth to return interval df
df <- df %>% 
  mutate(s = sd_interp(q))

#2.4 Convert stage to dem datum ------------------------------------------------
#Get datum data
datum<-readNWISsite('02446500')$alt_va    
  #Note this is in NAD27, will need to conver DEM to NAD27 as well

#convert to meteres
datum<-datum*0.3048

#Convert stage to elevation
df <- df %>% 
  mutate(ele_m = s + datum)

#Convert from NAD27 to NAD83


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Remove valley slope from DEM-----------------------------------------------
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

#Remove valley slope from dem
wbt_subtract(
  input1 = "dem_10.tif",
  input2 = "idw.tif",
  output = "dem_norm.tif",
  wd = workspace_dir)

#Read dem into R 
dem_norm<-raster(paste0(workspace_dir,"dem_norm.tif"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Inundate ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Update gage datum ----------------------------------------------------------
#Calc offset
offset <- raster::extract(dem_norm, gage) - raster::extract(dem, gage)

#Apply offset
df<- df %>% 
  mutate(ele_cor_m = ele_m + offset)

#4.2 Create flood maps----------------------------------------------------------
#Create inundation function 
dem_inundate<-function(dem_norm, ele){
  
  #Create conditional fun
  con<-function(condition, trueValue, falseValue){
    return(condition * trueValue + (!condition)*falseValue)
  }
  
  #apply fun
  con(dem_norm>ele,0,1)
}
  
#Create rater layers
flood_2yr<-dem_inundate(dem_norm, df$ele_cor_m[df$ri==2])
flood_5yr<-dem_inundate(dem_norm, df$ele_cor_m[df$ri==5])
flood_10yr<-dem_inundate(dem_norm, df$ele_cor_m[df$ri==10])
flood_50yr<-dem_inundate(dem_norm, df$ele_cor_m[df$ri==50])
flood_100yr<-dem_inundate(dem_norm, df$ele_cor_m[df$ri==50])

plot(flood_2yr)
plot(flood_100yr)

