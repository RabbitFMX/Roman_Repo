library(h5)
library(RNetCDF)
library(fields)
library(mapplots)
library(maptools)
library(ncdf4)
library(raster)
library(maps)
library(rgdal)

##########################
## handling NetCDF data ##
##########################

# ncdf4 package

setwd("d:/Samira/Work/Data/Satelite/OMI/OMNO2")


OMNOfiles <- list.files(pattern = ".nc")

CZ_crop <- c(11, 20, 48, 52)
EU_crop <- c(-5, 37, 37, 58)
OMNOcell_values_CZ = c()
OMNOcell_values_EU = c()

for (i in 1:length(OMNOfiles)){
  print(i)
  #NO2_r <- raster(files[i], varname = "TropNO2")
  NO2_r <- raster(OMNOfiles[i], varname = "ColumnAmountNO2Trop")
  print(files[i])
  projection(NO2_r) <- mycrs
  
  CZ_NO2_OMNO <- crop(NO2_r,CZ_crop)
  EU_NO2_OMNO <- crop(NO2_r,EU_crop)
  
  cellNA_EU = Which(is.na(EU_NO2_OMNO), cells = T)               # number of NA values in the EU domain
  cellNonNA_EU = Which(!is.na(EU_NO2_OMNO), cells = T)           # number of Non-NA values in the EU domain
  OMNOcell_values_EU[i] = length(cellNonNA_EU) / ncell(EU_NO2_r)  # percentage of Non-NA values
  
  cellNA_CZ = Which(is.na(CZ_NO2_OMNO), cells = T)               # number of NA values in the CZ domain
  cellNonNA_CZ = Which(!is.na(CZ_NO2_OMNO), cells = T)           # number of Non-NA values in the CZ domain
  OMNOcell_values_CZ[i] = length(cellNonNA_CZ) / ncell(CZ_NO2_OMNO)  # percentage of Non-NA values
  
} 

hist(OMNOcell_values_EU, col = "lightblue", main = "EU domain OMNO2 data")
hist(OMNOcell_values_CZ, col = "red", main = "CZ domain OMNO2 data")

plot(CZ_NO2_OMNO, main = "Example plot - CZ domain", xlab = "lon", ylab = "lat")

map(add=TRUE, col="blue")

plot(EU_NO2_OMNO, main = "Example plot - EU domain", xlab = "lon", ylab = "lat")
map(add=TRUE, col="blue")

########################
## handling HDF5 data ##
########################
source("http://bioconductor.org/biocLite.R") ; biocLite("rhdf5")
library(rhdf5)

setwd("d:/Samira/Work/Data/Satelite/OMI/OMNO2")

lon = seq(-180, 179.75, by = 0.25)
lat = seq(-90, 89.75, by = 0.25)

sat_df_EU = subset(sat_coords, lon >= -5 & lon <= 37 & lat >= 37 & lat <= 58) # Subset only data covering the EU domain
sat_df_CZ = subset(sat_coords, lon >= 11 & lon <= 20 & lat >= 48 & lat <= 52) # Subset only data covering the CZ domain

H5_files <- list.files(pattern = ".he5")

ValidCell_CZ <- c()
ValidCell_EU <- c()

for (i in 1:length(H5_files)) {
  
sat_data_hdf5 <- H5_files[i]
#sat_data_hdf5 <- h5file("OMI-Aura_L3-OMNO2d_2014m0101_v003-2014m0102t141407.he5")

sat_data = as.data.frame(sat_data_hdf5[list.datasets(sat_data_hdf5)[4]][,]) # load data from hdf5 file
sat_coords = expand.grid(lon = lon, lat = lat)
sat_coords$sat = as.vector(t(sat_data)) # create coordinates data frame and add satellite values to each location


rasterCZ <- rasterFromXYZ(sat_df_CZ)
cellCZ = ncell(rasterCZ)
cellNonNA_CZ = Which(rasterCZ>rasterCZ@data@min, cells = T)
ValidCell_CZ <- length(cellNonNA_CZ)/(cellCZ)
ValidCell_CZ

rasterEU <- rasterFromXYZ(sat_df_EU)
cellEU <- ncell(rasterEU)
cellNonNA_EU <- Which(rasterEU>rasterEU@data@min, cells = T)
ValidCell_EU <- length(cellNonNA_EU)/(cellEU)
ValidCell_EU

}

plot(rasterCZ)
map(add=TRUE, col="blue")

plot(rasterEU)
map(add=TRUE, col="blue")


