###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## e-bird and JV collaboration
## ##
## Objective:Automatically stacking selected e-bird rasters into single rasters. 
## ## Specifically calculating  number of species per pixel

## This code does the following:
## i) Read tiff files into Rasters 
## ii) Reclassify rasters (0,1)
## iii) Sum rasters generating total number of species oer pixel
##
## Updated and annotated by Jenny Munoz
## Last updated: August 2024
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

# 0 Set up  -----------------------------------------------------------------
# Install required Libraries 
# Data Manipulation
install.packages("tidyverse")
install.packages("janitor")
install.packages("glue") # String Manipulation
install.packages("fs") # File Operations
install.packages("png") # Image Handling
# Data Visualization
install.packages("viridis")
install.packages("scales")
install.packages("fields")
install.packages("readr") # Data Input/Output
# Geospatial Data
install.packages("rnaturalearth")
install.packages("sf")
install.packages("raster")
install.packages("ebirdst")
install.packages("rmapshaper")
install.packages("terra")


# Load necessary libraries
library(dplyr) # Essential for data manipulation with functions to filter, arrange, summarize, etc.
library(janitor) # Functions for simple data cleaning
library(glue) # Useful for data-driven string interpolations
library(fs) # A cross-platform interface for file system operations
library(png) # Allows reading and writing PNG images
library(viridis) # Provides color scales for improving data visualization accessibility
library(scales) # Graphical scales for mapping data to aesthetics in visualizations
library(fields) # Tools for spatial data
library(readr) # Fast and friendly way to read rectangular data like CSV files
library(rnaturalearth) # Provides map data for creating high-quality maps
library(sf) # Used for handling simple features to work with geographic data
library(raster) # For raster data manipulation and analysis
library(ebirdst) # Tools for accessing and analyzing eBird Status and Trends data
library(rmapshaper) # Simplifies shapes for data visualizations
library(terra) # For working with raster and vector data in spatial analysis

# 0 Examine and define the Working directory  --------------------------------------------------
getwd() 
setwd("~/Desktop") # set working directory

# 1 Estimating species Richness per pixel [Option1]----------------------------------------------------------
# In this section I take the raster files, read them as rasters, reclassify them (0,1) and sum them.
# The resulting raster file is then exported as a raster file where each pixel is the total number of species
####_###_###_###_
# Option 1 Working from individual files 
####_###_###_###_

data_file_1<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amebit_non-resident_breeding_abundance_seasonal_mean_21.tif"
data_file_2<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amecro_non-resident_breeding_abundance_seasonal_mean_21.tif"

raster_data_1 <- terra::rast(data_file_1)
raster_data_2 <- terra::rast(data_file_2)

reclassified_raster_1<- terra::classify(raster_data_1, cbind(0, Inf, 1)) # Reclassify the raster values: values > 0 to 1, values <= 0 to 0
reclassified_raster_2<- terra::classify(raster_data_2, cbind(0, Inf, 1)) # Reclassify the raster values: values > 0 to 1, values <= 0 to 0

sum_rasters<- sum(reclassified_raster_1,reclassified_raster_2)

writeRaster(sum_rasters, "example_raster_stacking/outputs/example_sum_rasters_amebit_and_amecro.tif")


# 2 Estimating species Richness per pixel [Option2 combos] --------------

####_###_###_###_
# Option 2- Create a function to look at raster combos! 
####_###_###_###_

# 2.1 indicate the path to the raster combos
folder_rasters_combos<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters"
#files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)

# 2.2 step create a fucntion to list the files in the folder
list_files <- function(folder_rasters_combos) {
  files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)
  return(files)
}

#2.3 create a function that , load reclassify and sum the rasters
load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  files <- list_files(folder_rasters_combos)
  combined_raster <- NULL
  
  for (file in files) {
    raster_data <- terra::rast(file)
    reclassified_raster <- classify(raster_data, cbind(0, Inf, 1)) # Reclassify the raster values: values > 0 to 1, values <= 0 to 0
    
    if (is.null(combined_raster)) {
      combined_raster <- reclassified_raster
    } else {
      combined_raster <- combined_raster + reclassified_raster
    }
  }
  
  if (!is.null(combined_raster)) {
    sum_raster <- sum(combined_raster)
    return(sum_raster)
  } else {
    return(NULL)
  }
}

# Explore the results
sum_raster_combo<-load_reclass_and_sum_raster (folder_rasters_combos)
writeRaster(sum_raster_combo, "example_raster_stacking/outputs/example_sum_rasters_combo.tif")

