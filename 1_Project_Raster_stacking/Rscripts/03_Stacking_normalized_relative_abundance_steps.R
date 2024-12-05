
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## e-bird and JV collaboration (Script3/3)
## ##
## Objective:Automatically stacking selected e-bird rasters into single rasters.
## Specifically calculating normalized abundance of a group of species per pixel
##
## This code does the following:
## i) Read tiff files into Rasters 
## ii) Reclassify rasters (0,1)
## iii) Sum rasters generating total number of species oer pixel
##
## Updated and annotated by Jenny Munoz (jen.munnoz@gmail.com)
## Last updated: Nov 2024
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

#  Set up  -----------------------------------------------------------------
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
library(tidyverse)
library(dplyr) # Essential for data manipulation with functions to filter, arrange, summarize, etc.
library(janitor) # Functions for simple data cleaning
library(fs) # A cross-platform interface for file system operations

library(viridis) # Provides color scales for improving data visualization accessibility
library(scales) # Graphical scales for mapping data to aesthetics in visualizations
library(fields) # Tools for spatial data
library(readr) # Fast and friendly way to read rectangular data like CSV files

library(rnaturalearth) # Provides map data for creating high-quality maps
library(sf) # Used for handling simple features to work with geographic data
library(raster) # For raster data manipulation and analysis
library(rmapshaper) # Simplifies shapes for data visualizations
library(terra) # For working with raster and vector data in spatial analysis

# 0 Examine and define the Working directory  --------------------------------------------------
getwd() 
setwd("~/Desktop") # set working directory

# 1 Estimating Normalized abundance per pixel [Option1]----------------------------------------------------------
# In this code I am normalizing each of the  estimated relative abundances provided (per species), so that we have for each species the higher estimated value as one.
## Each layer normalized indicate the areas where we have more abundance of each species with a 1
# After that I summed them so resulting files is the sum of teh normalized abundances. Larger numbers indicate an area where higher abundances for more species ocurr.
# because this can be strongly influenced by the number of species, I normalized by the number of species (layer) in each stack.

# Function to load, normalize, and sum a raster file
# Because we have the rasters that are class Spatrast with several layers, we need to normalize each layer first 
# Each layer normalized indicate the areas where we have more abundance of each species with a 1

###_###_####_####_####_###_
# Option 1 
###_###_####_####_####_###_

# Workflow
#step 1. Load each tiff file.
#step 2. Read the files as rasters
#step 3. Calculate the max values in a pixel for each raster.
#step 4. Normalize each raster by the maximum relative abundance
#step 5. Sum the normalized rasters. 
#step 6. Correct by the number of species that are in the pixel ( exclude zeros) or we can normalize by the maximum value of the sum rastes
#step 7. Save the resulting Raster file

#step 1. Load each tiff file.
data_file_1<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amebit_non-resident_breeding_abundance_seasonal_mean_21.tif"
data_file_2<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amecro_non-resident_breeding_abundance_seasonal_mean_21.tif"
data_file_3<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amekes_non-resident_breeding_abundance_seasonal_mean_21.tif"
num_species_included<-3

#step 2. Read the files as rasters
raster_data_1 <- terra::rast(data_file_1)
raster_data_2 <- terra::rast(data_file_2)
raster_data_3 <- terra::rast(data_file_3)

#step 3. Calculate the max values in a pixel for each raster.

max_value_1<- max(terra::values(raster_data_1), na.rm = TRUE)  # Identify max values per layer
max_value_2 <- max(terra::values(raster_data_2), na.rm = TRUE)  # Identify max values per layer
max_value_3<- max(terra::values(raster_data_3), na.rm = TRUE)  # Identify max values per layer

#step 4.  Normalize each raster by the maximum relative abundance
## Each layer normalized indicate the areas where we have more abundance of each species with a 1,max value shoudl be 1

normalized_raster_1 <- raster_data_1 / max_value_1   # divide each raster by the maximun value so that it is normalized 0-1, 
normalized_raster_2 <- raster_data_2 / max_value_2   # divide each raster by the maximun value so that it is normalized 0-1
normalized_raster_3 <- raster_data_3 / max_value_3   # divide each raster by the maximun value so that it is normalized 0-1

#step 5. Sum the normalized rasters. 
# It is important to consider that here pixels with higher numbers are highly influenced by the number of spcies per pixel
# therefore higher numbers indicate higher number of especies, areas where many sppecies have a higher abundance, or both
sum_multiple_normalized_rasters<- sum(normalized_raster_1,normalized_raster_2,normalized_raster_3)

# step 6. correct by the number of species that are in the pixel ( exclude zeros) or we can normalize by the maximum value of the sum rastes
sum_max_value<-max(terra::values(sum_multiple_normalized_rasters), na.rm = TRUE)

normalized_sum_raster <- sum_multiple_normalized_rasters / sum_max_value

# WARNING!!!!
# The interpretation of this numbers should be done with caution, higher number here simply indicate a higher abundance of several species in a giving pixel! 
# but  the numerical value has not direct interpretation

#step 7. Save the resulting Raster file
###_###_###_###_###_###_
writeRaster(normalized_sum_raster , "example_raster_stacking/outputs/example_normalized_percentage_population_rasters_combo.tif")
###_###_###_###_###_###_

