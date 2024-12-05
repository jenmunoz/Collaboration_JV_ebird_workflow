###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## e-bird and JV collaboration (Script2/3)
## ##
## Objective:Automatically stacking selected e-bird rasters into single rasters. 
## Specifically calculating  percentage of population of a group of species per pixel
##
## This code does the following:
## i) Read tiff files into Rasters 
## ii) Calculate the total estimate population per species in the area (JV region)
## iii) Normalize each raster (Calculte the percentage of population for each species)
## iv) Calculate the mean percentage population per pixel
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

# 1 Estimating Percentage of population (mean) per pixel              [Option1]----------------------------------------------------------

# Here we get the estimated abundance per species and divided by the total sum of the relative abundance for that species (total population=sum of values in the raster) 
# this will give us the percentage of population by pixel. 
# Then we add the percentages of the population per pixel for multiple species, larger numbers indicate larger population sizes for more species 
# If we add the percentages of the population per pixel for multiple species and divide by the total number of layers (Num of species) I can get the mean
# This will be the mean percentage of the population for the species in the group  of multiple species. It includes all the potential species, so to some extent in considers the richness

# I belive this number could have a direct interpretation !!! is the mean percentage of the population for all the species in the group , however I am concern that it might be strongly affected by the zeros 

# Alternatively If I instead divide by the number of species in a given pixel ( less say we exclude the species with zero abundances), I did not do that but this is a possibility to discuss

###_###_####_####_####_###_
# Option 1 
###_###_####_####_####_###_

# We follow these individual steps: 
#Step i). Set the folder path.
#Step ii). List all TIFF files in the folder.
#step 1. Load each tiff file.
#step 2. Read the files as rasters
#step 3. Calculate the total sum of values for each raster.
#step 4. Normalize each raster (Calculte the percentage of population for each species)
#step 5. Sum the normalized rasters.
#step 6. Calculate the mean percentage population 

#step 1. Load each tiff file.
data_file_1<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amebit_non-resident_breeding_abundance_seasonal_mean_21.tif"
data_file_2<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amecro_non-resident_breeding_abundance_seasonal_mean_21.tif"
data_file_3<-"/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters/CIJV_amekes_non-resident_breeding_abundance_seasonal_mean_21.tif"
num_species_included<-3

#step 2. Read the files as rasters
raster_data_1 <- terra::rast(data_file_1)
raster_data_2 <- terra::rast(data_file_2)
raster_data_3 <- terra::rast(data_file_3)

#plot(raster_data_3) # plot individual rasters


#step 3. Calculate the total sum of values for each raster.
total_sum_1<-sum(terra::values(raster_data_1), na.rm = TRUE) # total estimated population for species 1 in the raster extent ( e.g. the Jv)
total_sum_2<-sum(terra::values(raster_data_2), na.rm = TRUE) # total estimated population  for species 2 in the raster extent ( e.g. the Jv)
total_sum_3<-sum(terra::values(raster_data_3), na.rm = TRUE) # total estimated population  for species 2 in the raster extent ( e.g. the Jv)

#step 4.  Normalize each raster (Calculte the percentage of population for each species)
normalized_layer_1 <-(raster_data_1 / total_sum_1)# divide each value by the total sum of the layer, this gives us the percentage of population per pixel for the species
normalized_layer_2 <-(raster_data_2 / total_sum_2)# divide each value by the total sum of the layer, this gives us the percentage of population per pixel for the species
normalized_layer_3 <-(raster_data_3 / total_sum_3)# divide each value by the total sum of the layer, this gives us the percentage of population per pixel for the species

#step 5. Sum the normalized rasters.
sum_multiple_sp_percen_population<- sum(normalized_layer_1,normalized_layer_2,normalized_layer_3)

#step 6. Calculate the mean percentage population 
mean_percentage_population <- ((sum_multiple_sp_percen_population / num_species_included)*100)

###_###_###_###_###_###_
writeRaster(mean_percentage_population_opt2, "example_raster_stacking/outputs/example_percentage_population_rasters_combo.tif")
###_###_###_###_###_###_

# 2 Estimating percentage of population (function for species combos) [Option2] --------
###_###_####_####_####_###_
# Option 2
###_###_####_####_####_###_
# We combine all the individual steps in a function 


#Step i). Set the folder path.
folder_rasters_combos <- "/Users/jennymunoz/Desktop/example_raster_stacking/test_rasters"

#Step ii). Function to list  all TIFF files in the folder.
list_files <- function(folder_rasters_combos) {
  files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)
  return(files)
}

#step 1 - step 6. 
# Function to load, sum and_normalize_raster_by_total_population_mean
num_species_included <- 3

load_and_normalize_raster_by_total_population_mean <- function(folder_rasters_combos, num_species_included) {
  files <- list_files(folder_rasters_combos)
  combined_raster <- NULL
  
  for (eachfile in files) {
    raster_data <- terra::rast(eachfile)
    total_sum <- sum(terra::values(raster_data), na.rm = TRUE) # Calculate the total sum of values for the raster
    
    if (total_sum > 0) {
      normalized_layer <- raster_data / total_sum # Normalize the raster values
    } else {
      normalized_layer <- raster_data
    }
    
    if (is.null(combined_raster)) {
      combined_raster <- normalized_layer
    } else {
      combined_raster <- combined_raster + normalized_layer
    }
  }
  
  if (!is.null(combined_raster)) {
    # Calculate the mean percentage population
    mean_percentage_population <- ((combined_raster / num_species_included) * 100)
    return(mean_percentage_population)
  } else {
    return(NULL)
  }
}

# Set the number of species included

# Run the function
mean_percentage_population_opt2 <- load_and_normalize_raster_by_total_population_mean(folder_rasters_combos, num_species_included)

###_###_###_###_###_###_
writeRaster(mean_percentage_population_opt2, "example_raster_stacking/outputs/example_percentage_population_rasters_combo.tif")
###_###_###_###_###_###_



# 3 Simple plot of the resulting raster --------------

terra::plot(mean_percentage_population_opt2)

