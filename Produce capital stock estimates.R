###### Input parameters

  # Specify input directory

inputDir <- "" # Location of inputs (e.g. "C:/Capital stock/")

# Specify location of R scripts

scriptsPath <- ""

# Specify library path

libraryPath <- ""

###### End of input parameters

# Ensure wd in correct format

if (substr(scriptsPath,nchar(scriptsPath),nchar(scriptsPath))!="/"){
  
  scriptsPath <- paste0(scriptsPath,"/")
  
}

setwd(scriptsPath)

# Set library path if specified

if (libraryPath!=""){
  
  .libPaths(libraryPath)
  
}

# Read in libraries

# CRAN packages
# iii
library(dplyr)
library(readr)
library(readxl)
library(testthat)
library(tibble)
library(tidyr)
# Capstock packages
library(capstock)
library(pimIO)
library(prepim)
# Packages for Forecasting
library(forecast)
library(stringr)
# SQL
library(sqldf)
library(cellranger)
library(RSQLite)
##
library(writexl)

# Timestamp

runTime <- format(Sys.time(), "%Y-%m-%d_%H%M") # Used in various file out names

# Run sequential R scripts

  # Read in PIM inputs

source("./PIM_inputs.R")

  # Run PIM

source("./Run_pim.R")

  # Write out selected data (before post-processing)

source("./Write_PIM_outputs.R")

  # Unchain results and perform reclassifications

source("./Unchain.R")

  # Aggregate

source("./Aggregate.R")

# Chain & ANNUALISATION

source("./Chain.R")
