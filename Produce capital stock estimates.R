###### Input parameters

  # Specify input directory

inputDir <- ""

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
library(tempdisagg)
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
library(futile.logger)
library(doSNOW)
library(assertr)
library(tempdisagg)
library(purrrlyr)
library(data.table)
library(forcats)

# Timestamp

runTime <- format(Sys.time(), "%Y-%m-%d_%H%M") # Used in various file out names

# Run sequential R scripts

  # Read in PIM inputs

source("./PIM_inputs.R")

  # Run PIM

source("./Run_pim.R")

  # Write out selected data (before post-processing)

source("./Write_PIM_outputs.R")

# Chaining currently only works for UK GFCF estimates

if (!'Region' %in% names(out)){
  
  # Unchain results and perform reclassifications
  
  source("./Unchain.R")
  
  # Aggregate
  
  source("./Aggregate.R")
  
  # Chain & ANNUALISATION
  
  source("./Chain.R")
  
}