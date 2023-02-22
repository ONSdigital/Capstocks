
# If input directory not set use current wd

if (inputDir==("J:/Annual round/Userguide_test")){
  
  inputDir <- getwd("D:/Current code")
  
}

# Ensure '/' at end of input directory

if (substr(inputDir,nchar(inputDir),nchar(inputDir))!="/"){
  
  inputDir <- paste0(inputDir,"/") 
  
}

# Read in run parameters

params <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                    sheet = 'Run_parameters')

toChainFrom <- params$toChainFrom # quarter to chain from
toChainTo <- params$toChainTo # quarter to chain to
refPeriod <- params$refPeriod # reference period

# Specify output directory

outputDir <- params$outputDir

# If output directory not set use current wd

if (is.na(outputDir)){
  
  outputDir <- getwd()
  
}

if (substr(outputDir,nchar(outputDir),nchar(outputDir))!="/"){
  
  outputDir <- paste0(outputDir,"/")
  
}

# Read in GFCF and deflators, then combine

gfcf <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                  sheet = 'GFCF_CP')



# add columns

AverageLifeLengths <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),sheet = 'AverageLifeLengths', col_types = "text")

CoVs <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),sheet = 'CoVs', col_types = "text")

Min <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),sheet = 'Min', col_types = "text")

Max <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),sheet = 'Max', col_types = "text")


add_columns <- function(dat){
  
  if (length(setdiff(names(gfcf), names(dat))>0)){
    
    for (d in setdiff(names(gfcf), names(dat))){
      
      dat[d] <- dat[ncol(dat)]
      
    }
    
  }
  
  return(dat)
  
}

AverageLifeLengths <- add_columns(AverageLifeLengths)
CoVs <- add_columns(CoVs)
Min <- add_columns(Min)
Max <- add_columns(Max)


gfcf <- gather(gfcf, Period, gfcfCP, 4:ncol(gfcf))


#Add rows

Other <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),sheet = 'Other', col_types = "text")


add_rows <- function(dat){

  if (length(setdiff(gfcf$Period, dat$Period)>0)){

    for (d in setdiff(gfcf$Period, dat$Period)){

      dat[nrow(dat)+1,] <- c(d, dat[nrow(dat)-1,2],dat[nrow(dat)-1,3],dat[nrow(dat)-1,4])

    }

  }

  return(dat)

}

Other <- add_rows(Other)
######################################################################

deflators <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                       sheet = 'Price_index')
deflators <- gather(deflators, Period, PriceIndex, 4:ncol(deflators))
gfcf <- left_join(gfcf, deflators, by = c('Sector', 'Industry', 'Asset', 'Period'))

gfcf <- gfcf %>%
  group_by(Sector, Industry, Asset) %>%
  # Strip out leading zeros (no need to process these)
  slice(pmin(which(Period == refPeriod), which.max(gfcfCP != 0)):n()) %>%
  ungroup()

# Read in asset lives and combine
source("miscCapStocksFunctions.R")
gfcf <- joinLifeLengths(gfcf, AverageLifeLengths, CoVs, Min, Max)

gfcf$Average <- as.numeric(gfcf$Average)
gfcf$CoV <- as.numeric(gfcf$CoV)
gfcf$Max <- as.numeric(gfcf$Max)
gfcf$Min <- as.numeric(gfcf$Min)

# Adjustments

adjustments <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                         sheet = 'OCIV')
gfcf <- left_join(gfcf, adjustments, by = c("Sector", "Industry", "Asset", "Period"))

# Fill out K-values with zeros

naToZero <- function(x) dplyr::if_else(is.na(x), 0, x)
gfcf <- mutate_at(gfcf, .vars = c("K1CP", "K3CP", "K4CP", "K5CP", "K61CP", "K62CP"),
                  .funs = naToZero)

# Add other parameters

tax_util <- Other
write.csv(tax_util, file= paste0(inputDir, "series_tax_util_cpi_reshape.csv"), row.names=FALSE)
gfcf <- addOtherParams(gfcf, paste0(inputDir, "series_tax_util_cpi_reshape.csv"))
rm(tax_util)
file.remove(paste0(inputDir, "series_tax_util_cpi_reshape.csv"))

# Set-aside terminal costs, which are immediately
# consumed  

terminal <- filter(gfcf, Asset=="TERMINAL")
gfcf <- filter(gfcf, Asset!="TERMINAL")

# ------------------------ Add Reference Year ----------------------------------
# Set the reference year (for calculation of Capital Services only)
# Some stocks don't last until reference year so just use their max period

flog.info("\nAdding Reference Year.")
refYear <- params$refPeriod

# Re-referencing deflators

gfcf$year <- substr(gfcf$Period,2,5)

gfcf <- gfcf %>% group_by(Sector,Industry,Asset) %>% mutate(adjustment = (sum(PriceIndex[as.numeric(year)==substr(refPeriod,2,5)]))/4)
gfcf$PriceIndex <- gfcf$PriceIndex/gfcf$adjustment
gfcf$adjustment <- NULL
gfcf$year <- NULL
gfcf <- ungroup(gfcf)

# -------------------- Nest Series Ready for PIM -------------------------------

inputData <- gfcf %>%
  rename(Vintage = Period) %>%   # PIM refers to Periods as "Vintage"
  group_by(Sector, Industry, Asset) %>%
  # For each series, roll-up all columns into a single list-column called "data"
  nest() %>%
  ungroup()

# Profiles

configs <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                     sheet = 'Dep_ret_profiles')

configs <- expandConfigSpec(configs, toCover = inputData, joinKeys = c("Asset", "Industry", "Sector"))

# Create config objects and put them into a single list-column

configs <- configs %>%
  group_by(Sector, Industry, Asset) %>%
  do(config = do.call(pimir::pimConfig, select(., -Sector, -Industry, -Asset, -Notes)))

# Join data and configs for each series

inputData <- inputData %>% left_join(configs, by = c("Sector", "Industry", "Asset"))
rm(configs)


# Check if any series do not have observations in the refPeriod (e.g. they started afterwards)
# Remember we stripped out leading zeros

maxYears <- lapply(inputData$data, FUN = function(x) {
  leRefYear <- x$Vintage <= refYear
  suppressWarnings(max(x$Vintage[leRefYear]))
})
maxYears <- unlist(maxYears)
inputData$refYear <- maxYears

# Remove any series without data in refYear

noData <- inputData[is.na(inputData$refYear), ]
if (nrow(noData) > 0) flog.info("Removing series with no data in refPeriod:",
                                getSeries(noData), capture = TRUE)
inputData <- filter(inputData, !is.na(refYear))
flog.info(paste("Series count:", nrow(getSeries(inputData))))

# Calculate last complete year

lastCompleteYear <- unique(gfcf$Period)
lastCompleteYear <- lastCompleteYear[substr(lastCompleteYear,7,7)==4]
lastCompleteYear <- max(as.numeric(substr(lastCompleteYear,2,5)))


# Remove series not required

rm(adjustments, deflators, gfcf, noData, params, Other)
