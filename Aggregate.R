# ======================= AGGREGATE ============================================

# Rename other machinery, equipment and weapons
# systems to weapons - this is now incorrectly
# described, but will not be included in outputs

out$Asset[out$Asset=="OTH.MACH.WEAP"] <- "WEAPONS"

# Read in Sector/Industry/Asset hierarchies
# Aggregate to all Sector/Industry/Asset hierarchies
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#### AGGREAGTES NOT ADDING UP TO SECTOR, INDUSTRY, ASSET GROUPS, SO DECIMAL PLACES VALUES NOW ROUNDED TO ZERO DECIMAL POINTS BEFORE AGGRAGATION

#### SECTION TO CALL BESPOKE CALCULATIONS - CALLED FROM SCRIPT bespokeDataCalculations.R ####

source("bespokeDataCalculations.R")

# UNNEST DATA

# Split relevant S.13 COFOG's
indSplit <- read_excel(paste0(inputDir, "splitcofog.xlsx"), sheet = "split_COFOG", col_types = "text") # PULL IN THE SPLITS FROM THE SPREADSHEET
toSplit <- unique(paste0(indSplit$Sector,indSplit$Industry))
indSplit <- indSplit %>% gather(key = 'Period', value = "Perc", 4:ncol(indSplit))
indSplit$Perc <- as.numeric(indSplit$Perc)

out$concat <- paste0(out$Sector,out$Industry)
notSplit <- filter(out, !concat %in% toSplit)
split <- filter(out, concat %in% toSplit)
split <- left_join(split,indSplit)

splitNew <- split
splitNew$Industry <- splitNew$toIndustry

applySplit <- c( "gfcf_ociv",                    "GrossStockCVM",                "NetStockCVM",
                 "ProductiveStockCVM",           "TotalChangesInVolumeCVM",      "TotalOtherChangesInVolumeCVM",
                 "TotalOtherChangesInVolumeCP",  "ConsumptionOfFixedCapitalCVM", "NetFixedCapitalFormationCVM",
                 "GrossStockCP",                 "NetStockCP",                   "ProductiveStockCP",
                 "TotalChangesInVolumeCP",       "ConsumptionOfFixedCapitalCP",  "NetFixedCapitalFormationCP",
                 "NominalHoldingGL",             "RealHoldingGL",                "NeutralHoldingGL",
                 "CapitalServicesCYP",           "CapitalServicesPYP",           "ConsumptionOfFixedCapitalCYP",
                 "ConsumptionOfFixedCapitalPYP", "GrossStockCYP",                "GrossStockPYP",
                 "NetStockCYP",                  "NetStockPYP",                  "ProductiveStockCYP",
                 "ProductiveStockPYP" )

for (i in applySplit){
  splitNew[i] <- split[i]*split$Perc
  split[i] <- split[i]*(1-split$Perc)
}
splitNew[ ,c('toIndustry', 'Perc', 'concat')] <- list(NULL)
split[ ,c('toIndustry', 'Perc', 'concat')] <- list(NULL)
notSplit$concat <- NULL
out <- rbind(notSplit,split,splitNew)
rm(notSplit, split, splitNew,i)

measures <- c("GrossStockCVM", "NetStockCVM", "ProductiveStockCVM", "ConsumptionOfFixedCapitalCVM", "GrossStockCP",
              "NetStockCP", "ProductiveStockCP", "ConsumptionOfFixedCapitalCP", "CapitalServicesCP",
              "CapitalServicesCVM", "CapitalServicesCYP", "CapitalServicesPYP", "ConsumptionOfFixedCapitalCYP",
              "ConsumptionOfFixedCapitalPYP", "GrossStockCYP",  "GrossStockPYP",  "NetStockCYP", "NetStockPYP",
              "ProductiveStockCYP", "ProductiveStockPYP")

#### TAKE CARE OF ANY NEGATIVE VALUES BY MAKING THEM ZERO

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

for (m in measures){

  out[m] [ out[m]<0 ] <- 0
  out[m] [ is.nan(out[m]) ] <- 0

}

out [ is.nan(out) ] <- 0

#### Set capital consumption for cultivated assets to zero - this seems to have have been lost beteen v.1.0.8 and v.1.0.11

out$ConsumptionOfFixedCapitalCP [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalCVM [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalPYP [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalCYP [ out$Asset=="CULTIVATED" ] <- 0

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# ------------------- Read hierarchies for aggregation -------------------------
flog.info("Reading aggregation hierarchies.")
# Note, to avoid category conflicts each hierarchy column should be unique across
# the three hierarchies
hierarchies <- paste0(inputDir, "hierarchiessectorindustryasset.xlsx")
secHier <- read_excel(hierarchies, sheet = "Sector", col_types = "text")
indHier <- read_excel(hierarchies, sheet = "Industry", col_types = "text")
assHier <- read_excel(hierarchies, sheet = "Asset", col_types = "text")
# Format industry codes (i.e. "1" => "01")
indHier <- as.data.frame(lapply(indHier, prepim::formatIndustryCodes), stringsAsFactors = FALSE)
# Remove most detailed Industry level column and then remove duplicate rows
indHier <- distinct(mutate(indHier, A88 = NULL))

# ------------------- Aggregate All Hierarchies unrounded for chaining  --------------------------------

# Aggregate CP variables and all CYP/PYP pairs
# Specify variables to aggregate
colsToAggregate <- c("GrossStockCYP", "GrossStockPYP", "GrossStockCP",
                     "NetStockCYP", "NetStockPYP", "NetStockCP",
                     "ProductiveStockCYP", "ProductiveStockPYP", "ProductiveStockCP",
                     "CapitalServicesCYP", "CapitalServicesPYP", "CapitalServicesCP",
                     "ConsumptionOfFixedCapitalCYP", "ConsumptionOfFixedCapitalPYP", "ConsumptionOfFixedCapitalCP")

flog.info("Starting aggregation.")
# Perform the aggregation
aggregated <- aggAll(.data = out,
                           secHier = secHier, indHier = indHier, assHier = assHier,
                           values = colsToAggregate)
flog.info("Aggregation complete.")

# -------------------------- Remove Objects ------------------------------------
rm(colsToAggregate, secHier, indHier, assHier, hierarchies)
