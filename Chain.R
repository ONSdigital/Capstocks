# ======================= CHAIN AND ANNUALISE ============================================
#datForI <- sqldf("SELECT * FROM aggregated WHERE Industry = '77'")
#write.table(datForI, file = paste0(outputDir, "Data for Iasmina - Industry 49", "_", runTime, ".csv"), row.names = FALSE, sep=",")
#write_csv(test, paste0(outputDir, "TestDataset.csv"))

# Add in NPISH values for certain Industries based on Ratios
# Filter the aggregation dataset to the required level and time period
# Chain
# Annualise

flog.info("Chaining Subset of Data.")

##################################################################################################################################
# PULL IN SAVED OUTPUT
#### YOU WILL NEED TO CHANGE THE FILENAME BASED ON WHAT IS IN YOUR OUTPUT FOLDER ####
# aggregated <- read_rds("output/AGGREGATED.Rds")

##################### SOURCE FILE FOR FUNCTIONS ###############################

source("miscCapStocksFunctions.R")

##################################################################################################################################
# write_excel_csv(aggregated, path = paste0(outputDir, "AGGREGATED.csv"))
# spareAggregated <- aggregated
# aggregated <- spareAggregated
##################################################################################################################################

cols <- names(aggregated)

# ---------------------- Filter Aggregation Dataset ----------------------------

# Note the dataset could get very big at this point as we have every possible combination of aggregations.
# Since "chain" is a time-intensive function we want to be selective now as to which level of hierarchies we chain

## REDUCE THE AGGREGATED DATA DOWN TO WHAT IS NEEDED USING THE COVERAGE TABLE IN AN EXCEL FILE IN THE INPUT FOLDER
## PULL IN THE COMBINATIONS OF SECTOR, INDUSTRY AND ASSET
covTabStr <- paste0(inputDir, "CVM_coverage_table.xlsx")               # FILE PATH STRING
##covTabStr <- paste0("Input/Mapping & Aggregation/TEST CVM_coverage_table.xlsx")             # FILE PATH STRING
covTab <- read_excel(covTabStr, sheet = "CVM", col_types = "text")                           # OPEN THE FILE AND SELECT THE SHEET
## CREATE A NEW COLUMN THAT HOLDS A CONCATENATED STRING OF SECTOR, INDUSTRY AND ASSET
aggregated$coverage <- paste0(aggregated$Sector, aggregated$Industry, aggregated$Asset)
## DO THE SAME FOR THE COVERAGE DATA
covTab$Coverage <- paste0(covTab$Sector, covTab$Industry, covTab$Asset)
## SELECT THE DATA NEEDED BASED ON THE NEW COVERAGE COLUMNS AND A DATE FROM AND TO
toChain <- aggregated %>% filter(coverage %in% covTab$Coverage, Period >= toChainFrom)
#toChain <- aggregated %>% filter(Period >= toChainFrom)
toChain <- toChain %>% filter(Period <= toChainTo)

# DO SOME CLEANING
rm(covTabStr)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
#### WEAPONS AND OTHER.MACHINERY ADDED HERE RATHER THAN ADD ALL THE DIFFERENT COMBINATIONS IN THE COVERAGE FILE
#toChain <- aggregated
othMacWeap <- aggregated %>% filter(Asset %in% c('OTHER.MACHINERY','WEAPONS','MACH.EQ'), Period >= toChainFrom)
othMacWeap <- othMacWeap %>% filter(Period <= toChainTo)
toChain <- rbind(toChain, othMacWeap)
rm(othMacWeap, aggregated)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

##################################################################################################################################

# NOT ALL MEASURES (COLUMNS) ARE NEEDED AT THE MOMENT SO REMOVE THEM
toChain <- toChain %>% select(-ProductiveStockCP, -ProductiveStockCYP, -ProductiveStockPYP)
toChain <- toChain %>% select(-CapitalServicesCP, -CapitalServicesCYP, -CapitalServicesPYP)

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
cont1 <- nrow(toChain)
# JUST IN CASE YOU HAVE DUPLICATE DATA, GROUP TO REMOVE, (Sector/A21d/Asset1, Sector/A38/Asset1, Sector/A64/Asset1) all THE same !!!!
toChain <- sqldf("SELECT Sector, Industry, Asset, Period, GrossStockCYP, GrossStockPYP, GrossStockCP, NetStockCYP, NetStockPYP, NetStockCP,
                 ConsumptionOfFixedCapitalCYP, ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP, MAX([Group]) AS [Group]
                 FROM toChain GROUP BY Sector, Industry, Asset, Period, GrossStockCYP, GrossStockPYP, GrossStockCP, NetStockCYP, NetStockPYP, NetStockCP,
                 ConsumptionOfFixedCapitalCYP, ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP")
cont2 <- nrow(toChain)
if (cont1 != cont2)
{
  paste0("GROUPING STATEMENT HAS reduced toChain Rows by: ", toString(cont1 - cont2))
}
rm(cont1, cont2)
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

toChain <- rbind(toChain, createBespokeSecIndAssAggregations(toChain))


####  WEAPONS AND OTHER.MACHINERY ARE NOT NEEDED IN ANY OF THE CURRENT CRITERIA FOR CORD SO REMOVE TO IMPROVE SPEED OF CHAINING
####  NOW NEEDED, SO COMMENTED OUT
####  toChain <- toChain %>% filter(!(Asset %in% c('OTHER.MACHINERY','WEAPONS')))

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

# CHAIN THE DATA - THIS FUNCTION CHAINS BY GROUP IN A LOOP TO HELP WITH MEMORY PROBLEMS
# benchType can take 3 forms
# 'avg' as a string
# 'sum' as a string
# 4 as an integer
source("miscCapStocksFunctions.R")
refYear <- substr(refPeriod,2,5)
refYear <- as.double(refYear)

chained <- chainDataSkippingErrors(toChain, benchType = 4, refYear, correct_CVM = TRUE)

##################################################################################################################################

# CHECK FOR FAILURES AND LIST IF ANY APPEAR
failures <- unlist(lapply(chained$chained, FUN = function(x) inherits(x, "error")))
if (sum(failures) > 0)
{
  errs <- chained[(failures),]
  flog.warn(paste0(sum(failures), " series failed to process:"), chained[failures, ], capture = TRUE)
  chained <- chained[!(failures),]
} else {
  flog.info("All series chained successfully.")
}
#######################################################################

# ATTEMPT TO UNNEST - MIGHT HAVE TO USE UNNEST BY ROW (FIRST FUNCTION CALL) IF THE OVERALL UNNEST METHOD FAILS
# chainedUnnest <- unnestDataWithDifferentRowNumbers(chained)
chainedUnnest <- chained %>% unnest()
#chainedUnnest <- read_rds("output/chainedUnnest.Rds")


#################################################################################################################################
#################################################################################################################################

# REMOVE SOME COLUMNS THAT ARE NO LONGER NEEDED
chainedUnnest <- chainedUnnest %>% select(-Group, -Year)

#### CREATE A DATASET FOR INDUSTRY T AS TOTAL ASSET -  NOT PRESENT IN DATA BUT NEEDED FOR DELIVERIES
tTab = sqldf("select 'S.1' AS Sector, 'T' AS Industry, Asset, Period,
             0 AS GrossStockCYP, 0 AS GrossStockPYP, 0 AS GrossStockCP,
             0 AS NetStockCYP,  0 AS NetStockPYP, 0 AS NetStockCP,
             0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP, 0 AS ConsumptionOfFixedCapitalCP,
             0 AS ConsumptionOfFixedCapitalCVM, 0 AS GrossStockCVM, 0 AS NetStockCVM
             FROM chainedUnnest GROUP BY Asset, Period")

#################################################################################################################################
#################################################################################################################################

#### JOIN UP THE DATASETS
chainedUnnest <- rbind(chainedUnnest, tTab)
rm(tTab)
#chainedUnnest$Year <- substring(chainedUnnest$Period,2,5)
#################################################################################################################################
#################################################################################################################################

#### NOW SOME OF THE COLUMNS HAVE BEEN REMOVED YOU MIGHT HAVE DUPLICATE DATA SO JUST INCASE GROUP TOGETHER
chainedUnnest <- sqldf("SELECT * FROM chainedUnnest GROUP BY Sector, Industry, Asset, Period ORDER BY Sector, Industry, Asset, Period")

#################################################################################################################################

# Round at the lowest level for additive series and then aggregate

for (m in measures){
  
  out[m] <- round(out[m], 0)
  
}

# Hierarchies

hierarchies <- paste0(inputDir, "hierarchies_sector_industry_asset.xlsx")
secHier <- read_excel(hierarchies, sheet = "Sector", col_types = "text")
indHier <- read_excel(hierarchies, sheet = "Industry", col_types = "text")
assHier <- read_excel(hierarchies, sheet = "Asset", col_types = "text")
# Format industry codes (i.e. "1" => "01")
indHier <- as.data.frame(lapply(indHier, prepim::formatIndustryCodes), stringsAsFactors = FALSE)
# Remove most detailed Industry level column and then remove duplicate rows
indHier <- distinct(mutate(indHier, A88 = NULL))

# Specify variables to aggregate

colsToAggregate <- c("GrossStockCYP", "GrossStockPYP", "GrossStockCP",
                     "NetStockCYP", "NetStockPYP", "NetStockCP",
                     "ConsumptionOfFixedCapitalCYP", "ConsumptionOfFixedCapitalPYP", "ConsumptionOfFixedCapitalCP")


out <- out %>% filter(!((Sector=="S.1311" | Sector=="S.1313")
                        & (substr(Industry,1,2)!="GF"))) %>%
                filter(Asset!="WEAPONS")

aggregated_additive <- aggAll(.data = out,
                                    secHier = secHier, indHier = indHier, assHier = assHier,
                                    values = colsToAggregate)

## CREATE A NEW COLUMN THAT HOLDS A CONCATENATED STRING OF SECTOR, INDUSTRY AND ASSET

aggregated_additive$coverage <- paste0(aggregated_additive$Sector, aggregated_additive$Industry, aggregated_additive$Asset)
aggregated_additive <- aggregated_additive %>% filter(coverage %in% covTab$Coverage)

aggregated_additive <- select(aggregated_additive, -Sector_Level,
                              -Industry_Level, -Asset_Level, -Series,
                              -Group, -coverage)
aggregated_additiveAnnual <- annualiseData(aggregated_additive)
# aggregated_additiveAnnual <- select(aggregated_additiveAnnual, -ConsumptionOfFixedCapitalCVM, 
#                                     -NetStockCVM, -GrossStockCVM)
aggregated_additive <- gather(aggregated_additive, "Measure", "Value", 5:ncol(aggregated_additive))

# Round CVMs for each series

chainedUnnest <- select(chainedUnnest, Sector, Industry, Asset,
                        Period, ConsumptionOfFixedCapitalCVM,
                        GrossStockCVM, NetStockCVM)
chainedUnnestAnnual <- annualiseData(chainedUnnest)
chainedUnnest <- gather(chainedUnnest, "Measure", "Value", 5:ncol(chainedUnnest))

Quarterly <- rbind(aggregated_additive, chainedUnnest)
Quarterly <- spread(Quarterly, Period, Value)

# Annualise the data

aggregated_additiveAnnual <- gather(aggregated_additiveAnnual, "Measure", "Value", 5:ncol(aggregated_additiveAnnual))
chainedUnnestAnnual <- gather(chainedUnnestAnnual, "Measure", "Value", 5:ncol(chainedUnnestAnnual))

Annual <- rbind(aggregated_additiveAnnual, chainedUnnestAnnual)
Annual <- spread(Annual, Period, Value)

# Output

write.csv(Quarterly, "Quarterly_estimates.csv", row.names=F)
write.csv(Annual, "Annual_estimates.csv", row.names=F)
