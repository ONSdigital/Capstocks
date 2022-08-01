############################# UNCHAIN ##########################################

# Unchain the PIM results

# -------------------------- Check dataset -------------------------------------

# Check we have an "out" dataframe
# We may want to load one from a previous run, e.g.
#######################################################################################################################

#######################################################################################################################

##### Remove any existing series of terminal costs and add terminal costs series ########

out <- out %>% unnest(cols = c(data))
out <- filter(out, Asset!="TERMINAL")

if (exists('terminal')){
  if (nrow(terminal)>0){

    terminal <- terminal %>%
      rename(ConsumptionOfFixedCapitalCP = gfcfCP)
    terminal$ConsumptionOfFixedCapitalCVM <- terminal$ConsumptionOfFixedCapitalCP/terminal$PriceIndex
    terminal <- select(terminal, Period, Sector, Industry, Asset,
                       ConsumptionOfFixedCapitalCVM, ConsumptionOfFixedCapitalCP, PriceIndex)
    terminal$gfcf_ociv <- terminal$ConsumptionOfFixedCapitalCP
    terminal$refYear <- refPeriod
    terminal$GrossStockCVM <- 0
    terminal$NetStockCVM <- 0
    terminal$ProductiveStockCVM <- 0
    terminal$TotalChangesInVolumeCVM <- 0
    terminal$TotalOtherChangesInVolumeCVM <- 0
    terminal$TotalChangesInVolumeCP <- 0
    terminal$TotalOtherChangesInVolumeCP <- 0
    terminal$NetFixedCapitalFormationCVM <- 0
    terminal$GrossStockCP <- 0
    terminal$NetStockCP <- 0
    terminal$ProductiveStockCP <- 0
    terminal$TotalChangesInVolumeCP <- 0
    terminal$NetFixedCapitalFormationCP <- 0
    terminal$CapitalServicesCP <- 0
    terminal$CapitalServicesCVM <- 0
    terminal$Year <- as.numeric(substr(terminal$Period,2,5))
    terminal$NominalHoldingGL <- 0
    terminal$RealHoldingGL <- 0
    terminal$NeutralHoldingGL <- 0
    terminal$ReturnToCapital <- 0
    out <- rbind(out, terminal)

  }
}


# Set KP = CP for stocks in refYear

out <- out %>% group_by(Sector,Industry,Asset) %>% mutate(stockPI = (sum(PriceIndex[Period==refYear])))
out <- ungroup(out)

out$stockPI <- out$PriceIndex/out$stockPI
out$NetStockCVM <- out$NetStockCP/out$stockPI
out$GrossStockCVM <- out$GrossStockCP/out$stockPI
out$stockPI <- NULL

# Set KP = CP for flows in refYear

out <- out %>% group_by(Sector,Industry,Asset) %>% mutate(PI_Flow_adj = (sum(ConsumptionOfFixedCapitalCP[Year==substr(refPeriod,2,5)]))/
                                                              sum(ConsumptionOfFixedCapitalCVM[Year==substr(refPeriod,2,5)]))
out$PI_Flow_adj <- ifelse(is.na(out$PI_Flow_adj),0,out$PI_Flow_adj)
out$ConsumptionOfFixedCapitalCVM <- out$ConsumptionOfFixedCapitalCVM*out$PI_Flow_adj
out$PI_Flow_adj <- NULL

out <- out %>%
        group_by(Sector, Industry, Asset) %>%
        nest() %>% ungroup()

#####################################################################

if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")

# --------------------------- Unchain ------------------------------------------
dataCols <- colnames(out$data[[1]])
# Create a data.frame of all the CP/CVM pairs we want to unchain
# This can be derived from colnames since they end in CP or CVM as appropriate
pairs <- data.frame(CP = sort(grep("CP$", dataCols, value = TRUE)),
                    CVM = sort(grep("CVM$", dataCols, value = TRUE)),
                    stringsAsFactors = FALSE)

pairs$flow_stock <- ifelse(grepl("Stock",pairs$CP),"Stock","Flow")

# Filter to a subset of required pairs. Filtering *rows* by CP names will retrieve
# both CP and CVM *columns*
pairs <- filter(pairs, CP %in% c("CapitalServicesCP", "ConsumptionOfFixedCapitalCP",
                                 "GrossStockCP", "NetStockCP", "ProductiveStockCP"))

refYear <- as.numeric(substr(refPeriod,2,5))

flog.info("Starting unchain.")
# Unchain using explicit deflator method
out <- unchainAll(out, pairs = pairs, refYear = refYear, parallelise = FALSE)
cat("\n")
flog.info("Unchain complete.")

# Check for any failures
failures <- unlist(lapply(out$unchained, FUN = function(x) inherits(x, "error")))
if (sum(failures) > 0) {
  flog.warn(paste0(sum(failures), " series failed to process:"),
                                 out[failures, ], capture = TRUE)
} else {
  flog.info("All series unchained successfully.")
}

# Remove PriceIndex, no longer required and may get in the way of later data
# aggregation (where we can't add Price Indexes together)
# This will also combine the "data" and "unchained" cols into one "data" col
out <- out %>% unnest(cols = c(data, unchained)) %>%
  select(-PriceIndex) %>%
  group_by(Sector, Industry, Asset) %>%
  nest() %>%
  ungroup


# --------------------------- Reclassifications -------------------------------

# Reclassifications have to be performed after the PIM output is unchained since only then can the CVM values be added

# Unnesting the PIM outputs
out <- out %>% unnest(cols = c(data))

# Reading in the reclassifications file
reclassifications <- read_xlsx(path = paste0(inputDir,"piminput.xlsx"),
                        sheet = 'Reclassification')
reclassifications$From_Industry <- as.character(reclassifications$From_Industry)
reclassifications$From_Industry <- ifelse(nchar(reclassifications$From_Industry)==2,reclassifications$From_Industry,paste0("0",reclassifications$From_Industry))

for (i in 1:nrow(reclassifications)) {
# Specifying all parameters to make easier to call for them
From_Sector <- reclassifications[i,]$From_Sector
From_Industry <- reclassifications[i,]$From_Industry
From_Asset <- reclassifications[i,]$From_Asset
To_Sector <- reclassifications[i,]$To_Sector
Re_Period <- reclassifications[i,]$Period

# Taking out the Sector/Industry/Asset combination for which the reclassification should be done
from <- out[which(out$Sector==From_Sector & out$Industry==From_Industry & out$Asset==From_Asset),]

while (is.na(from$Period[1])){
  i <- i+1
  From_Sector <- reclassifications[i,]$From_Sector
  From_Industry <- reclassifications[i,]$From_Industry
  From_Asset <- reclassifications[i,]$From_Asset
  To_Sector <- reclassifications[i,]$To_Sector
  Re_Period <- reclassifications[i,]$Period

  from <- out[which(out$Sector==From_Sector & out$Industry==From_Industry & out$Asset==From_Asset),]
}

# Reordering the dataset from smallest to largest period
from <- from[order(from$Period),]

if (from$Period[1]>Re_Period){
  Re_Period <- from$Period[1]
}

# Dividing the "from" dataset into 2 parts, where from_unchanged is the data up to the reclassification period
# and the from_changed is the data after the reclassification period, which will have to change
from_unchanged <- from[1:which(from$Period==Re_Period)-1,]
from_changed <- from[which(from$Period==Re_Period):nrow(from),]

# Doing the same to the "to" dataset, which is where the reclassified outputs will go to
to<-out[which(out$Sector==To_Sector & out$Industry==From_Industry & out$Asset==From_Asset),]
to<-to[order(to$Period),]

if (!is.na(to$Period[1])){

if (to$Period[1]>Re_Period){
  Re_Period <- to$Period[1]
}

# Again dividing it into data that will/will not change
to_unchanged <- to[1:which(to$Period==Re_Period)-1,]
to_changed <- to[which(to$Period==Re_Period):nrow(to),]

# Merging both datasets that will change (the to_changed and from_changed)
merged <- merge(x = to_changed, y = from_changed, by = c("Industry", "Asset", "refYear", "Period", "Year"), all = TRUE)

# Creating a new data frame to which the added individual PIM outputs will go to and using the merged data frame to
# do that to ensure that the table dimensions are of the correct size for the outputs
new_to <- merged[,1:37]

# Chaning the names in the new_to, and removing the ".x" that was added when the table merging happened
colnames(new_to)<-gsub('.x$', '', colnames(new_to))

# Changing the na values to 0's since otherwise we can't add the values, where NA's exist
merged[is.na(merged)] <- 0

# Adding all relevant values to make the reclassifications happen
for (i in 1:31) {
  new_to[,i+6] <- merged[,i+6] + merged[,i+6+32]
}

# Changing the Sector
new_to$Sector <- To_Sector

# Creating a "new_from" data frame and replacing the Sector
new_from <- new_to
new_from$Sector <- From_Sector

# Changing all the values in the new_from data frame to 0, since all the values moved to the different Sector during reclassification
for(i in 7:37) {
  new_from[,i] <- 0
}

# Rearranging the output so that it would fit further tasks
new_from <- new_from[, c(6,1:4,7:27, 5, 28:37)]
new_to <- new_to[, c(6,1:4,7:27, 5, 28:37)]

# Removing the old "from" data frame from the pimOutput
out <- out[which(!(out$Sector==From_Sector & out$Industry==From_Industry & out$Asset==From_Asset)),]

# Removing the old "to" data frame from the pimOutput
out <- out[which(!(out$Sector==To_Sector & out$Industry==From_Industry & out$Asset==From_Asset)),]

# Adding back in the reclassified output, also including the data frame where no changes were made ("from_unchanged" and
# "to_unchanged") which would be for the PIM outputs up to the point where the reclassifications happened
out <- rbind(out,from_unchanged, new_from)
out <- rbind(out,to_unchanged, new_to)
} else {
  new_to <- from_changed
  new_to$Sector <- To_Sector

  new_from <- from_changed
  for(i in 7:37) {
    new_from[,i] <- 0
  }

  # Rearranging the output so that it would fit further tasks
  new_from <- new_from[, c(6,1:4,7:27, 5, 28:37)]
  new_to <- new_to[, c(6,1:4,7:27, 5, 28:37)]

  # Removing the old "from" data frame from the pimOutput
  out <- out[which(!(out$Sector==From_Sector & out$Industry==From_Industry & out$Asset==From_Asset)),]

  # Adding back in the reclassified output, also including the data frame where no changes were made ("from_unchanged" and
  # "to_unchanged") which would be for the PIM outputs up to the point where the reclassifications happened
  out <- rbind(out,from_unchanged, new_from)
  out <- rbind(out, new_to)

}
}

# Rename KPs incorrectly named CVMs

#out$Measure <- gsub("CVM", "KP", out$Measure)

pimoutput <- out %>%
  group_by(Sector, Industry, Asset) %>%
  nest() %>%
  ungroup()

write_rds(pimoutput, paste0(outputDir, "pimOutput_reclass_", runTime, ".Rds"))

# -------------------------- Remove Objects ------------------------------------
rm(dataCols, pairs, failures, from, from_changed, from_unchanged, From_Asset, From_Industry, From_Sector, to, to_changed, to_unchanged,
   To_Sector, merged, new_from, new_to, reclassifications, Re_Period, i,
   refYear, pimoutput)
