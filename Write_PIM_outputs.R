######################## OUTPUT PIM RESULTS ####################################
 
# Select some of the PIM outputs and write to CSV
# Note this is before any unchaining/aggregating/chaining of the results

# Check we have an "out" dataframe
# We may want to load one from a previous run, e.g.

if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")

# ---------------- Select variables and write to CSV ---------------------------
pimOutput <- out %>% unnest(cols = c(data))

# Save selected variables for output to CSV
baseVariables <- c("Sector", "Industry", "Asset", "Period")

# Define the variables required for output
requiredVariables <- c("GrossStockCVM", "NetStockCVM", "ConsumptionOfFixedCapitalCVM", #ProductiveStockCVM",
                       "GrossStockCP", "NetStockCP", "ConsumptionOfFixedCapitalCP") #ProductiveStockCP")

# Select the variables

pimOutput <- pimOutput %>% select(one_of(c(baseVariables, requiredVariables)))

# Gather up the variables under a new "Measure" column

pimOutput <- pimOutput %>% gather(key = "Measure", value = "Value", all_of(requiredVariables))

# Round

pimOutput <- pimOutput %>% mutate(Value = round(Value, 2))

# Spread the Period across the columns

pimOutput <- pimOutput %>% spread(Period, Value, fill = 0)

# Rename KPs incorrectly named CVMs

pimOutput$Measure <- gsub("CVM", "KP", pimOutput$Measure)

# Write to a CSV

write_csv(pimOutput, paste0(outputDir, "pim_outputs_", runTime, ".csv"))

# -------------------------- Remove Objects ------------------------------------
rm(baseVariables, requiredVariables, pimOutput)
