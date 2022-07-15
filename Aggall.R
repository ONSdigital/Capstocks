aggAll <- function (.data, secHier, indHier, assHier, values) 
{
  #.data <- out
  #values <- colsToAggregate
  stopifnot(is.data.frame(.data))
  stopifnot(is.data.frame(secHier))
  stopifnot(is.data.frame(indHier))
  stopifnot(is.data.frame(assHier))
  stopifnot(is.character(values))
  minimalCols <- c("Period", "Sector", "Industry", "Asset")
  missingValues <- setdiff(c(minimalCols, values), colnames(.data))
  if (length(missingValues) > 0) {
    stop(paste(paste(missingValues, collapse = ", "), "not present in data."))
  }
  secLevels <- colnames(secHier)
  indLevels <- colnames(indHier)
  assLevels <- colnames(assHier)
  secLowest <- secLevels[length(secLevels)]
  indLowest <- indLevels[length(indLevels)]
  assLowest <- assLevels[length(assLevels)]
  missingSectors <- setdiff(.data$Sector, secHier[[secLowest]])
  missingIndustries <- setdiff(.data$Industry, indHier[[indLowest]])
  missingAssets <- setdiff(.data$Asset, assHier[[assLowest]])
  if (any(lapply(list(missingSectors, missingIndustries, missingAssets), 
                 FUN = length) > 0)) {
    msg <- paste("The following categories are in the data but not present in the lowest level of the provided asset hierarchies\n", 
                 paste("Missing Sectors: ", paste(missingSectors, 
                                                  collapse = ", "), "\n"), paste("Missing Industries: ", 
                                                                                 paste(missingIndustries, collapse = ", "), "\n"), 
                 paste("Missing Assets: ", paste(missingAssets, collapse = ", ")), 
                 collapse = "\n")
    stop(msg)
  }
  oldNames <- c("Sector", "Industry", "Asset")
  newNames <- c(secLowest, indLowest, assLowest)
  .data <- dplyr::rename_(.data, .dots = setNames(oldNames, 
                                                  newNames))
  .data <- .data %>% dplyr::left_join(secHier) %>% dplyr::left_join(indHier) %>% 
    dplyr::left_join(assHier)
  .data <- data.table(.data)
  groupings <- expand.grid(Sector = secLevels, Industry = indLevels, 
                           Asset = assLevels, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  nGroups <- nrow(groupings)
  results <- vector(mode = "list", nGroups)
  for (i in seq_len(nGroups)) {
    secLevel <- groupings[i, "Sector"]
    indLevel <- groupings[i, "Industry"]
    assLevel <- groupings[i, "Asset"]
    aggGroups <- paste(secLevel, indLevel, assLevel, "Period", 
                       sep = ",")
    tmp <- .data[, lapply(.SD, sum, na.rm = TRUE), .SDcols = values, 
                 by = aggGroups]
    tmp <- dplyr::rename_(tmp, Sector = secLevel, Industry = indLevel, 
                          Asset = assLevel)
    tmp <- dplyr::mutate(tmp, Sector_Level = secLevel, Industry_Level = indLevel, 
                         Asset_Level = assLevel)
    tmp <- dplyr::mutate(tmp, Series = paste0(Sector,Industry,Asset))
    if (exists("tmp_list")){
      tmp <- dplyr::filter(tmp, !Series %in% tmp_list)
    }
    tmp_list <- unique(c(tmp$Series, if(exists("tmp_list")) tmp_list))
    results[[i]] <- tmp
  }
  results <- dplyr::bind_rows(results)
  results <- dplyr::mutate(results, Group = paste(Sector_Level, 
                                                  Industry_Level, Asset_Level, sep = "/"))
  results <- dplyr::ungroup(results)
  results <- dplyr::arrange(results, Group, Sector, Industry, 
                            Asset, Period)
  return(results)
}
