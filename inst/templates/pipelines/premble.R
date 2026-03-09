#' ═══════════════════════════════════════════════════════════════════════════
#' Pipeline Preamble
#' ═══════════════════════════════════════════════════════════════════════════
#' 
#' 
#' Author: [Your Name]
#' Date: [Date]
#' Project: [Project ID]
#' 
#' ════════════════════════════════════��══════════════════════════════════════

# Project Metadata ----
project_id <- "P####"  # e.g., P1436
project_pi <- "[PI Name] ([Institution])"  # e.g., Anselm Mak (NUHS)
sample_type <- "[Sample type and dilution]"  # e.g., serum (1:400)
sample_groups <- "[Sample group information]"  # e.g., N = 88 (n = 44 cases, n = 44 controls)
array_platform <- "[Platform and antibody]"  # e.g., iOme, anti-IgG
data_prepared_by <- "[Your Name]"
analysis_date <- format(Sys.Date(), "%Y-%m-%d")

library(iOmeAiFunctions)
# Verify package version
message("Using iOmeAiFunctions version: ", packageVersion("iOmeAiFunctions"))

## File paths ----
expset_file <- "../../ExpSet_list.rds"
eset_name = "sample_ExpSet"
NetI_data_name <- "sample_ImputedlogMeanNetI" 
norm_data_name <- "sample_loess_normalised" 
ExpSet_list   <- readRDS(expset_file)



eset <- ExpSet_list[[eset_name]]

NetI <- assayDataElement(eset, NetI_data_name)
norm <- assayDataElement(eset, norm_data_name)
meta <- pData(eset)
features = fData(eset)

## Annotation columns for plots ----
s_cols <- c("Sample_Group", "Assay", "Assay_Date", "Batch_ID", "PSA_class")
(s_cols = s_cols[s_cols %in% s_cols])


