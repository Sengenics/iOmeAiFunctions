#File name: inst/downstream/pipeline/pFC_downstream_pipeline.R
#Language: r
#' ═══════════════════════════════════════════════════════════════════════════
#' pFC Downstream Analysis Pipeline - ExpSet_list Compatible
#' ═══════════════════════════════════════════════════════════════════════════
#' 
#' Adapted from Sengenics_downstream_tier_1_template.R to work with ExpSet_list
#' and iOmeAiFunctions package
#' 
#' Author: [Your Name]
#' Date: [Date]
#' Project: [Project ID]
#' 
#' ═══════════════════════════════════════════════��═══════════════════════════

# Project Metadata ----
#' Data Source Path: [e.g., SV-0020-0123-OME-SV1-001-NUHS-ANSELM > 3.BI-WORKING > WO1 > RUN 2]
#' Project Number: [e.g., P1436]
#' Project PI (Centre): [e.g., Anselm Mak (NUHS)]
#' Sample type(s) and dilution: [e.g., serum (1 in 400)]
#' Sample group(s): [e.g., N = 88 (n = 44 cases, n = 44 controls)]
#' Array platform and detecting antibody: [e.g., iOme, anti-IgG]
#' Data preparation by: [Your Name]
#' QC notes: [Get from project Wiki - flagged/failed/rerun samples]
#' Analytical Tier: [e.g., Tier 2]
#' Project aims: [As specified by project PI]
#' Requested comparisons: [e.g., (1) SLE vs. RA, (2) RA vs. HC, (3) SLE vs. HC]

# Load Required Packages ----
library(iOmeAiFunctions)
library(Biobase)
library(tidyverse)
library(pheatmap)

# Load ExpSet_list ----
#' Load your preprocessed ExpSet_list
#' This should contain normalized and NCF-filtered data
ExpSet_list <- readRDS("ExpSet_list.rds")  # ← EDIT THIS PATH

message("Loaded ", length(ExpSet_list), " ExpressionSets:")
print(names(ExpSet_list))

# Extract Data for pFC Analysis ----
#' Select normalized, NCF-filtered data
#' Common names: "loess_NCFed", "loess_combat_NCFed"
eset_name <- "loess_combat_NCFed"  # ← EDIT THIS

if (!eset_name %in% names(ExpSet_list)) {
	available <- paste(names(ExpSet_list), collapse = ", ")
	stop("ExpressionSet '", eset_name, "' not found. Available: ", available)
}

eset <- ExpSet_list[[eset_name]]

# Extract expression matrix and metadata
loess_combat <- Biobase::exprs(eset)  # Normalized expression data
meta <- Biobase::pData(eset)  # Sample metadata

message("\nData dimensions:")
message("  Features: ", nrow(loess_combat))
message("  Samples: ", ncol(loess_combat))
message("  Metadata columns: ", paste(colnames(meta), collapse = ", "))

# Exclude Pooled Normal Samples ----
pn_samples <- grep("^PN", colnames(loess_combat), value = TRUE, ignore.case = TRUE)
if (length(pn_samples) > 0) {
	message("\nExcluding ", length(pn_samples), " PN samples from clinical analysis")
	meta_clinical <- meta[!rownames(meta) %in% pn_samples, ]
	loess_combat_clinical <- loess_combat[, rownames(meta_clinical)]
} else {
	meta_clinical <- meta
	loess_combat_clinical <- loess_combat
}

# Select Features for Analysis ----
#' Typically use NCF-filtered features
#' If not already filtered, you can filter here
keep <- rownames(loess_combat_clinical)  # ← EDIT: Apply additional filtering if needed

# Remove control features
remove_features <- c(grep("^ZZ_con", keep, value = TRUE),
										 grep("^IGHG1$", keep, value = TRUE),
										 grep("^RBPJ$", keep, value = TRUE))

if (length(remove_features) > 0) {
	keep <- setdiff(keep, remove_features)
}

message("\nFeatures for analysis: ", length(keep))

# pFC Analysis Parameters ----
var <- "Sample_Group"  # ← EDIT: Variable of interest
groupPos <- "Case"  # ← EDIT: Positive/case group
groupNeg <- "Control"  # ← EDIT: Negative/control group
fold_change <- 2  # ← EDIT: FC threshold
p_val <- 0.2  # ← EDIT: p-value threshold
descriptor <- paste0(groupPos, "_vs_", groupNeg, "_pFC")
add_anno <- NULL  # ← EDIT: Additional annotations for heatmaps, e.g., c("PSA_class", "Batch")
cores <- 4  # ← EDIT: Number of cores

# Verify parameters
if (!var %in% colnames(meta_clinical)) {
	stop("Variable '", var, "' not found in metadata")
}

group_table <- table(meta_clinical[[var]])
message("\nSample groups:")
print(group_table)

if (!groupPos %in% names(group_table)) {
	stop("Group '", groupPos, "' not found")
}
if (!groupNeg %in% names(group_table)) {
	stop("Group '", groupNeg, "' not found")
}

# Prepare Data for pFC ----
#' Create a temporary ExpressionSet with filtered data
eset_filtered <- ExpressionSet(
	assayData = loess_combat_clinical[keep, , drop = FALSE],
	phenoData = AnnotatedDataFrame(meta_clinical)
)

# Run pFC Analysis ----
message("\n═══════════════════════════════════════════════════════════")
message("Running pFC Analysis")
message("═══════════════════════════════════════════════════════════")

pfc_results <- pFC_process(
	eset = eset_filtered,
	var = var,
	groupPos = groupPos,
	groupNeg = groupNeg,
	fold_change = fold_change,
	p_val = p_val,
	PSA_flag = FALSE,  # ← EDIT: Set TRUE if PSA_class column exists
	cores = cores
)

message("✓ pFC analysis completed")
message("  Significant hits: ", nrow(pfc_results$pfc_significant))

# Generate Plots ----
message("\nGenerating plots...")

pfc_plots <- pFC_plot(
	pfc_results = pfc_results,
	add_anno = add_anno
)

# Save Results ----
output_dir <- paste0("pFC_downstream_", descriptor, "_", Sys.Date())
dir.create(output_dir, showWarnings = FALSE)

pFC_save(
	pfc_results = pfc_results,
	pfc_plots = pfc_plots,
	descriptor = file.path(output_dir, descriptor),
	plot_width = 15,
	plot_height = 10
)

message("✓ Results saved to: ", output_dir)

# Additional Downstream Analysis (Optional) ----
#' 
#' You can add additional analyses here:
#' 
#' 1. Functional Profiling (if significant hits found):
#' if (nrow(pfc_results$pfc_significant) > 5) {
#'   source("path/to/functional_profile.R")
#'   functional_profile(
#'     limma_TT = pfc_results$pfc_stats,
#'     background_set = loess_combat_clinical,
#'     threshold_p = p_val,
#'     threshold_FC = fold_change,
#'     descriptor = descriptor
#'   )
#' }
#' 
#' 2. Correlation Analysis:
#' # Use corr.k() function from Sengenics_accessory_functions.R
#' 
#' 3. t-SNE Visualization:
#' loess_RC <- t(scale(t(loess_combat_clinical[keep, ]), scale = FALSE, center = TRUE))
#' tsne_func(loess_RC, meta_clinical, descriptor, color = var, shape = "PSA_class")
#' 

# Summary Report ----
message("\n═══════════════════════════════════════════════════════════")
message("Analysis Complete")
message("════════════════════════════════════════════════════��══════")
message("Date: ", Sys.Date())
message("Input data: ", eset_name)
message("Features analyzed: ", length(keep))
message("Significant hits: ", nrow(pfc_results$pfc_significant))
message("Output directory: ", output_dir)
message("═══════════════════════════════════════════════════════════\n")

# Session Info ----
sessionInfo()