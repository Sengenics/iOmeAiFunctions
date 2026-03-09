

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

run_debug = T
if(run_debug){
	local_path <- "~/iOmeAiFunctions/"
	devtools_pkg <- "devtools"
	if (require(devtools_pkg, character.only = TRUE, quietly = TRUE)) {
		get("document", envir = asNamespace(devtools_pkg))(local_path)
	}
	do.call("::", list(as.name("pkgload"), as.name("load_all")))(local_path)
}else{
	library(iOmeAiFunctions)
}

# Verify package version
message("Using iOmeAiFunctions version: ", packageVersion("iOmeAiFunctions"))

## File paths ----
expset_file <- "../../ExpSet_list.rds"
eset_name = "clinical_ExpSet"
NetI_data_name <- "clinical_ImputedlogMeanNetI" 
norm_data_name <- "clinical_loess_normalised" 
ExpSet_list   <- readRDS(expset_file)



eset <- ExpSet_list[[eset_name]]

NetI <- assayDataElement(eset, NetI_data_name)
norm <- assayDataElement(eset, norm_data_name)
meta <- pData(eset)
features = fData(eset)

## Annotation columns for plots ----
s_cols <- c("Sample_Group", "Assay", "Assay_Date", "Batch_ID", "PSA_class")
(s_cols = s_cols[s_cols %in% s_cols])


message("\nUsing ExpressionSet: ", eset_name)
message("Dimensions: ", nrow(eset), " features × ", ncol(eset), " samples")
message("Metadata columns: ", paste(colnames(Biobase::pData(eset)), collapse = ", "))

# Exclude Pooled Normal Samples ----
#' pFC analysis should exclude PN samples
metadata <- Biobase::pData(eset)
pn_samples <- grep("^PN", colnames(eset), value = TRUE, ignore.case = TRUE)
length(pn_samples)


# Set pFC Parameters ----
var <- "Sample_Group"  # ← EDIT: metadata column name
groupPos <- "RA"  # ← EDIT: case/positive group name
groupNeg <- "SLE"  # ← EDIT: control/negative group name
fold_change <- 2  # ← EDIT: FC threshold (default: 2)
p_val <- 0.2  # ← EDIT: p-value threshold (default: 0.2)
descriptor <- paste0(groupPos, "_vs_", groupNeg, "_pFC")  # Output file prefix
cores <- 4  # ← EDIT: number of CPU cores to use

# Verify groups exist
if (!var %in% colnames(Biobase::pData(eset))) {
	stop("Variable '", var, "' not found in metadata. Available: ", 
			 paste(colnames(Biobase::pData(eset)), collapse = ", "))
}

group_values <- unique(as.character(Biobase::pData(eset)[[var]]))
if (!groupPos %in% group_values) {
	stop("Group '", groupPos, "' not found in variable '", var, "'. Available: ", 
			 paste(group_values, collapse = ", "))
}
if (!groupNeg %in% group_values) {
	stop("Group '", groupNeg, "' not found in variable '", var, "'. Available: ", 
			 paste(group_values, collapse = ", "))
}

message("\n═══════════════════════════════════════════════════════════")
message("pFC Analysis Configuration")
message("═══════════════════════════════════════════════════════════")
message("Variable: ", var)
message("Positive Group: ", groupPos, " (n = ", sum(Biobase::pData(eset)[[var]] == groupPos), ")")
message("Negative Group: ", groupNeg, " (n = ", sum(Biobase::pData(eset)[[var]] == groupNeg), ")")
message("FC Threshold: ", fold_change)
message("P-value Threshold: ", p_val)
message("Cores: ", cores)
message("═══════════════════════════════════════════════════════════\n")

# Run pFC Analysis ----
message("Running pFC analysis...")
start_time <- Sys.time()

assayDataElementNames(eset)

keep = features %>% 
	filter(ncf == 'retain') %>% 
	pull(Protein)

filtered_eset = eset[keep,]

pfc_results <- pFC_process(
	eset = filtered_eset,
	assay_name = norm_data_name,
	var = var,
	groupPos = groupPos,
	groupNeg = groupNeg,
	fold_change = fold_change,
	p_val = p_val,
	PSA_flag = FALSE,  # Set TRUE if you have PSA_class column
	cores = cores
)

end_time <- Sys.time()
message("✓ Analysis completed in ", round(difftime(end_time, start_time, units = "secs"), 1), " seconds")

# Generate Plots ----
message("\nGenerating plots...")

pfc_plots <- pFC_plot(
	pfc_results = pfc_results,
	add_anno = NULL  # ← EDIT: add metadata columns for heatmap annotation, e.g., c("PSA_class", "Batch")
)

message("✓ Plots generated")

# Save Results ----
message("\nSaving results...")

# Create output directory
output_dir <- paste0("pFC_results_", descriptor, "_", Sys.Date())
dir.create(output_dir, showWarnings = FALSE)

# Save all results
pFC_save(
	pfc_results = pfc_results,
	pfc_plots = pfc_plots,
	descriptor = file.path(output_dir, descriptor),
	plot_width = 15,
	plot_height = 10
)

message("✓ Results saved to: ", output_dir)

# Summary Report ----
message("\n═══════════════════════════════════════════════════════════")
message("pFC Analysis Summary")
message("═══════════════════════════════════════════════════════════")
message("Project: ", project_id)
message("Date: ", analysis_date)
message("Prepared by: ", data_prepared_by)
message("\nResults:")
message("  Total features tested: ", nrow(pfc_results$pfc_stats))
message("  Significant hits (p < ", p_val, "): ", nrow(pfc_results$pfc_significant))
message("  Sample size (", groupPos, "): ", sum(pfc_results$metadata[[var]] == groupPos))
message("  Sample size (", groupNeg, "): ", sum(pfc_results$metadata[[var]] == groupNeg))
message("\nOutput directory: ", output_dir)
message("═══════════════════════════════════════════════════════════\n")

# Optional: View Top Hits ----
if (nrow(pfc_results$pfc_significant) > 0) {
	message("\nTop 10 Significant Hits:")
	print(head(pfc_results$pfc_significant[order(pfc_results$pfc_significant$p_value), ], 10))
}

# Session Info ----
message("\nSession Info:")
sessionInfo()

# End of Pipeline ----
message("\n✓ Pipeline completed successfully!")
message("Review results in: ", output_dir)