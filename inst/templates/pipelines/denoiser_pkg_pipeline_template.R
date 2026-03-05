# =============================================================================
# Denoiser Pipeline Template
# =============================================================================
# 
# This template demonstrates how to run the denoiser pipeline from iOmeAiFunctions
# 
# Instructions:
#   1. Copy this file to your project directory
#   2. Edit the CONFIGURATION section below
#   3. Run the entire script or step-by-step
# 
# Generated from: iOmeAiFunctions package
# Date: [AUTO-GENERATED]
# =============================================================================

# SETUP ====

# Load package
library(iOmeAiFunctions)
library(Biobase)
library(dplyr)

# Set working directory (edit this!)
setwd("YOUR_PROJECT_PATH_HERE")

# CONFIGURATION ====

## Input files ----
expset_file <- "path/to/ExpSet_list.rds"
output_dir  <- "denoiser_output"

## Data selection ----
assay_name      <- "sample_ImputedlogMeanNetI"  # Assay to denoise
norm_assay_name <- "sample_loess_normalised"    # For visualization

## Pooled Normal samples ----
PN_column <- "Labels"
PN_value  <- "Pooled Normal"

## PC removal ----
n_PCs      <- 7
scale_pca  <- TRUE
center_pca <- TRUE

## Expected PN AAbs ----
# From limma analysis - edit this list!
PN_AAbs     <- c("PSIP1", "MAPK9", "MX1", "UBE2I", "PTPN11", "MLH1", "HCLS1", "MALL")
exp_PN_AAbs <- 4:12

## Cutpoint sweep ----
method  <- "singular"        # "singular", "MAD", or "AAD"
cut_seq <- seq(0.4, 3, 0.1)

## Antigen exclusions ----
exclude_antigens <- c("IGHG1", "RBPJ", "TRIM21", "dsDNA")

## Annotation columns for plots ----
s_cols <- c("Sample_Group", "Assay", "Assay_Date", "Batch_ID", "PSA_class")

## Project descriptor ----
descriptor <- "MyProject_NetI"


# STEP 1: Load Data ====

message("Loading ExpressionSet...")
ExpSet_list   <- readRDS(expset_file)
sample_ExpSet <- ExpSet_list$sample_ExpSet

message("ExpSets: ", paste(names(ExpSet_list), collapse = ", "))
message("Features: ", nrow(sample_ExpSet), " x Samples: ", ncol(sample_ExpSet))

# Extract matrices
NetI <- Biobase::assayDataElement(sample_ExpSet, assay_name)
norm <- Biobase::assayDataElement(sample_ExpSet, norm_assay_name)
meta <- Biobase::pData(sample_ExpSet)

# Validate
stopifnot(PN_column %in% colnames(meta))
message("PN samples: ", sum(meta[[PN_column]] == PN_value))

# Align data
NetI   <- NetI[, rownames(meta)]
s_cols <- s_cols[s_cols %in% colnames(meta)]

# Exclude interferent antigens
NetI_input <- NetI[setdiff(rownames(NetI), exclude_antigens), ]
message("Features after exclusion: ", nrow(NetI_input))


# STEP 2: PC Removal ====

message("\nStep 2: Removing principal components...")

denoise_results <- denoise_remove_PCs(
	eset       = sample_ExpSet,
	assay_name = assay_name,
	n_PCs      = n_PCs,
	scale      = scale_pca,
	center     = center_pca
)

message("Variance explained (first 10 PCs):")
message(paste(round(head(denoise_results$variance_explained, 10), 1), 
							collapse = "%, "), "%")

# Save intermediate results
dir.create(output_dir, showWarnings = FALSE)
saveRDS(denoise_results, 
				file.path(output_dir, paste0(descriptor, "_denoise_results.rds")))


# STEP 3: Cutpoint Sweep ====

message("\nStep 3: Testing cutpoints across PC levels...")

all_cutpoint_results <- list()

for (i in seq_along(denoise_results$denoised_data)) {
	message("  Testing ", i, " PC(s) removed...")
	
	cutpoint_results <- denoise_find_cutpoints(
		denoised_data = denoise_results$denoised_data[[i]],
		eset          = sample_ExpSet,
		PN_column     = PN_column,
		PN_value      = PN_value,
		PN_AAbs       = PN_AAbs,
		cut_seq       = cut_seq,
		method        = method
	)
	
	cutpoint_results$PCs_removed <- i
	all_cutpoint_results[[i]]    <- cutpoint_results
}

combined_cutpoint_results <- do.call(rbind, all_cutpoint_results)

write.csv(combined_cutpoint_results,
					file.path(output_dir, paste0(descriptor, "_parameter_optimisation_table.csv")))
message("Parameter optimisation table saved")


# STEP 4: Label Candidate Cutpoints ====

message("\nStep 4: Labeling candidate cutpoints...")

s_df <- label_candidate_cutpoints(
	cutpoint_results = combined_cutpoint_results,
	exp_PN_AAbs      = exp_PN_AAbs,
	cut_step         = cut_seq[2] - cut_seq[1]
)

print(s_df)
write.csv(s_df, 
					file.path(output_dir, paste0(descriptor, "_candidate_cutpoints.csv")))


# STEP 5: Select Optimal Parameters ====

message("\nStep 5: Selecting optimal parameters...")

top <- s_df %>%
	filter(PN_Aab_count_67_perc %in% exp_PN_AAbs) %>%
	filter(PCs_removed == min(PCs_removed, na.rm = TRUE)) %>%
	filter(TP_FP_ratio == max(TP_FP_ratio, na.rm = TRUE))

top <- top[1, ]

message("Optimal selection:")
message("  PCs removed : ", top$PCs_removed)
message("  Cutpoint    : ", top$cutpoint)
message("  PN AAb count: ", top$PN_Aab_count_67_perc)
message("  TP:FP ratio : ", top$TP_FP_ratio)

write.csv(top, 
					file.path(output_dir, paste0(descriptor, "_optimal_selection.csv")))


# STEP 6: Generate AAb-Called Matrices ====

message("\nStep 6: Generating AAb-called matrices...")

## Sample ----
aab_called_sample <- denoise_apply_cutpoint(
	denoised_data = denoise_results$denoised_data[[top$PCs_removed]],
	cutpoint      = top$cutpoint,
	method        = method
)

message("sample AAb-called: ", nrow(aab_called_sample), " x ", ncol(aab_called_sample))
write.csv(aab_called_sample,
					file.path(output_dir, paste0(descriptor, "_AAb_called_sample.csv")))

## Clinical ----
if ("clinical_ExpSet" %in% names(ExpSet_list)) {
	clinical_ExpSet    <- ExpSet_list$clinical_ExpSet
	clinical_assay     <- sub("^sample_", "clinical_", assay_name)
	
	clinical_denoise_results <- denoise_remove_PCs(
		eset       = clinical_ExpSet,
		assay_name = clinical_assay,
		n_PCs      = n_PCs,
		scale      = scale_pca,
		center     = center_pca
	)
	
	aab_called_clinical <- denoise_apply_cutpoint(
		denoised_data = clinical_denoise_results$denoised_data[[top$PCs_removed]],
		cutpoint      = top$cutpoint,
		method        = method
	)
	
	message("clinical AAb-called: ", nrow(aab_called_clinical), " x ", ncol(aab_called_clinical))
	write.csv(aab_called_clinical,
						file.path(output_dir, paste0(descriptor, "_AAb_called_clinical.csv")))
}


# STEP 7: Update ExpSet_list (Optional) ====

message("\nStep 7: Updating ExpressionSet...")

# Add denoised and AAb-called data to ExpressionSet
# (Implementation depends on your ExpSet_add_matrix_function)


# SUMMARY ====

message("\n=== PIPELINE COMPLETE ===")
message("  PCs removed : ", top$PCs_removed)
message("  Cutpoint    : ", top$cutpoint)
message("  PN AAb count: ", top$PN_Aab_count_67_perc)
message("  TP:FP ratio : ", top$TP_FP_ratio)
message("\nOutputs saved to: ", output_dir)
message("\nNext steps:")
message("  1. Review parameter_optimisation_table.csv")
message("  2. Check AAb_called_sample.csv")
message("  3. Visualize results with denoiser Shiny module")