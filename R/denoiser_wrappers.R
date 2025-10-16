#' Complete Denoising Pipeline
#'
#' Runs the complete denoising workflow: PC removal, cutpoint optimization,
#' and AAb calling. Suitable for use in AAb_caller_template.R
#'
#' @param eset ExpressionSet; ExpressionSet with expression data
#' @param assay_name Character; name of assay to use (default = NULL uses exprs())
#' @param n_PCs Integer; number of PCs to test (default = 7)
#' @param PN_column Character; metadata column identifying PN samples
#' @param PN_value Character; value identifying PN samples
#' @param PN_AAbs Character vector; expected PN AAbs from limma
#' @param exp_PN_AAbs Numeric vector; expected range of PN AAb count
#' @param cut_seq Numeric vector; cutpoint sequence to test
#' @param method Character; cutting method ("singular", "MAD", or "AAD")
#' @param output_dir Character; base output directory
#' @param descriptor Character; file descriptor
#' @param annotation_cols Character vector; columns for plot annotations
#' @param save_results Logical; whether to save results (default = TRUE)
#'
#' @return List containing:
#'   - \code{denoised_results}: Output from denoise_remove_PCs()
#'   - \code{cutpoint_results}: Data frame of all cutpoint analyses
#'   - \code{optimal_cutpoint}: Selected optimal parameters
#'   - \code{aab_called_data}: Final AAb-called matrix
#'   - \code{all_cutpoint_data}: List of AAb-called data for all tested cutpoints
#'
#' @export
#'
#' @examples
#' results <- denoise_pipeline(
#'   eset = eset,
#'   assay_name = "NetI",
#'   PN_AAbs = c("PSIP1", "MAPK9", "MX1"),
#'   exp_PN_AAbs = 6:12,
#'   output_dir = "AAb_called",
#'   descriptor = "NetI_IgG"
#' )
#' 
#' # Access final AAb-called data
#' aab_data <- results$aab_called_data
#'
denoise_pipeline <- function(eset, 
														 assay_name = NULL,
														 n_PCs = 7,
														 PN_column = "Sample_Group",
														 PN_value = "Pooled Normal",
														 PN_AAbs = NULL,
														 exp_PN_AAbs = 6:12,
														 cut_seq = seq(0.4, 3, 0.1),
														 method = "singular",
														 output_dir = "AAb_called",
														 descriptor = "denoised",
														 annotation_cols = c("Sample_Group"),
														 save_results = TRUE) {
	
	message("Starting denoising pipeline...")
	
	# Step 1: Remove PCs
	message("Step 1: Removing principal components...")
	denoised_results <- denoise_remove_PCs(
		eset = eset,
		assay_name = assay_name,
		n_PCs = n_PCs,
		scale = TRUE
	)
	
	# Step 2: Test cutpoints for each PC removal level
	message("Step 2: Testing cutpoints across PC removal levels...")
	
	all_cutpoint_results <- list()
	all_cutpoint_data <- list()
	
	for (i in seq_along(denoised_results$denoised_data)) {
		message(paste("  Testing with", i, "PC(s) removed..."))
		
		cutpoint_results <- denoise_find_cutpoints(
			denoised_data = denoised_results$denoised_data[[i]],
			eset = eset,
			PN_column = PN_column,
			PN_value = PN_value,
			PN_AAbs = PN_AAbs,
			cut_seq = cut_seq,
			method = method
		)
		
		cutpoint_results$PCs_removed <- i
		all_cutpoint_results[[i]] <- cutpoint_results
		
		# Store AAb-called data for all cutpoints
		cutpoint_data_list <- list()
		for (j in seq_along(cut_seq)) {
			cutpoint_data_list[[j]] <- denoise_apply_cutpoint(
				denoised_results$denoised_data[[i]],
				cutpoint = cut_seq[j],
				method = method
			)
		}
		all_cutpoint_data[[i]] <- cutpoint_data_list
	}
	
	# Combine all cutpoint results
	combined_cutpoint_results <- do.call(rbind, all_cutpoint_results)
	
	# Step 3: Select optimal cutpoint
	message("Step 3: Selecting optimal cutpoint...")
	optimal <- denoise_select_optimal_cutpoint(
		cutpoint_results = combined_cutpoint_results,
		exp_PN_AAbs = exp_PN_AAbs
	)
	
	message(paste("  Optimal: Cutpoint =", optimal$cutpoint, 
								", PCs removed =", optimal$PCs_removed))
	
	# Step 4: Generate final AAb-called data
	message("Step 4: Generating final AAb-called data...")
	final_aab_called <- denoise_apply_cutpoint(
		denoised_data = denoised_results$denoised_data[[optimal$PCs_removed]],
		cutpoint = optimal$cutpoint,
		method = method
	)
	
	# Step 5: Save results if requested
	if (save_results) {
		message("Step 5: Saving results...")
		
		# Get background data for border plots
		if (is.null(assay_name)) {
			background_data <- Biobase::exprs(eset)
		} else {
			background_data <- Biobase::assayDataElement(eset, assay_name)
		}
		
		save_complete_denoise_results(
			denoised_results = denoised_results,
			cutpoint_results = combined_cutpoint_results,
			optimal_cutpoint = optimal,
			aab_called_data = final_aab_called,
			output_dir = output_dir,
			descriptor = descriptor,
			eset = eset,
			annotation_cols = annotation_cols,
			background_data = background_data
		)
	}
	
	message("Denoising pipeline complete!")
	
	# Return all results
	return(list(
		denoised_results = denoised_results,
		cutpoint_results = combined_cutpoint_results,
		optimal_cutpoint = optimal,
		aab_called_data = final_aab_called,
		all_cutpoint_data = all_cutpoint_data
	))
}


#' Denoising Pipeline for Shiny App
#'
#' Modified denoising pipeline that returns all intermediate results for
#' interactive visualization in Shiny app. Does not save files by default.
#'
#' @param eset ExpressionSet; ExpressionSet with expression data
#' @param assay_name Character; name of assay to use
#' @param n_PCs Integer; number of PCs to test
#' @param PN_column Character; metadata column identifying PN samples
#' @param PN_value Character; value identifying PN samples
#' @param PN_AAbs Character vector; expected PN AAbs
#' @param cut_seq Numeric vector; cutpoint sequence to test
#' @param method Character; cutting method
#'
#' @return List with detailed results for Shiny visualization:
#'   - \code{denoised_list}: List of all denoised matrices (1 to n_PCs removed)
#'   - \code{pca_variance}: Variance explained by each PC
#'   - \code{cutpoint_tables}: List of cutpoint result tables for each PC level
#'   - \code{aab_called_matrices}: List of AAb-called matrices for each PC/cutpoint combo
#'   - \code{metadata}: Metadata from eset
#'
#' @export
#'
#' @examples
#' shiny_results <- denoise_shiny(
#'   eset = eset,
#'   assay_name = "NetI",
#'   n_PCs = 3,
#'   PN_AAbs = expected_AAbs
#' )
#' 
#' # Access specific denoised matrix
#' denoised_2PC <- shiny_results$denoised_list[[2]]
#' 
#' # Access cutpoint results for 2 PCs removed
#' cutpoint_table <- shiny_results$cutpoint_tables[[2]]
#'
denoise_shiny <- function(eset,
													assay_name = NULL,
													n_PCs = 3,
													PN_column = "Sample_Group",
													PN_value = "Pooled Normal",
													PN_AAbs = NULL,
													cut_seq = seq(0.4, 3, 0.1),
													method = "singular") {
	
	# Step 1: Remove PCs
	denoised_results <- denoise_remove_PCs(
		eset = eset,
		assay_name = assay_name,
		n_PCs = n_PCs,
		scale = TRUE
	)
	
	# Step 2: Calculate cutpoints for each PC level
	cutpoint_tables <- list()
	aab_called_matrices <- list()
	
	for (i in seq_along(denoised_results$denoised_data)) {
		# Calculate cutpoint metrics
		cutpoint_tables[[i]] <- denoise_find_cutpoints(
			denoised_data = denoised_results$denoised_data[[i]],
			eset = eset,
			PN_column = PN_column,
			PN_value = PN_value,
			PN_AAbs = PN_AAbs,
			cut_seq = cut_seq,
			method = method
		)
		
		# Generate AAb-called matrices for each cutpoint
		aab_matrices_for_pc <- list()
		for (j in seq_along(cut_seq)) {
			aab_matrices_for_pc[[j]] <- denoise_apply_cutpoint(
				denoised_results$denoised_data[[i]],
				cutpoint = cut_seq[j],
				method = method
			)
		}
		aab_called_matrices[[i]] <- aab_matrices_for_pc
	}
	
	# Return comprehensive results for Shiny
	return(list(
		denoised_list = denoised_results$denoised_data,
		pca_variance = denoised_results$variance_explained,
		pca_result = denoised_results$pca_result,
		cutpoint_tables = cutpoint_tables,
		aab_called_matrices = aab_called_matrices,
		metadata = Biobase::pData(eset),
		cut_seq = cut_seq,
		method = method
	))
}


#' Generate AAb Caller Template Script
#'
#' Creates an AAb_caller_template.R script with the denoiser functions
#' pre-configured for a specific project.
#'
#' @param output_file Character; path for output R script
#' @param project_name Character; project name for documentation
#' @param eset_file Character; path to RDS file containing ExpressionSet
#' @param assay_name Character; assay name to use
#' @param n_PCs Integer; number of PCs to test
#' @param PN_AAbs Character vector; expected PN AAbs
#' @param exp_PN_AAbs Numeric vector; expected PN AAb count range
#' @param annotation_cols Character vector; annotation columns
#'
#' @return Invisible NULL (script written to file)
#' @export
#'
#' @examples
#' generate_aab_caller_template(
#'   output_file = "AAb_caller_template_MyProject.R",
#'   project_name = "My Project",
#'   eset_file = "data/eset.rds",
#'   assay_name = "NetI",
#'   PN_AAbs = c("PSIP1", "MAPK9", "MX1"),
#'   exp_PN_AAbs = 6:12
#' )
#'
generate_aab_caller_template <- function(output_file = "AAb_caller_template.R",
																				 project_name = "My Project",
																				 eset_file = "eset.rds",
																				 assay_name = "NetI",
																				 n_PCs = 7,
																				 PN_AAbs = NULL,
																				 exp_PN_AAbs = 6:12,
																				 annotation_cols = c("Sample_Group", "Batch_ID")) {
	
	# Create template content
	template <- paste0('
# AAb Caller Template - ', project_name, '
# Generated: ', Sys.Date(), '
# User: ', Sys.getenv("USER"), '

# Load required libraries
library(iOmeAiFunctions)
library(Biobase)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# File paths
eset_file <- "', eset_file, '"
output_dir <- "AAb_called"

# Denoising parameters
n_PCs <- ', n_PCs, '
assay_name <- "', assay_name, '"
method <- "singular"  # Options: "singular", "MAD", "AAD"

# PN identification
PN_column <- "Sample_Group"
PN_value <- "Pooled Normal"

# Expected PN AAbs (from limma analysis)
PN_AAbs <- c(', paste0('"', PN_AAbs, '"', collapse = ", "), ')

# Expected PN AAb count range
exp_PN_AAbs <- ', deparse(exp_PN_AAbs), '

# Cutpoint sequence to test
cut_seq <- seq(0.4, 3, 0.1)

# Annotation columns for plots
annotation_cols <- c(', paste0('"', annotation_cols, '"', collapse = ", "), ')

# ==============================================================================
# LOAD DATA
# ==============================================================================

message("Loading ExpressionSet...")
eset <- readRDS(eset_file)

message("Samples: ", ncol(eset))
message("Features: ", nrow(eset))

# ==============================================================================
# RUN DENOISING PIPELINE
# ==============================================================================

results <- denoise_pipeline(
  eset = eset,
  assay_name = assay_name,
  n_PCs = n_PCs,
  PN_column = PN_column,
  PN_value = PN_value,
  PN_AAbs = PN_AAbs,
  exp_PN_AAbs = exp_PN_AAbs,
  cut_seq = cut_seq,
  method = method,
  output_dir = output_dir,
  descriptor = "', project_name, '",
  annotation_cols = annotation_cols,
  save_results = TRUE
)

# ==============================================================================
# RESULTS SUMMARY
# ==============================================================================

message("\\n=== DENOISING RESULTS SUMMARY ===")
message("Optimal PCs removed: ", results$optimal_cutpoint$PCs_removed)
message("Optimal cutpoint: ", results$optimal_cutpoint$cutpoint)
message("PN AAb count: ", results$optimal_cutpoint$PN_Aab_count_67_perc)
message("TP:FP ratio: ", results$optimal_cutpoint$TP_FP_ratio)
message("ZZ_con2 rate: ", results$optimal_cutpoint$zz_2_frac)
message("ZZ_con4 rate: ", results$optimal_cutpoint$zz_4_frac)
message("Unique AAbs: ", results$optimal_cutpoint$N_unique_AAbs)
message("Median AAbs/sample: ", results$optimal_cutpoint$sample_AAb_median)

# ==============================================================================
# ACCESS RESULTS
# ==============================================================================

# Final AAb-called matrix
aab_called <- results$aab_called_data

# Save to workspace
save(results, aab_called, file = file.path(output_dir, "', project_name, '_denoising_workspace.RData"))

message("\\nPipeline complete! Results saved to: ", output_dir)
')

# Write to file
writeLines(template, output_file)

message(paste("AAb caller template generated:", output_file))
invisible(NULL)
}