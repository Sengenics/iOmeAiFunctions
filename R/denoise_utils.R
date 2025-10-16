#' @importFrom magrittr %>%
NULL

#' Extract Expression Data from ExpressionSet
#'
#' Utility function to extract expression matrix from ExpressionSet
#'
#' @param eset ExpressionSet object
#' @param assay_name Character; name of assay (default = NULL uses exprs())
#'
#' @return Matrix of expression data
#' @export
#'
get_expression_data <- function(eset, assay_name = NULL) {
	if (!inherits(eset, "ExpressionSet")) {
		stop("eset must be an ExpressionSet object")
	}
	
	if (is.null(assay_name)) {
		return(Biobase::exprs(eset))
	} else {
		return(Biobase::assayDataElement(eset, assay_name))
	}
}


#' Get PN Sample IDs with Flexible Detection
#'
#' Extract sample IDs for Pooled Normal samples from ExpressionSet
#' Tries Sample_type == 'TR' first, then falls back to other methods
#'
#' @param eset ExpressionSet object
#' @param PN_column Character; column name identifying PN samples (default = "Sample_type")
#' @param PN_value Character; value identifying PN samples (default = "TR")
#'
#' @return Character vector of PN sample IDs
#' @export
#'
get_PN_samples <- function(eset, 
													 PN_column = "Sample_type", 
													 PN_value = "TR") {
	
	metadata <- Biobase::pData(eset)
	
	if (!PN_column %in% colnames(metadata)) {
		stop(paste("Column", PN_column, "not found in pData(eset).",
							 "Available columns:", paste(colnames(metadata), collapse = ", ")))
	}
	
	PN_samples <- rownames(metadata)[metadata[[PN_column]] == PN_value]
	
	if (length(PN_samples) == 0) {
		warning(paste0("No PN samples found with ", PN_column, " == '", PN_value, "'"))
	}
	
	return(PN_samples)
}


#' Get Clinical Sample IDs
#'
#' Extract sample IDs for clinical (non-PN) samples from ExpressionSet
#'
#' @param eset ExpressionSet object
#' @param PN_column Character; column name identifying PN samples
#' @param PN_value Character; value identifying PN samples
#'
#' @return Character vector of clinical sample IDs
#' @export
#'
get_clinical_samples <- function(eset,
																 PN_column = "Sample_type",
																 PN_value = "TR") {
	
	metadata <- Biobase::pData(eset)
	
	if (!PN_column %in% colnames(metadata)) {
		stop(paste("Column", PN_column, "not found in pData(eset)"))
	}
	
	clinical_samples <- rownames(metadata)[metadata[[PN_column]] != PN_value]
	
	return(clinical_samples)
}


#' Validate Denoising Inputs (Updated for TR)
#'
#' Check that all required inputs are valid for denoising pipeline
#'
#' @param eset ExpressionSet object
#' @param assay_name Character; assay name
#' @param PN_column Character; PN identifier column (default = "Sample_type")
#' @param PN_value Character; PN identifier value (default = "TR")
#'
#' @return Logical TRUE if valid, stops with error if not
#' @export
#'
validate_denoise_inputs <- function(eset, assay_name = NULL,
																		PN_column = "Sample_type",
																		PN_value = "TR") {
	
	# Check eset
	if (!inherits(eset, "ExpressionSet")) {
		stop("eset must be an ExpressionSet object")
	}
	
	# Check assay exists
	if (!is.null(assay_name)) {
		available_assays <- Biobase::assayDataElementNames(eset)
		if (!assay_name %in% available_assays) {
			stop(paste("Assay", assay_name, "not found. Available:",
								 paste(available_assays, collapse = ", ")))
		}
	}
	
	# Check PN column exists
	metadata <- Biobase::pData(eset)
	if (!PN_column %in% colnames(metadata)) {
		available_cols <- colnames(metadata)
		stop(paste0(
			"Column '", PN_column, "' not found in metadata.\n",
			"Available columns: ", paste(available_cols, collapse = ", "), "\n",
			"Suggestion: Try 'Sample_type', 'Labels', or 'Sample_Group'"
		))
	}
	
	# Check PN samples exist - with more detail
	unique_values <- unique(metadata[[PN_column]])
	message("Available values in '", PN_column, "': ", paste(unique_values, collapse = ", "))
	
	PN_samples <- rownames(metadata)[metadata[[PN_column]] == PN_value]
	
	if (length(PN_samples) == 0) {
		# Show what's available
		value_counts <- table(metadata[[PN_column]])
		
		stop(paste0(
			"No samples found with '", PN_column, "' = '", PN_value, "'.\n\n",
			"Available values and counts:\n",
			paste(names(value_counts), ":", value_counts, collapse = "\n"),
			"\n\nSuggestion: Update PN_column and PN_value in the Denoiser Controls."
		))
	}
	
	if (length(PN_samples) < 3) {
		stop(paste("Insufficient PN samples found (n =", length(PN_samples), 
							 "). Need at least 3 for reliable denoising."))
	}
	
	# Check for required control antigens
	expr_data <- get_expression_data(eset, assay_name)
	required_controls <- c("ZZ_con2", "ZZ_con4")
	missing_controls <- setdiff(required_controls, rownames(expr_data))
	
	if (length(missing_controls) > 0) {
		warning(paste("Missing control antigens:", 
									paste(missing_controls, collapse = ", ")))
	}
	
	message("✅ Validation passed!")
	message(paste("  Samples:", ncol(expr_data)))
	message(paste("  Features:", nrow(expr_data)))
	message(paste("  PN samples:", length(PN_samples)))
	message(paste("  PN identifier: '", PN_column, "' = '", PN_value, "'", sep = ""))
	
	return(TRUE)
}


#' Compare Denoising Parameters
#'
#' Create comparison table of different denoising parameter combinations
#'
#' @param cutpoint_results Data frame; combined results from multiple PC levels
#' @param top_n Integer; number of top results to return (default = 10)
#' @param sort_by Character; column to sort by (default = "TP_FP_ratio")
#'
#' @return Data frame of top parameter combinations
#' @export
#'
compare_denoise_parameters <- function(cutpoint_results,
																			 top_n = 10,
																			 sort_by = "TP_FP_ratio") {
	
	if (!sort_by %in% colnames(cutpoint_results)) {
		stop(paste("Column", sort_by, "not found in results"))
	}
	
	# Sort and select top results
	sorted_results <- cutpoint_results[order(-cutpoint_results[[sort_by]]), ]
	top_results <- head(sorted_results, top_n)
	
	# Clean up for display
	display_cols <- c("PCs_removed", "cutpoint", "PN_Aab_count_67_perc",
										"PN_AAb_hit_rate", "TP_FP_ratio", "zz_2_frac", "zz_4_frac",
										"N_unique_AAbs", "sample_AAb_median")
	
	display_cols <- intersect(display_cols, colnames(top_results))
	
	return(top_results[, display_cols])
}


#' Calculate AAb Summary Statistics
#'
#' Generate summary statistics for AAb-called data
#'
#' @param aab_called_data Data frame; AAb-called matrix
#' @param eset ExpressionSet; for metadata
#' @param group_by Character; metadata column to group by (optional)
#'
#' @return Data frame with summary statistics
#' @export
#'
calculate_aab_summary <- function(aab_called_data, eset, group_by = NULL) {
	
	# Overall statistics
	total_antigens <- nrow(aab_called_data)
	total_samples <- ncol(aab_called_data)
	unique_aabs <- sum(rowSums(aab_called_data) > 0)
	
	# Per-sample statistics
	aabs_per_sample <- colSums(aab_called_data > 0)
	
	# Per-antigen statistics
	samples_per_antigen <- rowSums(aab_called_data > 0)
	antigen_prevalence <- (samples_per_antigen / total_samples) * 100
	
	summary_stats <- data.frame(
		Metric = c("Total Antigens", "Total Samples", "Unique AAbs Detected",
							 "Mean AAbs per Sample", "Median AAbs per Sample",
							 "Max AAbs per Sample", "Mean Antigen Prevalence (%)",
							 "Median Antigen Prevalence (%)"),
		Value = c(total_antigens, total_samples, unique_aabs,
							round(mean(aabs_per_sample), 1),
							round(median(aabs_per_sample), 1),
							max(aabs_per_sample),
							round(mean(antigen_prevalence[antigen_prevalence > 0]), 1),
							round(median(antigen_prevalence[antigen_prevalence > 0]), 1))
	)
	
	# Group-specific statistics if requested
	if (!is.null(group_by)) {
		metadata <- Biobase::pData(eset)
		
		if (group_by %in% colnames(metadata)) {
			groups <- unique(metadata[[group_by]])
			
			for (grp in groups) {
				grp_samples <- rownames(metadata)[metadata[[group_by]] == grp]
				grp_samples <- intersect(grp_samples, colnames(aab_called_data))
				
				if (length(grp_samples) > 0) {
					grp_data <- aab_called_data[, grp_samples, drop = FALSE]
					grp_aabs <- colSums(grp_data > 0)
					
					summary_stats <- rbind(summary_stats, data.frame(
						Metric = paste(grp, "- Mean AAbs per Sample"),
						Value = round(mean(grp_aabs), 1)
					))
					
					summary_stats <- rbind(summary_stats, data.frame(
						Metric = paste(grp, "- Median AAbs per Sample"),
						Value = round(median(grp_aabs), 1)
					))
				}
			}
		}
	}
	
	return(summary_stats)
}


#' Diagnose ExpressionSet List Structure
#'
#' Print detailed information about ExpSet_list structure for debugging
#'
#' @param ExpSet_list List of ExpressionSets
#'
#' @return Invisible NULL (prints diagnostics)
#' @export
diagnose_ExpSet_list <- function(ExpSet_list) {
	
	cat("═══════════════════════════════════════════════════════════\n")
	cat("EXPRESSIONSET LIST DIAGNOSTICS\n")
	cat("═══════════════════════════════════════════════════════════\n\n")
	
	cat("Number of ExpressionSets:", length(ExpSet_list), "\n")
	cat("Names:", paste(names(ExpSet_list), collapse = ", "), "\n\n")
	
	for (eset_name in names(ExpSet_list)) {
		cat("───────────────────────────────────────────────────────────\n")
		cat("ExpressionSet:", eset_name, "\n")
		cat("───────────────────────────────────────────────────────────\n")
		
		eset <- ExpSet_list[[eset_name]]
		
		if (!inherits(eset, "ExpressionSet")) {
			cat("  ⚠️  NOT an ExpressionSet! Class:", class(eset), "\n\n")
			next
		}
		
		cat("  Dimensions:", nrow(eset), "features ×", ncol(eset), "samples\n")
		
		# Check assayData
		cat("  AssayData class:", class(eset@assayData), "\n")
		
		assay_names <- Biobase::assayDataElementNames(eset)
		cat("  Available assays (", length(assay_names), "):\n")
		
		for (assay in assay_names) {
			assay_data <- Biobase::assayDataElement(eset, assay)
			cat("    -", assay, "\n")
			cat("      Class:", class(assay_data), "\n")
			cat("      Dimensions:", paste(dim(assay_data), collapse = " × "), "\n")
			cat("      Is matrix:", is.matrix(assay_data), "\n")
			cat("      Is numeric:", is.numeric(assay_data), "\n")
		}
		
		# Check phenoData
		pdata_cols <- colnames(Biobase::pData(eset))
		cat("  PhenoData columns (", length(pdata_cols), "):", 
				paste(head(pdata_cols, 10), collapse = ", "), "\n")
		
		cat("\n")
	}
	
	cat("═══════════════════════════════════════════════════════════\n")
	
	invisible(NULL)
}


#' Quick Inspect ExpressionSet for Denoising
#'
#' Print detailed diagnostics about an ExpressionSet
#'
#' @param eset ExpressionSet object
#' @param assay_name Character; assay to inspect
#'
#' @export
quick_inspect_eset <- function(eset, assay_name = NULL) {
	
	cat("\n╔═══════════════════════════════════════════════════════════╗\n")
	cat("║           EXPRESSIONSET QUICK INSPECTION                 ║\n")
	cat("╚═══════════════════════════════════════════════════════════╝\n\n")
	
	# Basic info
	cat("📦 ExpressionSet Info:\n")
	cat("   Class:", class(eset), "\n")
	
	# Fix: Use methods::validObject instead of Biobase::validObject
	is_valid <- tryCatch({
		methods::validObject(eset)
		TRUE
	}, error = function(e) {
		FALSE
	})
	cat("   Valid:", is_valid, "\n")
	cat("   Dimensions:", nrow(eset), "features ×", ncol(eset), "samples\n\n")
	
	# AssayData
	cat("🔬 AssayData:\n")
	cat("   Storage mode:", Biobase::storageMode(Biobase::assayData(eset)), "\n")
	assay_names <- Biobase::assayDataElementNames(eset)
	cat("   Available assays (", length(assay_names), "):\n")
	for (a in assay_names) {
		cat("      -", a, "\n")
	}
	cat("\n")
	
	# Check exprs()
	cat("📊 exprs() [Default assay]:\n")
	exprs_data <- Biobase::exprs(eset)
	cat("   Class:", class(exprs_data), "\n")
	cat("   Is matrix:", is.matrix(exprs_data), "\n")
	cat("   Is numeric:", is.numeric(exprs_data), "\n")
	if (is.matrix(exprs_data) || is.data.frame(exprs_data)) {
		cat("   Dimensions:", paste(dim(exprs_data), collapse = " × "), "\n")
		cat("   Range: [", round(min(exprs_data, na.rm = TRUE), 2), ", ",
				round(max(exprs_data, na.rm = TRUE), 2), "]\n")
		cat("   Contains NA:", any(is.na(exprs_data)), "\n")
	}
	cat("\n")
	
	# Check specific assay if requested
	if (!is.null(assay_name)) {
		cat("🎯 Requested assay: '", assay_name, "'\n", sep = "")
		if (assay_name %in% assay_names) {
			assay_data <- Biobase::assayDataElement(eset, assay_name)
			cat("   ✅ Found!\n")
			cat("   Class:", class(assay_data), "\n")
			cat("   Is matrix:", is.matrix(assay_data), "\n")
			cat("   Is numeric:", is.numeric(assay_data), "\n")
			if (is.matrix(assay_data) || is.data.frame(assay_data)) {
				cat("   Dimensions:", paste(dim(assay_data), collapse = " × "), "\n")
				cat("   Range: [", round(min(assay_data, na.rm = TRUE), 2), ", ",
						round(max(assay_data, na.rm = TRUE), 2), "]\n")
			}
		} else {
			cat("   ❌ NOT FOUND\n")
		}
		cat("\n")
	}
	
	# PhenoData
	cat("👥 PhenoData:\n")
	pdata <- Biobase::pData(eset)
	cat("   Samples:", nrow(pdata), "\n")
	cat("   Variables:", ncol(pdata), "\n")
	cat("   Column names:", paste(head(colnames(pdata), 10), collapse = ", "), "\n")
	if (ncol(pdata) > 10) cat("   ... and", ncol(pdata) - 10, "more\n")
	cat("\n")
	
	# Check for PN samples
	cat("🧪 Pooled Normal Check:\n")
	if ("Sample_type" %in% colnames(pdata)) {
		sample_types <- table(pdata$Sample_type)
		cat("   Sample types found:\n")
		for (st in names(sample_types)) {
			cat("      -", st, ":", sample_types[st], "samples\n")
		}
		
		if ("TR" %in% pdata$Sample_type) {
			cat("   ✅ Found TR (technical replicate/PN) samples\n")
		}
	} else if ("Sample_Group" %in% colnames(pdata)) {
		sample_groups <- table(pdata$Sample_Group)
		cat("   Sample groups found:\n")
		for (grp in names(sample_groups)) {
			cat("      -", grp, ":", sample_groups[grp], "samples\n")
		}
	} else if ("Labels" %in% colnames(pdata)) {
		labels <- table(pdata$Labels)
		cat("   Labels found:\n")
		for (lbl in names(labels)) {
			cat("      -", lbl, ":", labels[lbl], "samples\n")
		}
	} else {
		cat("   ⚠️  No standard PN identifier column found\n")
	}
	cat("\n")
	
	# Sample some data
	cat("📋 Sample of exprs() data:\n")
	print(head(exprs_data[, 1:min(5, ncol(exprs_data))], 3))
	cat("\n")
	
	cat("═══════════════════════════════════════════════════════════\n\n")
	
	invisible(eset)
}


#' Detect PN Identifier in Metadata (Updated for TR)
#'
#' Auto-detect the pooled normal identifier from metadata
#' Priority: Sample_type == 'TR', then Labels column, then Sample_Group
#'
#' @param eset ExpressionSet object
#'
#' @return List with PN_column and PN_value
#' @export
detect_PN_identifier <- function(eset) {
	
	metadata <- Biobase::pData(eset)
	
	# Priority 1: Sample_type == 'TR'
	if ("Sample_type" %in% colnames(metadata)) {
		if ("TR" %in% unique(metadata$Sample_type)) {
			n_tr <- sum(metadata$Sample_type == "TR")
			message("✓ Auto-detected PN samples: Sample_type = 'TR' (", n_tr, " samples)")
			return(list(PN_column = "Sample_type", PN_value = "TR"))
		}
	}
	
	# Priority 2: Labels column
	if ("Labels" %in% colnames(metadata)) {
		unique_labels <- unique(metadata$Labels)
		message("Labels column found with values: ", paste(unique_labels, collapse = ", "))
		
		# Try common PN identifiers in Labels
		pn_variants <- c("Pooled Normal", "PN", "Pool", "TR", "pooled_normal", 
										 "pooled normal", "POOL", "Pooled_Normal")
		
		for (variant in pn_variants) {
			if (variant %in% unique_labels) {
				n_samples <- sum(metadata$Labels == variant)
				message("✓ Auto-detected PN samples: Labels = '", variant, "' (", n_samples, " samples)")
				return(list(PN_column = "Labels", PN_value = variant))
			}
		}
		
		# If no match, return first label value
		message("ℹ No standard PN identifier in Labels. Using first value: '", unique_labels[1], "'")
		return(list(PN_column = "Labels", PN_value = unique_labels[1]))
	}
	
	# Priority 3: Sample_Group column
	group_cols <- c("Sample_Group", "sample_group", "SampleGroup", "Group", "group")
	
	for (col in group_cols) {
		if (col %in% colnames(metadata)) {
			unique_values <- unique(metadata[[col]])
			
			# Try common PN identifiers
			pn_variants <- c("Pooled Normal", "PN", "Pool", "TR", "pooled_normal")
			
			for (variant in pn_variants) {
				if (variant %in% unique_values) {
					n_samples <- sum(metadata[[col]] == variant)
					message("✓ Auto-detected PN samples: ", col, " = '", variant, "' (", n_samples, " samples)")
					return(list(PN_column = col, PN_value = variant))
				}
			}
		}
	}
	
	# Default fallback
	message("⚠ Could not auto-detect PN samples. Using defaults.")
	return(list(PN_column = "Sample_type", PN_value = "TR"))
}


#' Get Available PN Options from Metadata
#'
#' Return all possible PN identifier options from metadata
#'
#' @param eset ExpressionSet object
#'
#' @return Named list of column -> unique values
#' @export
get_PN_options <- function(eset) {
	
	metadata <- Biobase::pData(eset)
	
	# Columns to check for PN identifiers
	candidate_cols <- c("Sample_type", "Labels", "Sample_Group", 
											"sample_group", "SampleGroup", "Group", "Type")
	
	available_cols <- intersect(candidate_cols, colnames(metadata))
	
	options_list <- list()
	
	for (col in available_cols) {
		unique_vals <- unique(metadata[[col]])
		# Only include if not too many unique values (< 20)
		if (length(unique_vals) < 20) {
			options_list[[col]] <- unique_vals
		}
	}
	
	return(options_list)
}