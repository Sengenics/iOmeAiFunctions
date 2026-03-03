#' Remove Principal Components from Expression Data (Denoising Step 1)
#'
#' This function performs PCA on the expression data and removes the first n_PCs 
#' principal components to denoise the data by removing technical noise.
#'
#' @param eset ExpressionSet object with expression data
#' @param assay_name Character; name of assay data to use (default = NULL uses exprs())
#' @param n_PCs Integer; number of principal components to remove (default = 1)
#' @param scale Logical; whether to scale data during PCA (default = TRUE)
#'
#' @return List containing:
#'   - \code{denoised_data}: List of denoised data frames for 1:n_PCs removed
#'   - \code{pca_result}: Full PCA result object
#'   - \code{variance_explained}: Vector of variance explained by each PC (%)
#'   - \code{n_PCs_removed}: Vector of PCs removed in each iteration
#'
#' @import Biobase
#'
#' @examples
#' denoised <- denoise_remove_PCs(eset, n_PCs = 3)
#' # Access denoised data with 2 PCs removed:
#' denoised_2PC <- denoised$denoised_data[[2]]
#'
#' @export
denoise_remove_PCs <- function(eset, assay_name = NULL, n_PCs = 1, scale = TRUE, center = TRUE, transpose = FALSE) {
	
	# Extract expression data from ExpressionSet
	if (inherits(eset, "ExpressionSet")) {
		if (is.null(assay_name)) {
			expr_data <- Biobase::exprs(eset)
		} else {
			expr_data <- Biobase::assayDataElement(eset, assay_name)
		}
	} else {
		stop("eset must be an ExpressionSet object. Use create_denoise_eset() to create one.")
	}
	
	# Validate expression data
	if (!is.matrix(expr_data) && !is.data.frame(expr_data)) {
		stop("Expression data must be a matrix or data.frame")
	}
	
	# Step 1: Perform PCA
	
	if(transpose == T){
		expr_data = t(expr_data)
	}
	pca_result <- prcomp(expr_data, scale. = scale, center = center)
	
	# Step 2: Calculate variance explained by each PC
	total_variance <- sum(pca_result$sdev^2)
	variance_contribution <- round(pca_result$sdev^2 / total_variance * 100, digits = 2)
	
	message("Variance explained by PCs:")
	message(paste(head(variance_contribution, 10), collapse = ", "))
	
	# Step 3: Iteratively remove PCs and reconstruct data
	n_PCs_vec <- seq(1:n_PCs)
	df_list <- list()
	
	for (i in 1:n_PCs) {
		# Remove first i PCs
		cleaned_principal_components <- pca_result$x[, -n_PCs_vec[1:i], drop = FALSE]
		
		# Reconstruct denoised data
		denoised_data <- cleaned_principal_components %*% 
			t(pca_result$rotation[, -n_PCs_vec[1:i], drop = FALSE])
		
		# Convert to data frame
		denoised_data <- as.data.frame(denoised_data)
		
		df_list[[i]] <- denoised_data
	}
	
	# # Remove PCs and reconstruct
	# denoised_data <- list()
	# for (i in 1:n_PCs) {
	# 	# ... reconstruction code ...
	# 	denoised_data[[i]] <- reconstructed_matrix
	# }
	
	# Return results
	return(list(
		#denoised_data = denoised_data,
		denoised_data = df_list,
		pca_result = pca_result,
		variance_explained = variance_contribution,
		n_PCs_removed = n_PCs_vec
	))
}


#' Create ExpressionSet for Denoising
#'
#' Helper function to create an ExpressionSet from matrix and metadata
#'
#' @param expr_data Matrix or data.frame; expression data with features as rows, samples as columns
#' @param metadata Data.frame; sample metadata with rownames matching colnames of expr_data
#'
#' @return ExpressionSet object
#' @import Biobase
#'
#' @examples
#' eset <- create_denoise_eset(NetI, meta)
#'
#' @export
create_denoise_eset <- function(expr_data, metadata) {
	
	# Validate inputs
	if (!is.matrix(expr_data) && !is.data.frame(expr_data)) {
		stop("expr_data must be a matrix or data.frame")
	}
	
	if (!is.data.frame(metadata)) {
		stop("metadata must be a data.frame")
	}
	
	# Ensure sample names match
	if (!identical(colnames(expr_data), rownames(metadata))) {
		warning("Reordering metadata to match expr_data columns")
		metadata <- metadata[colnames(expr_data), , drop = FALSE]
	}
	
	# Create AnnotatedDataFrame for phenotype data
	pheno_data <- Biobase::AnnotatedDataFrame(data = metadata)
	
	# Create ExpressionSet
	eset <- Biobase::ExpressionSet(
		assayData = as.matrix(expr_data),
		phenoData = pheno_data
	)
	
	return(eset)
}


#' Calculate Mode (Most Frequent Value)
#'
#' Helper function to calculate the mode of a numeric vector
#'
#' @param x Numeric vector
#' @return Numeric; the mode value
#' @keywords internal
mode_function <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}


#' Calculate Average Absolute Deviation
#'
#' Helper function to calculate AAD from a specified center
#'
#' @param x Numeric vector
#' @param center Numeric; center value (default is mode)
#' @return Numeric; average absolute deviation
#' @keywords internal
aad_calc <- function(x, center = mode_function(x)) {
	mean(abs(x - center))
}


#' Apply Cutpoint to Denoised Data
#'
#' This function applies a cutpoint threshold to denoised data, setting values
#' below the threshold to zero. Supports three methods: singular cutpoint,
#' MAD-based (Median Absolute Deviation), or AAD-based (Average Absolute Deviation).
#'
#' @param denoised_data Data frame; denoised expression data
#' @param cutpoint Numeric; threshold value(s) to apply
#' @param method Character; one of "singular", "MAD", or "AAD" (default = "singular")
#'
#' @return Data frame with values below threshold set to zero
#' @export
#'
#' @examples
#' cut_data <- denoise_apply_cutpoint(denoised_data, cutpoint = 0.5)
#'
denoise_apply_cutpoint <- function(denoised_data, cutpoint, method = "singular") {
	
	if (method == "singular") {
		# Simple threshold: set all values <= cutpoint to 0
		cut_data <- denoised_data
		cut_data[cut_data <= cutpoint] <- 0
		
	} else if (method == "MAD") {
		# Median Absolute Deviation-based threshold
		antigen_medians <- apply(denoised_data, 1, median)
		antigen_MADs <- apply(denoised_data, 1, mad)
		antigen_thresholds <- antigen_medians + antigen_MADs * cutpoint
		
		# Apply per-antigen thresholds
		exceeds_threshold <- sweep(denoised_data, 1, antigen_thresholds, ">")
		cut_data <- denoised_data
		cut_data[!exceeds_threshold] <- 0
		
	} else if (method == "AAD") {
		# Average Absolute Deviation-based threshold
		antigen_modes <- apply(denoised_data, 1, mode_function)
		antigen_AADs <- apply(denoised_data, 1, aad_calc)
		antigen_thresholds <- antigen_modes + antigen_AADs * cutpoint
		
		# Apply per-antigen thresholds
		exceeds_threshold <- sweep(denoised_data, 1, antigen_thresholds, ">")
		cut_data <- denoised_data
		cut_data[!exceeds_threshold] <- 0
		
	} else {
		stop("Method must be one of: 'singular', 'MAD', or 'AAD'")
	}
	
	# Remove antigens with all zeros
	zero_rows <- rowSums(cut_data) == 0
	if (any(zero_rows)) {
		message(paste(sum(zero_rows), "antigens removed (all zeros after cutting)"))
		cut_data <- cut_data[!zero_rows, , drop = FALSE]
	}
	
	return(cut_data)
}


#' Calculate Denoiser Metrics
#'
#' This function calculates quality metrics for a given cutpoint on denoised data,
#' including PN AAb ratios, ZZ control positivity rates, and TP:FP ratios.
#'
#' @param cut_data Data frame; AAb-called data (after applying cutpoint)
#' @param eset ExpressionSet; original ExpressionSet with metadata
#' @param PN_column Character; column name in pData(eset) identifying PN samples (default = "Sample_Group")
#' @param PN_value Character; value in PN_column identifying PN samples (default = "Pooled Normal")
#' @param PN_AAbs Character vector; expected AAb names in PNs (from limma)
#' @param cutpoint Numeric; the cutpoint value used
#'
#' @return Named list with metrics:
#'   - \code{cutpoint}: The cutpoint value
#'   - \code{PN_Aab_count_67_perc}: Number of AAbs present in >=67% of PNs
#'   - \code{PN_AAb_hit_rate}: Percentage of expected PN AAbs detected
#'   - \code{TP_FP_ratio}: True positive to false positive ratio
#'   - \code{zz_2_frac}: Fraction of samples positive for ZZ_con2
#'   - \code{zz_4_frac}: Fraction of samples positive for ZZ_con4
#'   - \code{N_unique_AAbs}: Number of unique AAbs detected
#'   - \code{sample_AAb_median}: Median number of AAbs per sample
#'
#' @import Biobase
#' @export
#'
#' @examples
#' metrics <- denoise_calculate_metrics(cut_data, eset, PN_AAbs = expected_AAbs, cutpoint = 0.5)
#'
denoise_calculate_metrics <- function(cut_data, eset, 
																			PN_column = "Sample_Group",
																			PN_value = "Pooled Normal",
																			PN_AAbs = NULL, 
																			cutpoint) {
	
	# Extract metadata
	metadata <- Biobase::pData(eset)
	
	# Identify PN samples
	if (!PN_column %in% colnames(metadata)) {
		stop(paste("Column", PN_column, "not found in pData(eset)"))
	}
	
	PN_samples <- rownames(metadata)[metadata[[PN_column]] == PN_value]
	
	# Validate that samples match
	if (!all(colnames(cut_data) %in% rownames(metadata))) {
		stop("Not all samples in cut_data are present in metadata")
	}
	
	# Extract PN data
	PN_cols <- intersect(PN_samples, colnames(cut_data))
	if (length(PN_cols) == 0) {
		warning("No PN samples found in cut_data")
		return(NULL)
	}
	
	dns_PN <- cut_data[, PN_cols, drop = FALSE]
	
	# Calculate which antigens are positive in each PN sample
	PN_lists <- apply(dns_PN, 2, function(x) names(which(x > 0)))
	
	# Count frequency of each antigen across PNs
	all_antigens <- unlist(PN_lists)
	if (length(all_antigens) == 0) {
		# No AAbs detected in PNs
		return(list(
			cutpoint = cutpoint,
			PN_Aab_count_67_perc = 0,
			PN_AAb_hit_rate = 0,
			TP_FP_ratio = 0,
			zz_2_frac = if("ZZ_con2" %in% rownames(cut_data)) sum(cut_data["ZZ_con2", ] > 0) / ncol(cut_data) else NA,
			zz_4_frac = if("ZZ_con4" %in% rownames(cut_data)) sum(cut_data["ZZ_con4", ] > 0) / ncol(cut_data) else NA,
			N_unique_AAbs = sum(rowSums(cut_data) > 0),
			sample_AAb_median = median(colSums(cut_data > 0))
		))
	}
	
	count_df <- as.data.frame(table(all_antigens))
	colnames(count_df) <- c("antigen", "count")
	count_df$PN_frac <- count_df$count / length(PN_cols)
	
	# Antigens present in >= 67% of PNs
	legit <- count_df[count_df$PN_frac >= 2/3, ]
	
	# Calculate PN AAb hit rate
	if (!is.null(PN_AAbs) && length(PN_AAbs) > 0) {
		PN_match_perc <- round(
			length(intersect(legit$antigen, PN_AAbs)) / length(PN_AAbs) * 100,
			digits = 2
		)
	} else {
		PN_match_perc <- NA
	}
	
	# Calculate TP:FP ratio
	TPs <- sum(legit$count)
	FPs <- sum(count_df$count) - TPs
	TP_FP_ratio <- if (FPs > 0) round(TPs / FPs, digits = 2) else Inf
	
	# Calculate ZZ control fractions
	zz_2_frac <- if ("ZZ_con2" %in% rownames(cut_data)) {
		round(sum(cut_data["ZZ_con2", ] > 0) / ncol(cut_data), digits = 2)
	} else {
		NA
	}
	
	zz_4_frac <- if ("ZZ_con4" %in% rownames(cut_data)) {
		round(sum(cut_data["ZZ_con4", ] > 0) / ncol(cut_data), digits = 2)
	} else {
		NA
	}
	
	# Calculate unique AAbs and per-sample AAb counts
	N_unique_AAbs <- sum(rowSums(cut_data) > 0)
	sample_AAb_median <- median(colSums(cut_data > 0))
	
	return(list(
		cutpoint = cutpoint,
		PN_Aab_count_67_perc = nrow(legit),
		PN_AAb_hit_rate = PN_match_perc,
		TP_FP_ratio = TP_FP_ratio,
		zz_2_frac = zz_2_frac,
		zz_4_frac = zz_4_frac,
		N_unique_AAbs = N_unique_AAbs,
		sample_AAb_median = sample_AAb_median
	))
}


#' Find Optimal Cutpoints
#'
#' This function tests a sequence of cutpoints on denoised data and calculates
#' metrics for each to help identify the optimal threshold.
#'
#' @param denoised_data Data frame; denoised expression data
#' @param eset ExpressionSet; original ExpressionSet with metadata
#' @param PN_column Character; column name identifying PN samples
#' @param PN_value Character; value identifying PN samples
#' @param PN_AAbs Character vector; expected PN AAbs
#' @param cut_seq Numeric vector; sequence of cutpoints to test (default: seq(0.4, 3, 0.1))
#' @param method Character; cutting method - "singular", "MAD", or "AAD"
#'
#' @return Data frame with one row per cutpoint and columns for each metric
#' @export
#'
#' @examples
#' cutpoint_results <- denoise_find_cutpoints(
#'   denoised_data, eset, PN_AAbs = expected_AAbs
#' )
#'
denoise_find_cutpoints <- function(denoised_data, eset, 
																	 PN_column = "Sample_Group",
																	 PN_value = "Pooled Normal",
																	 PN_AAbs = NULL, 
																	 cut_seq = seq(0.4, 3, 0.1),
																	 method = "singular") {
	
	results_list <- list()
	
	for (i in seq_along(cut_seq)) {
		# Apply cutpoint
		cut_data <- denoise_apply_cutpoint(denoised_data, cut_seq[i], method = method)
		
		# Calculate metrics
		metrics <- denoise_calculate_metrics(
			cut_data, eset, PN_column, PN_value, PN_AAbs, cut_seq[i]
		)
		
		results_list[[i]] <- metrics
	}
	
	# Convert to data frame
	results_df <- do.call(rbind, lapply(results_list, as.data.frame))
	
	return(results_df)
}


#' Select Optimal Cutpoint
#'
#' This function selects the optimal cutpoint based on criteria:
#' 1. PSA+ rate <= 5% (if PSA_results provided)
#' 2. PN AAb count within expected range
#' 3. Minimize PCs removed
#' 4. Maximize TP:FP ratio
#'
#' @param cutpoint_results Data frame; output from denoise_find_cutpoints()
#' @param PSA_results Data frame; PSA typing results (optional)
#' @param exp_PN_AAbs Numeric vector; expected range of PN AAbs (e.g., 6:12)
#' @param PCs_removed Integer; number of PCs removed for this analysis
#'
#' @return Data frame with single row of optimal cutpoint parameters
#' @export
#'
denoise_select_optimal_cutpoint <- function(cutpoint_results, 
																						PSA_results = NULL,
																						exp_PN_AAbs = 6:12,
																						PCs_removed = NULL) {
	
	# Start with all results
	optimal <- cutpoint_results
	
	# Filter by expected PN AAb count
	optimal <- optimal[optimal$PN_Aab_count_67_perc %in% exp_PN_AAbs, ]
	
	if (nrow(optimal) == 0) {
		warning("No cutpoints meet PN AAb count criteria. Returning best TP:FP ratio.")
		optimal <- cutpoint_results[which.max(cutpoint_results$TP_FP_ratio), ]
		return(optimal)
	}
	
	# Filter by PSA rate if available
	if (!is.null(PSA_results)) {
		optimal <- merge(optimal, PSA_results, by = "cutpoint")
		optimal <- optimal[optimal$PSA_pos_perc <= 5, ]
		
		if (nrow(optimal) == 0) {
			warning("No cutpoints meet PSA criteria. Relaxing PSA constraint.")
			optimal <- cutpoint_results[cutpoint_results$PN_Aab_count_67_perc %in% exp_PN_AAbs, ]
		}
	}
	
	# Select cutpoint with maximum TP:FP ratio
	optimal <- optimal[which.max(optimal$TP_FP_ratio), ]
	
	# Add PCs_removed if provided
	if (!is.null(PCs_removed)) {
		optimal$PCs_removed <- PCs_removed
	}
	
	return(optimal)
}


#' Label Candidate Cutpoints per PC Level
#'
#' Identifies the four candidate cutpoints (a, b, c, d) for each PC removal
#' level in the cutpoint results table, mirroring the logic in cut_and_drying().
#'
#' Candidate definitions:
#' - b: one step below the first cutpoint where BOTH zz_2_frac AND zz_4_frac == 0
#' - a: one step below b
#' - c: one step above b
#' - d: cutpoint with highest TP:FP ratio where PN_Aab_count_67_perc is in
#'      exp_PN_AAbs AND PN_AAb_hit_rate >= 65. Falls back to max TP:FP overall
#'      if no candidates meet criteria.
#'
#' @param cutpoint_results Data frame; output from denoise_find_cutpoints() with
#'   PCs_removed column added. Must contain columns: cutpoint, zz_2_frac,
#'   zz_4_frac, TP_FP_ratio, PN_Aab_count_67_perc, PN_AAb_hit_rate, PCs_removed
#' @param exp_PN_AAbs Numeric vector; expected range of PN AAb counts (e.g. 4:8)
#' @param cut_step Numeric; increment used in cut_seq (e.g. 0.1)
#'
#' @return Data frame with labelled candidate rows and a `label` column in the
#'   format "PC1_a", "PC1_2_b", "PC1to4_d" etc. Returns NULL if no valid
#'   candidates found.
#'
#' @export
#'
#' @note Original logic from cut_and_drying() in Sengenics_accessory_functions.R
#'
#' @examples
#' \dontrun{
#' s_df <- label_candidate_cutpoints(
#'   cutpoint_results = rv$cutpoint_results,
#'   exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max),
#'   cut_step = input$cut_step
#' )
#' }
label_candidate_cutpoints <- function(cutpoint_results, exp_PN_AAbs, cut_step) {
	
	# Input checks
	required_cols <- c("cutpoint", "zz_2_frac", "zz_4_frac", 
										 "TP_FP_ratio", "PN_Aab_count_67_perc", 
										 "PN_AAb_hit_rate", "PCs_removed")
	
	missing_cols <- setdiff(required_cols, colnames(cutpoint_results))
	if (length(missing_cols) > 0) {
		stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
	}
	
	pc_levels <- sort(unique(cutpoint_results$PCs_removed))
	labelled_list <- list()
	
	for (pc in pc_levels) {
		
		df <- cutpoint_results[cutpoint_results$PCs_removed == pc, ]
		
		# --- Build PC label (matches original rowname convention) ---
		pc_label <- switch(as.character(pc),
											 "1" = "PC1",
											 "2" = "PC1_2",
											 "3" = "PC1_2_3",
											 paste0("PC1to", pc)  # PC1to4, PC1to5 etc.
		)
		
		# --- Find b: one step below first point where BOTH zz fracs == 0 ---
		zz_zero <- df[df$zz_2_frac == 0 & df$zz_4_frac == 0, ]
		
		if (nrow(zz_zero) == 0) {
			warning(paste("No zero ZZ point found for PC level", pc, "- skipping"))
			next
		}
		
		b <- zz_zero$cutpoint[1] - cut_step
		a <- b - cut_step
		c_ <- b + cut_step
		
		# --- Find d: max TP:FP in expected PN AAb range with hit rate >= 65 ---
		d_candidates <- df[df$PN_Aab_count_67_perc %in% exp_PN_AAbs &
											 	df$PN_AAb_hit_rate >= 65, ]
		
		if (nrow(d_candidates) == 0) {
			# Fallback 1: relax hit rate filter, keep PN AAb range
			d_candidates <- df[df$PN_Aab_count_67_perc %in% exp_PN_AAbs, ]
		}
		
		if (nrow(d_candidates) == 0) {
			# Fallback 2: just take max TP:FP across all cutpoints
			warning(paste("No d candidates meet PN AAb criteria for PC level", pc,
										"- using global max TP:FP"))
			d_candidates <- df
		}
		
		d <- d_candidates$cutpoint[which.max(d_candidates$TP_FP_ratio)]
		
		# --- Pull matching rows using factor matching to avoid floating point issues ---
		target_cutpoints <- c(a, b, c_, d)
		
		df$cutpoint_f   <- as.factor(round(df$cutpoint, 8))
		target_f        <- as.factor(round(target_cutpoints, 8))
		
		candidates <- df[df$cutpoint_f %in% target_f, ]
		
		if (nrow(candidates) == 0) {
			warning(paste("No candidate rows found for PC level", pc,
										"- candidates may fall outside cut_seq range"))
			next
		}
		
		# --- Assign a/b/c/d labels ---
		candidates$label <- paste0(
			pc_label, "_",
			c("a", "b", "c", "d")[match(
				as.character(candidates$cutpoint_f),
				as.character(target_f)
			)]
		)
		
		# Clean up helper column
		candidates$cutpoint_f <- NULL
		
		labelled_list[[as.character(pc)]] <- candidates
	}
	
	if (length(labelled_list) == 0) {
		warning("No labelled candidates produced - check zz_2_frac/zz_4_frac columns")
		return(NULL)
	}
	
	result <- do.call(rbind, labelled_list)
	rownames(result) <- result$label
	
	return(result)
}