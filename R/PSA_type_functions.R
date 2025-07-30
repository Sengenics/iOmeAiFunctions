#' PSA Type 2 Shiny Function
#'
#' Computes PSA scores and classifications using a fold-change threshold and a baseline reference
#' method (either median or mode). It organizes results for downstream visualization and analysis.
#'
#' @param PSA_ROs Character vector of region-of-interest identifiers for PSA analysis.
#' @param df Data frame or matrix of expression values (rows = features, columns = samples).
#' @param metadata Data frame containing metadata (not used directly in this function).
#' @param FC_threshold Numeric value; log2 fold-change threshold for classifying features as PSA.
#' @param PSA_threshold Numeric value; percentage threshold to determine PSA positivity.
#' @param method Character string; either 'median' (default) or a custom function named `mode_function` to compute baseline.
#'
#' @return A list with:
#' \item{PSA_ants_match}{Character vector of matched PSA identifiers found in `df`}
#' \item{avg_array}{Named numeric vector of baseline values (median or mode) per feature}
#' \item{PSA_ratios}{Matrix of log2 differences from baseline for matched PSA identifiers}
#' \item{PSA_score_df}{Data frame of PSA scores, counts, classification, and column order for plotting}
#'
#' @note
#' Version 2.0 from  
#' PSA_type_functions.R
#' @export
PSA_type_2_shiny <- function(PSA_ROs, df, metadata,
														 FC_threshold,
														 PSA_threshold,
														 method = 'median') {
	
	# Match PSA regions of interest with available data rows
	PSA_ants_match <- intersect(PSA_ROs, rownames(df))
	
	# Compute baseline values (either median or mode)
	if (method == 'median') {
		avg_array <- apply(df, 1, median)
	} else {
		avg_array <- apply(df, 1, mode_function)
	}
	
	# Calculate log2 difference of each matched PSA feature from the baseline
	PSA_ratios <- apply(df[PSA_ants_match, ], 2, function(x) x - avg_array[PSA_ants_match])
	
	# Count number of PSA features above threshold per sample
	PSA_number <- apply(PSA_ratios, 2, function(x) length(which(x > log2(FC_threshold))))
	
	# Compute percentage score per sample
	PSA_score <- apply(PSA_ratios, 2, function(x) length(which(x > log2(FC_threshold))) / nrow(PSA_ratios) * 100)
	
	# Create score data frame with classification
	PSA_score_df <- data.frame(
		Sample = names(PSA_score),
		PSA_number = PSA_number,
		PSA_score = PSA_score
	) %>%
		mutate(PSA_class = ifelse(PSA_score > PSA_threshold, "pos", "neg"))
	
	# Set factor levels for class (ensures consistent plotting order)
	PSA_score_df$PSA_class <- factor(PSA_score_df$PSA_class, levels = c('neg', 'pos'))
	
	# Sort samples by class for visualization
	PSA_sort <- PSA_score_df %>% arrange(PSA_class)
	data_plot <- df[PSA_ants_match, PSA_sort$Sample]
	
	# Reorder columns within each class using hierarchical clustering
	for (j in seq_along(unique(PSA_sort$PSA_class))) {
		cols <- which(PSA_sort$PSA_class == unique(PSA_sort$PSA_class)[j])
		ns <- data_plot[, cols, drop = FALSE]
		ns[is.na(ns)] <- 0
		
		if (length(cols) > 1) {
			ns_clust <- hclust(vegdist(t(ns), distance = "correlation"))
			order <- ns_clust$labels[ns_clust$order]
		} else {
			order <- colnames(ns)
		}
		
		if (j == 1) {
			toplot <- ns[, order]
		} else {
			toplot <- cbind(toplot, ns[, order])
		}
	}
	
	# Record plotting order of samples
	PSA_order_df <- data.frame(
		Sample = colnames(toplot),
		PSA_order = seq_len(ncol(toplot))
	)
	
	# Merge plotting order with PSA score data
	PSA_score_df <- PSA_score_df %>%
		left_join(PSA_order_df, by = "Sample")
	
	# Return results
	list(
		PSA_ants_match = PSA_ants_match,
		avg_array = avg_array,
		PSA_ratios = PSA_ratios,
		PSA_score_df = PSA_score_df
	)
}
