#' ROC Analysis Mini Function
#'
#' @param input Matrix of expression data
#' @param metadata Data frame with sample metadata
#' @param variable Character, column name in metadata for grouping
#' @param groupPos Character, positive group name
#' @param groupNeg Character, negative group name
#' @param descriptor Character, descriptor for output naming
#' @param folder Character, folder path for output (NULL for no file output)
#'
#' @return Data frame with ROC results
#' @export
ROC_mini <- function(input, metadata, variable, groupPos, groupNeg, descriptor, folder = NULL) {
	
	# Debug output
	cat("\n=== ROC_mini Debug ===\n")
	cat("Variable:", variable, "\n")
	cat("Metadata columns:", paste(colnames(metadata), collapse = ", "), "\n")
	cat("Metadata nrow:", nrow(metadata), "\n")
	cat("groupPos:", groupPos, "\n")
	cat("groupNeg:", groupNeg, "\n")
	cat("Variable exists:", variable %in% colnames(metadata), "\n")
	
	# Check if variable exists
	if (!variable %in% colnames(metadata)) {
		stop("Variable '", variable, "' not found in metadata. Available columns: ", 
				 paste(colnames(metadata), collapse = ", "))
	}
	
	# Get the variable column
	group_values <- metadata[[variable]]
	cat("Unique group values:", paste(unique(group_values), collapse = ", "), "\n")
	cat("=====================\n\n")
	
	# Filter metadata to include only the two groups
	keep_rows <- group_values %in% c(groupPos, groupNeg)
	
	if (sum(keep_rows) == 0) {
		stop("No samples found matching groupPos='", groupPos, "' or groupNeg='", groupNeg, "'")
	}
	
	meta_filtered <- metadata[keep_rows, , drop = FALSE]
	data_filtered <- input[, rownames(meta_filtered), drop = FALSE]
	
	# Verify variable still exists after filtering
	if (!variable %in% colnames(meta_filtered)) {
		stop("Variable '", variable, "' lost after filtering")
	}
	
	# Get filtered group values
	filtered_group_values <- meta_filtered[[variable]]
	
	# Create binary response variable
	response <- ifelse(filtered_group_values == groupPos, 1, 0)
	
	cat("ROC analysis: ", sum(response == 1), " positive samples, ", 
			sum(response == 0), " negative samples\n")
	
	# Run ROC for each feature
	roc_results <- lapply(rownames(data_filtered), function(feature) {
		
		predictor <- as.numeric(data_filtered[feature, ])
		
		tryCatch({
			roc_obj <- pROC::roc(response, predictor, quiet = TRUE)
			
			# Find optimal cutoff (Youden's index)
			coords <- pROC::coords(roc_obj, "best", best.method = "youden")
			
			data.frame(
				Protein = feature,
				AUC = as.numeric(roc_obj$auc),
				Sensitivity = coords$sensitivity,
				Specificity = coords$specificity,
				Optimal.Cutoff = coords$threshold,
				stringsAsFactors = FALSE
			)
		}, error = function(e) {
			warning("ROC failed for feature '", feature, "': ", e$message)
			data.frame(
				Protein = feature,
				AUC = NA,
				Sensitivity = NA,
				Specificity = NA,
				Optimal.Cutoff = NA,
				stringsAsFactors = FALSE
			)
		})
	})
	
	roc_df <- do.call(rbind, roc_results)
	rownames(roc_df) <- roc_df$Protein
	
	# Write to file if folder is specified
	if (!is.null(folder)) {
		dir.create(folder, showWarnings = FALSE, recursive = TRUE)
		write.csv(roc_df, file.path(folder, paste0("ROC_results_", descriptor, ".csv")), row.names = FALSE)
	}
	
	return(roc_df)
}