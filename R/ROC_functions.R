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
## ROC Mini Function ####
ROC_mini <- function(input, metadata, variable, groupPos, groupNeg, descriptor, folder = NULL) {
	
	tryCatch({
		# ✅ FIX: Validate inputs
		if (!variable %in% colnames(metadata)) {
			stop("Variable '", variable, "' not found in metadata. Available: ", paste(colnames(metadata), collapse = ", "))
		}
		
		# ✅ FIX: Ensure metadata variable is character/factor for comparison
		metadata[[variable]] <- as.character(metadata[[variable]])
		groupPos <- as.character(groupPos)
		groupNeg <- as.character(groupNeg)
		
		# Filter metadata to relevant groups
		meta_filtered <- metadata[metadata[[variable]] %in% c(groupPos, groupNeg), , drop = FALSE]
		
		if (nrow(meta_filtered) == 0) {
			warning("No samples found for groups: ", groupPos, ", ", groupNeg)
			return(NULL)
		}
		
		# Match expression data
		data_use <- input[, rownames(meta_filtered), drop = FALSE]
		
		if (ncol(data_use) == 0 || nrow(data_use) == 0) {
			warning("No matching expression data for ROC analysis")
			return(NULL)
		}
		
		# Create binary response (1 = groupPos, 0 = groupNeg)
		response <- ifelse(meta_filtered[[variable]] == groupPos, 1, 0)
		
		# Validate we have both groups
		if (length(unique(response)) < 2) {
			warning("Only one group found in filtered data")
			return(NULL)
		}
		
		# Calculate ROC for each feature
		roc_results <- lapply(rownames(data_use), function(feature) {
			feature_data <- as.numeric(data_use[feature, ])
			
			# Skip if all values are the same
			if (length(unique(feature_data)) == 1) {
				return(data.frame(
					Protein = feature,
					AUC = NA,
					Sensitivity = NA,
					Specificity = NA,
					Optimal.Cutoff = NA,
					stringsAsFactors = FALSE
				))
			}
			
			# Calculate ROC
			roc_obj <- tryCatch({
				pROC::roc(response, feature_data, quiet = TRUE)
			}, error = function(e) {
				warning("ROC failed for feature: ", feature, " - ", e$message)
				return(NULL)
			})
			
			if (is.null(roc_obj)) {
				return(data.frame(
					Protein = feature,
					AUC = NA,
					Sensitivity = NA,
					Specificity = NA,
					Optimal.Cutoff = NA,
					stringsAsFactors = FALSE
				))
			}
			
			# Find optimal cutoff (Youden's index)
			coords <- tryCatch({
				pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), 
										 best.method = "youden", quiet = TRUE)
			}, error = function(e) {
				list(threshold = NA, sensitivity = NA, specificity = NA)
			})
			
			data.frame(
				Protein = feature,
				AUC = as.numeric(pROC::auc(roc_obj)),
				Sensitivity = coords$sensitivity,
				Specificity = coords$specificity,
				Optimal.Cutoff = coords$threshold,
				stringsAsFactors = FALSE
			)
		})
		
		roc_df <- do.call(rbind, roc_results)
		rownames(roc_df) <- roc_df$Protein
		
		# Save if folder provided
		if (!is.null(folder)) {
			write.csv(roc_df, file.path(folder, paste0("ROC_results_", descriptor, ".csv")))
		}
		
		return(roc_df)
		
	}, error = function(e) {
		warning("Error in ROC_mini: ", e$message)
		print(e)
		return(NULL)
	})
}