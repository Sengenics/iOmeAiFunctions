#' NCF Correlation Calculation Function
#'
#' Computes Pearson correlation between all features in the input matrix and a specified ZZ reference feature (e.g., `"ZZ_con2"`),
#' returning a data table of correlation values and their quantiles.
#'
#' @param input_data A numeric matrix or data frame of features (rows = features, columns = samples).
#' @param zz Character string specifying the name of the reference feature (default is `"ZZ_con2"`).
#'
#' @return A list containing:
#' \item{df_ZZ}{A `data.table` with feature names and their correlation to the `zz` reference.}
#' \item{q_ZZ}{Quantiles of the correlation values.}
#' \item{zz}{The name of the reference feature used.}
#'
#' @note
#' Version 1.0 from  
#' NCF_function.R
#' @export
NCF_Corr <- function(input_data, zz = "ZZ_con2") { 
	# Calculate correlation of all features (rows) to the specified zz row
	ZZ_cors <- apply(input_data, 1, function(x) corr.test(x, as.matrix(input_data)[zz, ]))
	
	# Flatten and extract the ".r" correlation values
	ZZ_temp <- unlist(ZZ_cors)
	ZZ_r <- ZZ_temp[grep("\\.r", names(ZZ_temp))]
	ZZ_r <- unlist(ZZ_r)
	ZZ_r <- ZZ_r[-grep("ci", names(ZZ_r))]  # Remove confidence intervals if present
	
	# Format as data.table
	df_ZZ <- data.table(
		feature = gsub("\\.r", "", names(ZZ_r)),
		correlation = ZZ_r
	)
	
	# Compute quantiles
	q_ZZ <- quantile(ZZ_r)
	cat("Quantiles of correlation coefficients:\n")
	print(q_ZZ)
	
	# Return
	list(
		df_ZZ = df_ZZ,
		q_ZZ = q_ZZ,
		zz = zz
	)
}




#' NCF Threshold Filtering Function
#'
#' Filters features (typically antibody signals) based on their correlation with a reference signal (e.g., `ZZ_con2`)
#' using either a fixed correlation `threshold` or by selecting the bottom one-third if `percentage = TRUE`.
#'
#' @param input_data A data frame or matrix containing the original input feature data (with row names as feature IDs).
#' @param df_ZZ A data frame with at least two columns: `feature` and `correlation`, representing correlation of each feature with a reference.
#' @param threshold Numeric correlation cutoff (only used if `percentage = FALSE`).
#' @param percentage Logical; if `TRUE`, select the bottom one-third least-correlated features instead of applying a fixed threshold.
#'
#' @return A list containing:
#' \item{df_ZZ}{The filtered correlation table.}
#' \item{ncf_list}{A list with `keep` and `junk` feature vectors.}
#' \item{threshold}{The numeric threshold used (if applicable).}
#' \item{comment}{A summary string of the filtering result.}
#' \item{cut_count}{The number of features retained.}
#'
#' @note
#' Version 1.0 from  
#' NCF_function.R
#' @export
NCF_threshold <- function(input_data, df_ZZ, threshold, percentage = FALSE) {
	
	# Remove placeholder summary rows
	df_ZZ <- df_ZZ %>%
		filter(!feature %in% c('mean', 'mode', 'median'))
	
	# Determine total number of valid features from input_data
	total <- input_data %>%
		as.data.frame() %>%
		rownames_to_column('feature') %>%
		filter(!feature %in% c('mean', 'mode', 'median')) %>%
		pull(feature) %>%
		unique() %>%
		length()
	
	# Apply filter based on either percentage or fixed threshold
	if (percentage != FALSE) {
		cut_num <- round(total / 3, digits = 0)
		df_ZZ_keep <- df_ZZ %>%
			arrange(correlation) %>%
			slice(1:cut_num)
	} else {
		df_ZZ_keep <- df_ZZ %>%
			filter(correlation < threshold)
	}
	
	# Remove features matching 'ZZ_con'
	df_ZZ_keep <- df_ZZ_keep %>%
		filter(!feature %in% grep('ZZ_con', df_ZZ_keep$feature, value = TRUE))
	
	# Summary of kept features
	cut_count <- nrow(df_ZZ_keep)
	comment <- paste0(cut_count, " of ", total, " (", round(cut_count / total * 100, 2), "%) antibody signals will be retained")
	cat(comment, "\n")
	
	keep <- df_ZZ_keep$feature
	junk <- setdiff(df_ZZ$feature, keep)
	
	ncf_list <- list(
		keep = keep,
		junk = junk
	)
	
	output_list <- list(
		df_ZZ = df_ZZ,
		ncf_list = ncf_list,
		threshold = threshold,
		comment = comment,
		cut_count = cut_count
	)
	
	return(output_list)
}
