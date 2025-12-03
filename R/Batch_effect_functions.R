#' Apply ComBat Batch Correction to ExpressionSet
#'
#' Applies the ComBat algorithm (from the `sva` package) to correct for batch effects in an `ExpressionSet`.
#' Batch information is constructed by collapsing multiple metadata columns into a single batch factor.
#'
#' @param ExpSet An `ExpressionSet` object.
#' @param ComBat_columns Character vector of column names in `pData(ExpSet)` to define batch groups.
#'
#' @return A list containing:
#' \item{m}{The batch-corrected expression matrix.}
#' \item{meta}{The updated metadata including the `ComBat` batch variable.}
#'
#' @note
#' Version 1.0 from  
#' Batch_effect_functions.R
#' @importFrom sva ComBat
#' @export
ComBat_function <- function(meta,m,ComBat_columns) {
	
	ComBat_columns = ComBat_columns[ComBat_columns %in% colnames(meta)]
	# Create batch identifier by collapsing selected metadata columns
	meta$ComBat <- apply(meta[, ComBat_columns, drop = FALSE], 1, paste0, collapse = "_")
	meta$ComBat <- as.factor(as.character(meta$ComBat))
	
	# Null model (no covariates)
	modcombat <- model.matrix(~1, data = meta)
	
	# Apply ComBat
	loess_combat <- ComBat(m, batch = meta$ComBat, mod = modcombat)
	
	# Return corrected matrix and updated metadata
	list(
		m = loess_combat,
		meta = meta
	)
}

#' Run Tidy ANOVA for Multiple Batch Annotations
#'
#' Performs ANOVA tests across multiple phenoData annotations to identify
#' batch effects in expression data.  Returns a tidy data frame with test
#' statistics for each annotation.
#'
#' @param m Numeric matrix.   Expression data with features in rows and samples
#'   in columns.  Typically from `Biobase::exprs(ExpressionSet)`.
#' @param meta Data frame. Sample metadata with annotations in columns and
#'   samples in rows.  Typically from `Biobase::pData(ExpressionSet)`. 
#' @param annotations Character vector.  Column names from `meta` to test for
#'   batch effects.  Each annotation is tested independently.
#'
#' @return Data frame with ANOVA results for each annotation.   Columns include:
#'   \describe{
#'     \item{Annotation}{Name of the tested annotation column}
#'     \item{df}{Degrees of freedom}
#'     \item{sumsq}{Sum of squares}
#'     \item{meansq}{Mean sum of squares}
#'     \item{statistic}{F-statistic}
#'     \item{p.value}{P-value from ANOVA test}
#'   }
#'   All numeric values are rounded to 3 significant figures.
#'   Returns empty data frame if computation fails.
#'
#' @details
#' This function wraps `anova_betadine_function()` and applies it to multiple
#' annotations. It uses `broom::tidy()` to convert ANOVA results to a tidy
#' format and filters for the "Groups" term.  
#'
#' Error handling: If ANOVA computation fails for any annotation, a warning
#' message is printed with the annotation name, and that annotation is skipped. 
#' If all annotations fail, an empty data frame is returned.
#'
#' @seealso \code{\link{anova_betadine_function}} for single annotation ANOVA
#'
#' @importFrom purrr map_dfr
#' @importFrom broom tidy
#' @importFrom dplyr filter rename mutate across where
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using ExpressionSet
#' library(Biobase)
#' data(sample_ExpressionSet)
#' 
#' m <- exprs(sample_ExpressionSet)
#' meta <- pData(sample_ExpressionSet)
#' 
#' # Test multiple annotations
#' annotations <- c("sex", "type", "score")
#' results <- anova_betadine_tidy_function(m, meta, annotations)
#' 
#' # View significant batch effects
#' results %>%
#'   filter(p.value < 0.05) %>%
#'   arrange(p.value)
#' }
#'
#' @note Original location: i-Ome-AI batch correction workflow
anova_betadine_tidy_function <- function(m, meta, annotations) {
	
	# Validate inputs
	if (length(annotations) == 0) {
		warning("No annotations provided")
		return(data.frame())
	}
	
	if (! all(annotations %in% colnames(meta))) {
		missing <- setdiff(annotations, colnames(meta))
		warning("Annotations not found in metadata: ", paste(missing, collapse = ", "))
		annotations <- intersect(annotations, colnames(meta))
		if (length(annotations) == 0) {
			return(data.frame())
		}
	}
	
	# Process each annotation with individual error handling
	results_list <- list()
	
	for (annotation in annotations) {
		result <- tryCatch(
			{
				# Check if column has valid groupings
				groups <- meta[[annotation]]
				
				# Remove NA values
				valid_idx <- !  is.na(groups)
				if (sum(valid_idx) < nrow(meta)) {
					message(sprintf("Note: Annotation '%s' has %d NA values (removed)", 
													annotation, sum(! valid_idx)))
				}
				
				# Check number of groups
				n_groups <- length(unique(groups[valid_idx]))
				if (n_groups < 2) {
					warning(sprintf("Annotation '%s' has < 2 groups (skipped)", annotation))
					NULL
				} else {
					# Run ANOVA
					anova_result <- anova_betadine_function(m, meta, annotation)
					
					# Tidy results
					tidy_result <- tidy(anova_result) %>%
						filter(term == "Groups") %>%
						rename(Annotation = term) %>%
						mutate(Annotation = annotation) %>% 
						mutate(across(where(is.numeric), ~ signif(.x, 3)))
					
					tidy_result
				}
			},
			error = function(e) {
				warning(sprintf("ANOVA failed for annotation '%s': %s", annotation, e$message))
				NULL
			}
		)
		
		if (!is.null(result)) {
			results_list[[annotation]] <- result
		}
	}
	
	# Combine results
	if (length(results_list) == 0) {
		warning("All ANOVA computations failed")
		return(data.frame())
	}
	
	anova_df <- bind_rows(results_list)
	
	return(anova_df)
}

#' ANOVA Test for Batch Effects Using Beta Dispersion
#'
#' Performs ANOVA on beta dispersion to test whether group dispersions differ
#' significantly, indicating potential batch effects in expression data.
#'
#' @param m Numeric matrix.  Expression data with features in rows and samples
#'   in columns.  Typically from `Biobase::exprs(ExpressionSet)`. 
#' @param meta Data frame. Sample metadata with annotations in columns and
#'   samples in rows.  Row names must match column names of `m`.
#'   Typically from `Biobase::pData(ExpressionSet)`.
#' @param col_name Character string. Column name in `meta` defining groups
#'   to test for batch effects (e.g., "Batch", "Sample_Group").
#'
#' @return ANOVA table (class `anova`) with the following components:
#'   \describe{
#'     \item{Df}{Degrees of freedom}
#'     \item{Sum Sq}{Sum of squares}
#'     \item{Mean Sq}{Mean sum of squares}
#'     \item{F value}{F-statistic}
#'     \item{Pr(>F)}{P-value}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Row-scales the expression matrix using `row_scale_function()`
#'   \item Calculates Euclidean distance matrix on transposed, scaled data
#'   \item Computes beta dispersion using `vegan::betadisper()`
#'   \item Performs ANOVA on dispersion to test for group differences
#' }
#'
#' Beta dispersion measures the variability (spread) of samples within groups.
#' Significant ANOVA results indicate that groups have different dispersions,
#' which may suggest batch effects or technical variation.
#'
#' @note
#' - Requires row names of `meta` to match column names of `m`
#' - Column name with special characters (spaces, backticks) is handled
#' - Prints the column name and constructed command for debugging
#'
#' @seealso
#' \code{\link{anova_betadine_tidy_function}} for testing multiple annotations
#' \code{\link{row_scale_function}} for row scaling
#' \code{\link[vegan]{betadisper}} for beta dispersion analysis
#'
#' @importFrom vegan vegdist betadisper
#' @importFrom stats anova
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using ExpressionSet
#' library(Biobase)
#' data(sample_ExpressionSet)
#' 
#' m <- exprs(sample_ExpressionSet)
#' meta <- pData(sample_ExpressionSet)
#' 
#' # Test for batch effects by sample type
#' anova_result <- anova_betadine_function(m, meta, "type")
#' print(anova_result)
#' 
#' # Check significance
#' if (anova_result$`Pr(>F)`[1] < 0.05) {
#'   message("Significant batch effect detected!")
#' }
#' }
#'
#' @note Original location: Batch_Effect_functions.R
anova_betadine_function <- function(m, meta, col_name) {
	
	# Validate inputs
	if (! col_name %in% colnames(meta)) {
		stop(sprintf("Column '%s' not found in metadata", col_name))
	}
	
	if (! all(rownames(meta) %in% colnames(m))) {
		stop("Row names of 'meta' must match column names of 'm'")
	}
	
	# Print column being tested
	message("Testing annotation: ", col_name)
	
	# Row scale the expression data
	m_RC <- row_scale_function(m)
	
	# Calculate Euclidean distance matrix for centered and transposed data
	# Select columns based on row names in metadata
	diss <- vegan::vegdist(t(m_RC[, rownames(meta)]), method = "euclidean")
	
	# Perform beta dispersion analysis
	# Dynamically construct command to handle column names with special characters
	cmd <- paste0("betadine <- vegan::betadisper(diss, group = meta$`", col_name, "`)")
	message("Command: ", cmd)
	eval(parse(text = cmd))
	
	# Perform ANOVA on beta dispersion result
	# Tests whether dispersion differs significantly between groups
	anova_result <- stats::anova(betadine)
	
	return(anova_result)
}
