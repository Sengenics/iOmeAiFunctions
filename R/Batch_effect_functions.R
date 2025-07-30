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
