#' Create Correlation Chart with Title
#'
#' Generates a correlation chart using the PerformanceAnalytics package with 
#' a formatted title based on the correlation method used.
#'
#' @param plot_data A numeric matrix or data frame containing the variables to correlate.
#'   Each column represents a variable.
#' @param method Character string specifying the correlation method to use.
#'   One of "pearson" (default), "kendall", or "spearman".
#'   See \code{\link[stats]{cor}} for details.
#'
#' @return None. This function is called for its side effect of creating a plot.
#'
#' @details
#' This function creates a correlation matrix plot using 
#' \code{\link[PerformanceAnalytics]{chart.Correlation}}.
#' The plot includes:
#' \itemize{
#'   \item Upper panel: Correlation coefficients with significance stars
#'   \item Diagonal: Density plots (histogram disabled)
#'   \item Lower panel: Scatter plots with fitted lines
#' }
#'
#' The function automatically capitalizes the correlation method name in the title
#' (e.g., "pearson" becomes "Pearson Correlation").
#'
#' @note Requires the \code{PerformanceAnalytics} package.
#'
#' @seealso \code{\link[PerformanceAnalytics]{chart.Correlation}}, \code{\link[stats]{cor}}
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#'
#' # Pearson correlation
#' correlation_title_plot_function(data, method = "pearson")
#'
#' # Spearman correlation
#' correlation_title_plot_function(data, method = "spearman")
#' }
#'
#' @export
#' @importFrom PerformanceAnalytics chart.Correlation
#' @importFrom graphics title plot.new par
correlation_title_plot_function <- function(plot_data, method = "pearson") {
	
	# Validate method
	valid_methods <- c("pearson", "kendall", "spearman")
	if (!method %in% valid_methods) {
		stop("method must be one of: ", paste(valid_methods, collapse = ", "))
	}
	
	# Validate plot_data
	if (!is.data.frame(plot_data) && !is.matrix(plot_data)) {
		stop("plot_data must be a data frame or matrix")
	}
	
	if (ncol(plot_data) < 2) {
		stop("plot_data must have at least 2 columns")
	}
	
	# Check for numeric data
	if (!all(sapply(plot_data, is.numeric))) {
		stop("All columns in plot_data must be numeric")
	}
	
	# Set plot margins
	par(mar = c(5, 4, 1, 2) + 0.1)
	
	# Initialize new plot
	plot.new()
	
	# Create correlation chart
	PerformanceAnalytics::chart.Correlation(
		plot_data, 
		histogram = FALSE,  
		pch = 20,
		silent = TRUE,
		method = method
	)
	
	# Format method name for title (capitalize first letter)
	method_formatted <- paste0(
		toupper(substr(method, 1, 1)), 
		substr(method, 2, nchar(method))
	)
	
	# Add title
	title(main = paste(method_formatted, "Correlation"), cex.main = 1.5)
	
	invisible(NULL)
}
