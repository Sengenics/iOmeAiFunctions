#' Estimate the mode of a numeric vector using kernel density
#' @note File:maths_function.R
#'
#' This function estimates the mode (most frequent value) of a numeric vector
#' by calculating the peak of its kernel density estimate.
#' If fewer than 4 non-NA values are present, it returns NA.
#'
#' @param values A numeric vector.
#'
#' @return A single numeric value representing the mode, or \code{NA} if not enough data.
#' 
#' @export
#'
#' @examples
#' mode_function(c(1, 2, 2, 3, 4))
#' mode_function(c(NA, 2, 2, NA, 3))
#' @export
mode_function = function(values){
	values = values[!is.na(values)]
	if (length(values) > 3) {
		d = density(values)
		mode_x = d$x[which.max(d$y)]
		#mode_x = density_df$x[grep(max(density_df$y),density_df$y)]
	} else {
		mode_x = NA
	}
	return(mode_x)
}

#' Row-Center Expression Matrix
#'
#' Centers each row (feature) of an expression matrix by subtracting the row mean.
#' This removes baseline differences between features while preserving relative
#' sample-to-sample variation.
#'
#' @param m Numeric matrix.  Expression data with features in rows and samples
#'   in columns.  Typically from `Biobase::exprs(ExpressionSet)`. 
#'
#' @return Numeric matrix of the same dimensions as `m`, with each row centered
#'   to have a mean of zero.  Column and row names are preserved.
#'
#' @details
#' Row centering is performed by:
#' \enumerate{
#'   \item Transposing the matrix so rows become columns
#'   \item Using `scale()` with `center = TRUE` and `scale = FALSE`
#'   \item Transposing back to original orientation
#' }
#'
#' This transformation:
#' \itemize{
#'   \item Removes baseline expression differences between features
#'   \item Preserves relative expression patterns across samples
#'   \item Does NOT scale by standard deviation (no normalization)
#'   \item Maintains the original matrix dimensions and names
#' }
#'
#' Row centering is commonly used before distance calculations in batch effect
#' analysis, as it focuses on expression patterns rather than absolute levels.
#'
#' @seealso
#' \code{\link{anova_betadine_function}} which uses this function
#' \code{\link[base]{scale}} for the underlying centering operation
#'
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
#' 
#' # Row-center the data
#' m_centered <- row_scale_function(m)
#' 
#' # Verify centering: row means should be ~0
#' rowMeans(m_centered)
#' 
#' # Compare original vs centered
#' par(mfrow = c(1, 2))
#' hist(rowMeans(m), main = "Original Row Means")
#' hist(rowMeans(m_centered), main = "Centered Row Means")
#' }
#'
#' @note maths_functions.R
row_scale_function <- function(m) {
	
	# Validate input
	if (! is.matrix(m) && !is.data.frame(m)) {
		stop("Input 'm' must be a matrix or data frame")
	}
	
	if (!is.numeric(as.matrix(m))) {
		stop("Input 'm' must contain numeric values")
	}
	
	# Center each row by subtracting row mean
	# scale = FALSE: do not divide by standard deviation
	# center = TRUE: subtract the mean
	# Double transpose to center rows instead of columns
	m_RC <- t(scale(t(m), scale = FALSE, center = TRUE))
	
	return(m_RC)
}

# 
# hyperbolic_function <- function(x, 
# 																x_min = 250, y_min = 10,
# 																x_max = 1000, y_max = 2,
# 																type = 'Protein') {
# 	x_mid = y_max*x_max/100
# 	print(x_mid)
# 	if(type == 'Protein'){
# 		print(x_mid)
# 		y = ifelse(x <= x_min, y_min, 
# 							 ifelse(x >= x_max,y_max,
# 							 			 x_mid/x*100))
# 	}else{
# 		slope = (y_max - y_min) / (x_max - x_min)
# 		intercept = y_max - (slope * x_max)
# 
# 		y = ifelse(x <= x_min, y_min, 
# 							 ifelse(x >= x_max,y_max,
# 							 			 ((((0.08889 * x) + 55.556))/x)*100))
# 	}
# 	y
# }
# 
# calculateY <- function(x) {
# 	hyperbolic_function(x,
# 											500,5,
# 											1000,2.5
# 	)
# }
# 
# calculateY_plot = function(){
# 	x = seq(0,2000,1)
# 	df = data.frame(x = x) %>% 
# 		rowwise() %>% 
# 		mutate(y = calculateY(x))%>% 
# 		mutate(spots = x * y /100) %>% 
# 		ungroup()
# 	
# 	correction_factor = 2.5
# 	inverse_factor = 1/correction_factor
# 	
# 	p = ggplot(df) + 
# 		geom_line(aes(x = x, y = y,col = '% Proteins'), size = 2) + 
# 		geom_line(aes(x = x, y = spots / correction_factor, col = '# Protein'), size = 2) +
# 		scale_y_continuous(
# 			"% Proteins",
# 			breaks = seq(0,20,2),
# 			sec.axis = sec_axis(~ . * correction_factor, name = "# Proteins")
# 		) + 
# 		theme(text = element_text(size = 20)) + 
# 		geom_vline(xintercept = c(50,500)) + 
# 		geom_point(x = 114, y = calculateY(114), aes(fill = 'PAI'), shape = 21, size = 5 ) + 
# 		geom_point(x = 270, y = calculateY(270), aes(fill = 'CTA'), shape = 21, size = 5 ) + 
# 		geom_point(x = 1877, y = calculateY(1877), aes(fill = 'i-Ome v6'), shape = 21, size = 5 ) +
# 		labs(colour = 'Lines', fill = 'Products')
# 	print(p)
# }
# 
# calculateY_plot()
# 
# 
# hyperbolic_curve()
# hyperbolic_curve <- function(x, x_start = 50, x_end = 500, y_start = 10, y_end = 1, steepness = 0.01) {
# 	# Ensure proper direction
# 	if (y_start < y_end) {
# 		# Inverted sigmoid (starts low, ends high)
# 		y_end + (y_start - y_end) / (1 + exp(steepness * (x - x_start)))
# 	} else {
# 		# Normal sigmoid (starts high, ends low)
# 		y_end + (y_start - y_end) / (1 + exp(steepness * (x - ((x_start + x_end) / 2))))
# 	}
# }
# 
# # Example usage:
# calculateY <- function(x) {
# 	hyperbolic_curve(x, 
# 									 x_start = 50,   # curve starts transitioning here
# 									 x_end = 500,    # curve ends transitioning here  
# 									 y_start = 10,   # value at start
# 									 y_end = 1)      # value at end
# }

