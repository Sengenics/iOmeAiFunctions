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
#' @importFrom stats density
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
