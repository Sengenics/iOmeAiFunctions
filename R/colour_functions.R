#' KL Color Palette
#'
#' A qualitative color palette compiled from all available qualitative palettes in `RColorBrewer`.
#' Designed to provide a large number of distinct colors for categorical annotations (e.g., heatmaps).
#'
#' @format A character vector of hex color codes.
#' @export
#'
#' @note
#' Version 1.0 from  
#' colour_functions.R
#' @export
KL_palette <- {
	qual_col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual', ]
	KL_palette = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
	remove_cols = c('#FFFF99','#FFFFCC','#FFFF33',"#FFFFB3","#FFF2AE") #yellow colours can be hard to see
	KL_palette = KL_palette[!KL_palette %in% remove_cols]
	KL_palette
}


#' Assign Distinct Colors to Categorical Metadata Variables
#'
#' This function assigns colors to categorical variables using a supplied color palette.
#' It is useful for creating annotation color mappings (e.g., for heatmaps or visual groupings).
#' Only non-numeric (categorical) columns are assigned colors. For each variable, unique levels
#' are converted to factors and assigned distinct colors.
#'
#' @param meta_colors A data frame with samples as rows and metadata variables as columns.
#' @param variables A character vector of column names in `meta_colors` to assign colors to.
#' @param palette A vector of colors from which to draw (default is `KL_palette`).
#'
#' @return A named list, where each element corresponds to a variable and contains
#'   a named vector mapping factor levels to color hex codes.
#' @export
#'
#' @examples
#' \dontrun{
#' anno_col <- color_distinct(meta_colors = meta, variables = s_cols, palette = KL_palette)
#' }
#'
#' @note
#' Version 1.0 from  
#' colour_functions.R
#' @export
color_distinct <- function(meta_colors = NULL, variables = NULL, palette = KL_palette) {
	
	# Subset metadata based on number of columns
	# If meta_colors has only one column, use as-is
	# Otherwise, subset selected variables
	if (ncol(meta_colors) == 1) {
		meta_sub <- meta_colors
	} else {
		meta_sub <- meta_colors[, variables, drop = FALSE]
	}
	
	mycolors2 <- list()  # List to collect color assignments
	
	for (i in 1:length(variables)) {
		
		if (length(variables) > 1) {
			# Case: multiple variables provided
			
			if (!is.numeric(meta_sub[, i])) {
				# Only assign colors to non-numeric variables
				
				meta_sub[, i] <- as.factor(meta_sub[, i])  # Convert to factor
				n_colors <- nlevels(meta_sub[, i])         # Count number of factor levels
				
				# Randomly sample colors for each level
				mycolors <- list(category = sample(palette, n_colors))
				
				# Name the list and color vector for clarity
				names(mycolors) <- colnames(meta_sub)[i]
				names(mycolors[[1]]) <- levels(meta_sub[, i])
				
				print(mycolors)  # Optional: print for inspection/debug
				mycolors2 <- c(mycolors, mycolors2)  # Append to the full list
			}
			
		} else {
			# Case: single variable
			
			if (!is.numeric(meta_sub[, i])) {
				meta_sub[, 1] <- as.factor(meta_sub[, 1])
				n_colors <- nlevels(meta_sub[, 1])
				
				# Assign colors to factor levels
				mycolors2 <- list(category = sample(palette, n_colors))
				names(mycolors2) <- colnames(meta_colors)[variables]
				names(mycolors2[[1]]) <- levels(meta_sub[, 1])
				
				print(mycolors2)
			}
		}
	}
	
	return(mycolors2)
}


