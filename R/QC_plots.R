#' Protein Coefficient of Variation Plot
#'
#' Generates a scatter plot and histogram to visualize the coefficient of variation (CV)
#' for each protein (or specified feature) from a provided dataframe.
#'
#' @param TR_df Dataframe containing the data to analyze.
#' @param feature_column Character string indicating the column name to group features by (default: `'Protein'`).
#' @param data_column Character string indicating the column name containing the numeric values to calculate CV (default: `'value'`).
#' @param cv_cutoff Numeric value indicating the cutoff for labeling features with high CV (default: `10`).
#'
#' @return A list containing:
#' \describe{
#'   \item{cv_df}{Dataframe with calculated mean, standard deviation, and CV for each feature.}
#'   \item{plot}{ggplot object representing a scatter plot of mean vs. CV, labeled for features exceeding the CV cutoff.}
#' }
#'
#' @details
#' CV is calculated as \(\frac{sd}{mean} \times 100\).
#'
#' The plot includes:
#' - A scatter plot showing mean vs. CV.
#' - A horizontal dashed line indicating the CV cutoff.
#' - Labels identifying features exceeding the CV cutoff.
#'
#' Requires the `ggrepel` package for label repulsion in the plot.
#'
#' @importFrom ggrepel geom_text_repel
#'
#' @note
#' Version 1.0.0 from
#' QC_plots.R
#'
#' @export
TR_Protein_CV_function = function(TR_df, feature_column = 'Protein', data_column = 'value', cv_cutoff = 10){

	cv_df = TR_df %>%
		group_by(!!sym(feature_column),Labels) %>%
		summarise(mean = mean(!!sym(data_column), na.rm = TRUE),
							sd = sd(!!sym(data_column), na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(cv = sd / mean * 100)

	cv_df_labels = cv_df %>%
		filter(cv > cv_cutoff)

	ggplot(cv_df) +
		geom_histogram(aes(x = cv))

	p = ggplot(cv_df) +
		geom_point(aes(x = mean, y = cv)) +
		geom_text_repel(data = cv_df_labels,
										aes(x = mean, y = cv, label = !!sym(feature_column)),
										max.overlaps = Inf) +
		geom_hline(yintercept = cv_cutoff, col = 'blue', linetype = 'dashed') +
	  facet_grid(. ~ Labels) +
	  xlab('log2(NetI)') +
	  ylab('CV %') +
		ggtitle("NetI")

	list(cv_df = cv_df, plot = p)
}
