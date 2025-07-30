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


#' Generate Cy3 BSA Lineplot List
#'
#' Creates diagnostic QC plots related to Cy3 BSA controls, including:
#' - line plots per sample and feature
#' - histogram of log2 mean NetI
#' - batch vs signal scatter plots
#'
#' @param datCollate A list containing input data, parameters, and manifest.
#' @param PerSample_data QC results list for PerSample level.
#' @param PerProtein_data QC results list for PerProtein level.
#' @param feature_column Character. Column name for features (e.g., "Protein" or "Feature").
#'
#' @return A named list of ggplot2 objects:
#' \describe{
#'   \item{PerSample}{Feature vs NetI line plot per Sample.}
#'   \item{PerProtein}{Sample vs NetI line plot per Feature.}
#'   \item{NetI_histogram}{Histogram of log2 mean NetI across samples.}
#'   \item{NetI_Batch}{Scatter plot of NetI vs batch, colored by label.}
#' }
#'
#' @export
generate_Cy3BSA_lineplot_list <- function(datCollate,
																					PerSample_data,
																					PerProtein_data,
																					feature_column = "Protein") {

	feature_sym <- rlang::sym(feature_column)
	QC <- 'ctrl_QC'
	data_name <- 'Cy3AvgCV'

	# Labels reference table
	labels_df <- datCollate$data$RawData %>%
		dplyr::select(Labels, Sample) %>%
		distinct()

	# Control probes
	ctrl_BSA_probes <- datCollate$param$ctrl_BSA_probes

	# Filter Data for BSA probes using feature_column
	Data <- datCollate$data$Data %>%
		filter(!!feature_sym %in% ctrl_BSA_probes) %>%
		left_join(labels_df, by = "Sample")

	# Extract PerSample Cy3AvgCV table
	PerSample_sub_data <- PerSample_data[[QC]][[data_name]]$data

	# PerSample line plot: x = feature, group = Sample
	data = PerSample_data
	grouping_column = 'Sample'
	x = feature_column

	(PerSample <- QC_line_plot_list_function(
		Data,
		PerSample_data,
		QC,
		data_name,
		'Sample',
		x,
		feature_column
	) +
		geom_hline(yintercept = datCollate$param$Cy3BSA_log2RFU,
							 col = 'blue', linetype = 'dashed'))

	# PerProtein line plot: x = Sample, group = feature
	PerProtein <- QC_line_plot_list_function(
		Data,
		PerProtein_data,
		QC,
		data_name,
		'Protein',
		'Sample',
		feature_column
	) +
		geom_hline(yintercept = datCollate$param$Cy3BSA_log2RFU,
							 col = 'blue', linetype = 'dashed')

	# Histogram of log2 mean NetI
	NetI_histogram <- ggplot(PerSample_sub_data) +
		geom_histogram(aes(x = log2meanNetI), fill = 'dark grey', col = 'white') +
		geom_vline(xintercept = datCollate$param$Cy3BSA_log2RFU,
							 col = 'blue', linetype = 'dashed')

	# Batch vs NetI plot
	batch_column <- datCollate$param$batch_column
	label_df <- datCollate$manifest %>%
		dplyr::select(Sample, Labels, !!sym(batch_column))

	NetI_Batch <- PerSample_sub_data %>%
		left_join(label_df, by = "Sample") %>%
		ggplot() +
		geom_point(aes(x = !!sym(batch_column), y = log2meanNetI, col = Labels)) +
		theme(axis.text.x = element_text(angle = 90))

	list(
		PerSample = PerSample,
		PerProtein = PerProtein,
		NetI_histogram = NetI_histogram,
		NetI_Batch = NetI_Batch
	)
}



#' QC Line Plot List Function
#'
#' Prepares and formats data for line-based QC plots using provided grouping.
#'
#' @param Data Raw or filtered input data frame (typically probe-level).
#' @param data Full QC object list (with nested [[QC]][[data_name]]$data structure).
#' @param QC Character. Name of the QC list entry (e.g., 'ctrl_QC').
#' @param data_name Character. Name of the sub-data entry (e.g., 'Cy3AvgCV').
#' @param grouping_column Character. Column for grouping traces (e.g., 'Sample' or 'Feature').
#' @param x Character. Column name to use for the x-axis.
#' @param feature_column Character. Column name of features to clean (default = "Protein").
#'
#' @return A `plotly` object created via `QC_line_plotly_function()`.
#'
#' @note
#' Version 1.0.0 from
#' QC_sub_functions.R
#'
#' @export
QC_line_plot_list_function <- function(Data,
																			 data,
																			 QC,
																			 data_name,
																			 grouping_column,
																			 x,
																			 feature_column = 'Protein') {
	# Access sub-data and join with full data
	sub_data <- data[[QC]][[data_name]]
	plot_data <- Data %>%
		left_join(sub_data$data, by = "Sample")  # assumes Sample is common key

	# Clean feature names dynamically
	plot_data[[feature_column]] <- plot_data[[feature_column]] %>%
		stringr::str_replace_all("_([0-9])$", "_0\\1") %>%
		gsub("zzz_", "", ., fixed = TRUE)

	# Generate interactive line plot
	(line_plot <- QC_line_plotly_function(
		plot_data,
		title,
		grouping_column,
		x,
		feature_column
	))

	return(line_plot)
}


#' QC Line Plotly Function
#'
#' Generates a ggplot-based QC line plot for mean intensity (log2 scaled), with grouping and labeling.
#'
#' @param plot_data Data frame containing log2(mean) values and QC groupings.
#' @param title Plot title (not currently shown in plot unless added externally).
#' @param grouping_column Column name to group line traces (e.g., 'Sample').
#' @param x X-axis variable (typically 'Sample' or a feature column like 'Protein').
#' @param feature_column Column name to use for point labeling (default = "Protein").
#'
#' @return A ggplot object for interactive rendering.
#'
#' @note
#' Version 1.0.0 from
#' QC_Plots.R
#'
#' @export
QC_line_plotly_function <- function(plot_data,
																		title,
																		grouping_column = 'Sample',
																		x = 'Protein',
																		feature_column = 'Protein') {

	if (x == 'Sample') {
		p <- ggplot(plot_data) +
			geom_line(aes(
				x = !!sym(x),
				y = log2(mean),
				group = !!sym(grouping_column),
				col = !!sym(grouping_column)
			), alpha = 0.5) +
			geom_violin(aes(
				x = !!sym(x),
				y = log2(mean),
				col = QC
			)) +
			geom_point(aes(
				x = !!sym(x),
				y = log2(mean),
				fill = Labels,
				label = !!sym(feature_column)
			), shape = 21) +
			viridis::scale_color_viridis(discrete = TRUE)
	} else {
		p <- ggplot(plot_data) +
			geom_line(aes(
				x = !!sym(x),
				y = log2(mean),
				group = !!sym(grouping_column),
				col = QC
			)) +
			geom_point(aes(
				x = !!sym(x),
				y = log2(mean),
				fill = Labels,
				label = Sample
			), shape = 21) +
			scale_colour_brewer(palette = "Dark2", drop = FALSE, na.translate = FALSE)
	}

	p <- p +
		ylab("log2(mean Net Intensity)") +
		theme(
			axis.title = element_text(size = 14),
			plot.title = element_text(size = 14),
			axis.text.x = element_text(angle = 90, hjust = 0, size = 8)
		)

	if (grouping_column == 'Sample') {
		p <- p + guides(color = 'none')
	}

	return(p)
}


