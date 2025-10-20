#' Pooled Normal Visualization Module UI
#'
#' @param id Character; module namespace ID
#' @param title Character; title for the box
#' @param status Character; box status color (default "info")
#' @param show_density Logical; whether to show density plot (default TRUE)
#'
#' @export
mod_pn_viz_ui <- function(id, title = "Data Visualization", status = "info", show_density = TRUE) {
	ns <- NS(id)
	
	tagList(
		actionButton(ns('debug'),'Denug'),
		shinydashboard::box(
			title = title,
			width = NULL,
			status = status,
			solidHeader = TRUE,
			collapsible = TRUE,
			
			plotOutput(ns("heatmap"), height = "400px")
		),
		
		shinydashboard::box(
			title = paste(title, "- Correlation"),
			width = NULL,
			status = status,
			solidHeader = FALSE,
			collapsible = TRUE,
			collapsed = FALSE,
			
			plotOutput(ns("correlation"), height = "400px")
		),
		
		if (show_density) {
			shinydashboard::box(
				title = paste(title, "- Density"),
				width = NULL,
				status = status,
				solidHeader = FALSE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				plotOutput(ns("density"), height = "350px")
			)
		}
	)
}

#' Pooled Normal Visualization Module Server
#'
#' @param id Character; module namespace ID
#' @param data Reactive; matrix with features as rows, samples as columns
#' @param metadata Reactive; data frame with sample metadata (rownames = sample IDs)
#' @param title Character or Reactive; title for plots
#' @param annotation_cols Reactive; character vector of metadata columns to annotate
#' @param highlight_features Reactive; character vector of features to highlight (optional)
#' @param cluster_rows Logical or Reactive; whether to cluster rows (default FALSE)
#' @param cluster_cols Logical or Reactive; whether to cluster columns (default FALSE)
#' @param show_density Logical; whether to render density plot (default TRUE)
#'
#' @export
mod_pn_viz_server <- function(id, 
															data, 
															metadata, 
															title = "Data",
															annotation_cols = reactive(NULL),
															highlight_features = reactive(NULL),
															cluster_rows = FALSE,
															cluster_cols = FALSE,
															show_density = TRUE) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$debug,{
			browser()
		})
		# Get title (handle both reactive and static)
		get_title <- reactive({
			if (is.reactive(title)) {
				title()
			} else {
				title
			}
		})
		
		# Get cluster settings (handle both reactive and static)
		get_cluster_rows <- reactive({
			if (is.reactive(cluster_rows)) {
				cluster_rows()
			} else {
				cluster_rows
			}
		})
		
		get_cluster_cols <- reactive({
			if (is.reactive(cluster_cols)) {
				cluster_cols()
			} else {
				cluster_cols
			}
		})
		
		# Heatmap ####
		output$heatmap <- renderPlot({
			req(data(), metadata())
			
			plot_data <- data()
			plot_metadata <- metadata()
			
			# Validate data
			if (ncol(plot_data) == 0 || nrow(plot_data) == 0) {
				plot.new()
				text(0.5, 0.5, "No data available", cex = 1.5, col = "red")
				return(NULL)
			}
			
			# Prepare column annotations
			annotation_col <- NULL
			anno_cols <- annotation_cols()
			if (!is.null(anno_cols) && length(anno_cols) > 0) {
				# Match sample order
				annotation_col <- plot_metadata[colnames(plot_data), anno_cols, drop = FALSE]
			}
			
			# Prepare row annotations (highlight specific features)
			annotation_row <- NULL
			highlight_feat <- highlight_features()
			if (!is.null(highlight_feat) && length(highlight_feat) > 0) {
				annotation_row <- data.frame(
					Highlighted = ifelse(rownames(plot_data) %in% highlight_feat, "Yes", "No"),
					row.names = rownames(plot_data)
				)
			}
			
			# Create heatmap
			pheatmap::pheatmap(
				plot_data,
				main = get_title(),
				annotation_col = annotation_col,
				annotation_row = annotation_row,
				annotation_legend = FALSE,
				cluster_rows = get_cluster_rows(),
				cluster_cols = get_cluster_cols(),
				show_rownames = FALSE,
				show_colnames = TRUE,
				fontsize_row = 8,
				fontsize_col = 10,
				color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
				annotation_colors = if (!is.null(annotation_row)) {
					list(Highlighted = c(Yes = "#d62728", No = "grey80"))
				} else NULL
			)
		})
		
		# Correlation Plot ####
		output$correlation <- renderPlot({
			req(data())
			
			plot_data <- data()
			
			# Need at least 2 samples
			if (ncol(plot_data) < 2) {
				plot.new()
				text(0.5, 0.5, "Need at least 2 samples for correlation", cex = 1.5, col = "red")
				return(NULL)
			}
			
			# Transpose for correlation (samples as rows, features as columns)
			tryCatch({
				correlation_title_plot_function(
					plot_data = plot_data,
					method = "pearson"
				)
				#title(sub = get_title(), line = 0.5, cex.sub = 1.2)
			}, error = function(e) {
				plot.new()
				text(0.5, 0.5, paste("Correlation error:", e$message), cex = 1.2, col = "red")
			})
		})
		
		# Density Plot (only if enabled) ####
		if (show_density) {
			output$density <- renderPlot({
				req(data(), metadata())
				
				plot_data <- data()
				plot_metadata <- metadata()
				
				if (ncol(plot_data) == 0) {
					plot.new()
					text(0.5, 0.5, "No data available", cex = 1.5, col = "red")
					return(NULL)
				}
				
				# Convert to long format for plotting
				# data_long <- data.frame(
				# 	Sample = rep(colnames(plot_data), each = nrow(plot_data)),
				# 	Feature = rep(rownames(plot_data), ncol(plot_data)),
				# 	Expression = as.vector(plot_data),
				# 	stringsAsFactors = FALSE
				# )
				data_long = plot_data %>% 
					as.data.frame() %>% 
					rownames_to_column("Feature") %>% 
					gather(key = Sample, value = Expression,-1)
				
				# Add group information if annotation columns provided
				anno_cols <- annotation_cols()
				if (!is.null(anno_cols) && length(anno_cols) > 0) {
					# Use first annotation column for coloring
					group_col <- anno_cols[1]
					data_long$Group <- plot_metadata[data_long$Sample, group_col]
				} else {
					data_long$Group <- "All"
				}
				
				# Create density plot
				ggplot2::ggplot(data_long, ggplot2::aes(x = Expression, color = Sample, group = Sample)) +
					ggplot2::geom_density(size = 1, alpha = 0.7) +
					ggplot2::labs(
						title = paste(get_title(), "- Expression Distribution"),
						x = "Expression Value",
						y = "Density"
					) +
					ggplot2::theme_minimal(base_size = 14) +
					ggplot2::theme(
						legend.position = "right",
						plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
					)
			})
		}
		
		# Return reactive values if needed
		return(reactive({
			list(
				data = data(),
				metadata = metadata()
			)
		}))
	})
}