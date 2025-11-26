#' Data Filter Module UI
#'
#' UI for multi-level cascading filters on ExpressionSet sample or feature data.
#' Allows users to add/remove filter levels dynamically.
#'
#' @param id Character; module namespace ID
#' @param label Character; label for the filter (e.g., "Sample Filter" or "Feature Filter")
#'
#' @return tagList with filter controls
#' @export
#'
#' @note
#' Version 1.0 - Created for heatmap module
#' Supports cascading AND logic filters
#' @export
mod_data_filter_ui <- function(id, label = "Data Filter") {
	ns <- NS(id)
	
	tagList(
		h4(label),
		uiOutput(ns("filter_ui")),
		
		fluidRow(
			column(
				width = 6,
				actionButton(
					ns("add_filter"),
					"Add Filter Level",
					icon = icon("plus"),
					class = "btn-primary btn-sm"
				)
			),
			column(
				width = 6,
				uiOutput(ns("filter_summary"))
			)
		)
	)
}


#' Data Filter Module Server
#'
#' Server logic for multi-level cascading filters on pData or fData from ExpressionSet.
#' Supports adding/removing filter levels with AND logic.
#'
#' @param id Character; module namespace ID
#' @param eset Reactive ExpressionSet
#' @param data_type Character; "pData" for samples or "fData" for features
#'
#' @return List with reactive elements:
#' \describe{
#'   \item{filtered_indices}{Reactive integer vector of indices passing all filters}
#'   \item{n_filtered}{Reactive integer, number of items passing filters}
#'   \item{n_total}{Reactive integer, total number of items}
#'   \item{filter_active}{Reactive logical, TRUE if any filters applied}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # In server - filter samples
#' sample_filter <- mod_data_filter_server(
#'   "sample_filter",
#'   eset = reactive(my_eset),
#'   data_type = "pData"
#' )
#' 
#' # Access filtered indices
#' observe({
#'   indices <- sample_filter$filtered_indices()
#'   message("Selected ", length(indices), " samples")
#' })
#' 
#' # Filter features
#' feature_filter <- mod_data_filter_server(
#'   "feature_filter",
#'   eset = reactive(my_eset),
#'   data_type = "fData"
#' )
#' }
#'
#' @note
#' Version 1.0 - Created for heatmap module
#' Filters use cascading AND logic
#' @export
mod_data_filter_server <- function(id, eset, data_type = "pData") {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Reactive values to track filter levels
		filter_levels <- reactiveVal(list())
		
		# Get the appropriate data frame
		data_df <- reactive({
			req(eset())
			
			if (data_type == "pData") {
				Biobase::pData(eset())
			} else if (data_type == "fData") {
				Biobase::fData(eset())
			} else {
				stop("data_type must be 'pData' or 'fData'")
			}
		})
		
		# Total number of rows
		n_total <- reactive({
			req(data_df())
			nrow(data_df())
		})
		
		# Add filter level
		observeEvent(input$add_filter, {
			current_filters <- filter_levels()
			new_id <- length(current_filters) + 1
			
			current_filters[[paste0("filter_", new_id)]] <- list(
				id = new_id,
				column = NULL,
				values = NULL
			)
			
			filter_levels(current_filters)
		})
		
		# Remove filter level (dynamic observers created below)
		observe({
			filters <- filter_levels()
			
			lapply(names(filters), function(filter_name) {
				filter_id <- filters[[filter_name]]$id
				remove_button_id <- paste0("remove_filter_", filter_id)
				
				observeEvent(input[[remove_button_id]], {
					current_filters <- filter_levels()
					current_filters[[filter_name]] <- NULL
					filter_levels(current_filters)
				}, ignoreInit = TRUE)
			})
		})
		
		# Render filter UI dynamically
		output$filter_ui <- renderUI({
			req(data_df())
			
			filters <- filter_levels()
			
			if (length(filters) == 0) {
				return(p(em("No filters applied.  Click 'Add Filter Level' to start filtering.")))
			}
			
			# Get currently filtered data for cascading
			current_data <- data_df()
			current_indices <- 1:nrow(current_data)
			
			filter_ui_elements <- lapply(names(filters), function(filter_name) {
				filter_info <- filters[[filter_name]]
				filter_id <- filter_info$id
				
				# Apply previous filters for cascading
				if (filter_id > 1) {
					prev_filters <- filters[1:(filter_id - 1)]
					for (prev_filter in prev_filters) {
						if (! is.null(prev_filter$column) && !is.null(prev_filter$values)) {
							current_indices <- current_indices[current_data[current_indices, prev_filter$column] %in% prev_filter$values]
						}
					}
					current_data <- current_data[current_indices, , drop = FALSE]
				}
				
				# Available columns
				available_columns <- colnames(current_data)
				
				# Column selection
				column_input <- selectInput(
					ns(paste0("filter_column_", filter_id)),
					paste("Filter", filter_id, "- Column:"),
					choices = c("Select column..." = "", available_columns),
					selected = filter_info$column %||% ""
				)
				
				# Value selection (conditional on column selection)
				value_input <- uiOutput(ns(paste0("filter_values_ui_", filter_id)))
				
				# Remove button
				remove_button <- actionButton(
					ns(paste0("remove_filter_", filter_id)),
					"Remove",
					icon = icon("times"),
					class = "btn-danger btn-sm"
				)
				
				# Combine into box
				shinydashboard::box(
					title = paste("Filter Level", filter_id),
					width = NULL,
					status = "info",
					solidHeader = FALSE,
					collapsible = TRUE,
					
					fluidRow(
						column(width = 5, column_input),
						column(width = 5, value_input),
						column(width = 2, br(), remove_button)
					)
				)
			})
			
			do.call(tagList, filter_ui_elements)
		})
		
		# Render value selection UI for each filter
		observe({
			filters <- filter_levels()
			
			lapply(names(filters), function(filter_name) {
				filter_info <- filters[[filter_name]]
				filter_id <- filter_info$id
				
				output[[paste0("filter_values_ui_", filter_id)]] <- renderUI({
					selected_column <- input[[paste0("filter_column_", filter_id)]]
					
					if (is.null(selected_column) || selected_column == "") {
						return(p(em("Select a column first")))
					}
					
					# Get available values from current data
					current_data <- data_df()
					current_indices <- 1:nrow(current_data)
					
					# Apply previous filters
					if (filter_id > 1) {
						prev_filters <- filters[1:(filter_id - 1)]
						for (prev_filter in prev_filters) {
							if (! is.null(prev_filter$column) && !is.null(prev_filter$values)) {
								current_indices <- current_indices[current_data[current_indices, prev_filter$column] %in% prev_filter$values]
							}
						}
						current_data <- current_data[current_indices, , drop = FALSE]
					}
					
					available_values <- sort(unique(as.character(current_data[[selected_column]])))
					
					selectInput(
						ns(paste0("filter_values_", filter_id)),
						"Select values:",
						choices = available_values,
						selected = filter_info$values,
						multiple = TRUE
					)
				})
			})
		})
		
		# Update filter_levels when selections change
		observe({
			filters <- filter_levels()
			
			updated <- FALSE
			for (filter_name in names(filters)) {
				filter_info <- filters[[filter_name]]
				filter_id <- filter_info$id
				
				new_column <- input[[paste0("filter_column_", filter_id)]]
				new_values <- input[[paste0("filter_values_", filter_id)]]
				
				if (! identical(filter_info$column, new_column) || !identical(filter_info$values, new_values)) {
					filters[[filter_name]]$column <- new_column
					filters[[filter_name]]$values <- new_values
					updated <- TRUE
				}
			}
			
			if (updated) {
				filter_levels(filters)
			}
		})
		
		# Calculate filtered indices
		filtered_indices <- reactive({
			req(data_df())
			
			filters <- filter_levels()
			
			if (length(filters) == 0) {
				return(1:nrow(data_df()))
			}
			
			indices <- 1:nrow(data_df())
			df <- data_df()
			
			for (filter_name in names(filters)) {
				filter_info <- filters[[filter_name]]
				
				if (!is.null(filter_info$column) && !is.null(filter_info$values) && 
						filter_info$column != "" && length(filter_info$values) > 0) {
					indices <- indices[df[indices, filter_info$column] %in% filter_info$values]
				}
			}
			
			return(indices)
		})
		
		# Filter summary
		output$filter_summary <- renderUI({
			n_filt <- length(filtered_indices())
			n_tot <- n_total()
			
			if (length(filter_levels()) == 0) {
				p(strong(paste("All", n_tot, "items")), style = "color: #6c757d;")
			} else {
				p(strong(paste("Showing", n_filt, "of", n_tot, "items")), 
					style = if (n_filt < n_tot) "color: #007bff;" else "color: #28a745;")
			}
		})
		
		# Return reactive values
		return(list(
			filtered_indices = filtered_indices,
			n_filtered = reactive(length(filtered_indices())),
			n_total = n_total,
			filter_active = reactive(length(filter_levels()) > 0)
		))
	})
}