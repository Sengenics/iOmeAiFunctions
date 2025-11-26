#' ExpressionSet Import Module UI
#'
#' UI for uploading and importing ExpressionSet data files.
#' Provides file upload, validation, and status display.
#'
#' @param id Character; module namespace ID
#'
#' @return tagList with file input and status UI elements
#' @export
#'
#' @note
#' Version 1.0 - Created for modular ExpSet import
#' Original pattern from Denoiser app server.R
#' @export
mod_expset_import_ui <- function(id) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			column(
				width = 4,
				fileInput(
					ns("expset_file"),
					"Choose ExpSet. rds File",
					accept = c(".rds", ".RDS"),
					placeholder = "No file selected",
					buttonLabel = "Browse.. .",
					width = "100%"
				)
			),
			column(
				width = 3,
				br(),
				actionButton(
					ns("load_expset"),
					"Load ExpSet File",
					icon = icon("file-upload"),
					class = "btn-success btn-lg",
					style = "margin-top: 5px;"
				)
			),
			column(
				width = 5,
				br(),
				uiOutput(ns("expset_status_ui"))
			)
		),
		
		hr(),
		
		p(icon("info-circle"), strong("File Requirements:")),
		tags$ul(
			tags$li("File must be in .rds or .RDS format"),
			tags$li("File should contain an ExpressionSet object or a named list of ExpressionSets"),
			tags$li("ExpressionSets must have expression data accessible via Biobase::exprs()"),
			tags$li("Sample metadata should be available via Biobase::pData()")
		)
	)
}


#' ExpressionSet Import Module Server
#'
#' Server logic for uploading, validating, and loading ExpressionSet data. 
#' Handles file upload, validation, and provides reactive ExpSet_list.
#' If no file is uploaded, attempts to load from package data.
#'
#' @param id Character; module namespace ID
#'
#' @return List with reactive elements:
#' \describe{
#'   \item{ExpSet_list}{Reactive returning list of ExpressionSets (uploaded or default)}
#'   \item{source}{Reactive returning "uploaded", "package", or "none"}
#'   \item{status}{Reactive returning status message}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # In server
#' expset_data <- mod_expset_import_server("expset_import")
#' 
#' # Access the ExpSet_list
#' observe({
#'   req(expset_data$ExpSet_list())
#'   message("Loaded ", length(expset_data$ExpSet_list()), " ExpressionSets")
#'   message("Source: ", expset_data$source())
#' })
#' }
#'
#' @note
#' Version 1.0 - Created for modular ExpSet import
#' Original pattern from Denoiser app server.R
#' @export
mod_expset_import_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Reactive values to store state
		uploaded_expset <- reactiveVal(NULL)
		upload_status <- reactiveVal("No file uploaded")
		upload_success <- reactiveVal(FALSE)
		data_source <- reactiveVal("none")
		
		# Load ExpSet when button is clicked
		observeEvent(input$load_expset, {
			req(input$expset_file)
			
			upload_status("Loading...")
			upload_success(FALSE)
			
			tryCatch({
				# Read the RDS file
				message("Loading ExpSet from: ", input$expset_file$name)
				expset_data <- readRDS(input$expset_file$datapath)
				
				# Validate the uploaded data
				is_valid <- FALSE
				error_msg <- ""
				
				# Check if it's an ExpressionSet object
				if (inherits(expset_data, "ExpressionSet")) {
					message("✓ Single ExpressionSet detected")
					# Wrap in a list with a default name
					expset_data <- list(uploaded_data = expset_data)
					is_valid <- TRUE
				}
				# Check if it's a list of ExpressionSets
				else if (is.list(expset_data)) {
					# Verify all elements are ExpressionSets
					all_eset <- all(sapply(expset_data, function(x) inherits(x, "ExpressionSet")))
					
					if (all_eset && length(expset_data) > 0) {
						message("✓ List of ", length(expset_data), " ExpressionSets detected")
						is_valid <- TRUE
					} else {
						error_msg <- "List does not contain valid ExpressionSet objects"
					}
				} else {
					error_msg <- paste("Invalid data type:", class(expset_data)[1])
				}
				
				if (is_valid) {
					# Additional validation: check that ExpressionSets have required components
					validation_results <- sapply(expset_data, function(eset) {
						has_pdata <- !is.null(tryCatch(Biobase::pData(eset), error = function(e) NULL))
						has_pdata
					})
					
					validation_results_exprs <- sapply(expset_data, function(eset) {
						has_exprs <- !is.null(tryCatch(Biobase::exprs(eset), error = function(e) NULL))
						has_exprs
					})
					
					if (all(validation_results) && all(validation_results_exprs)) {
						# Store the loaded ExpSet
						uploaded_expset(expset_data)
						upload_status(paste("✅ Successfully loaded:", input$expset_file$name))
						upload_success(TRUE)
						data_source("uploaded")
						
						showNotification(
							HTML(paste0(
								"<strong>✅ ExpSet loaded successfully!</strong><br>",
								"File: ", input$expset_file$name, "<br>",
								"Contains: ", length(expset_data), " ExpressionSet(s)"
							)),
							type = "message",
							duration = 8
						)
						
						message("✓ ExpSet validation passed")
						message("✓ Available assays: ", paste(names(expset_data), collapse = ", "))
					} else {
						error_msg <- "ExpressionSets missing required components (exprs or pData)"
						upload_status(paste("❌ Error:", error_msg))
						showNotification(error_msg, type = "error", duration = 10)
					}
				} else {
					upload_status(paste("❌ Error:", error_msg))
					showNotification(
						HTML(paste0(
							"<strong>❌ Invalid ExpSet file</strong><br>",
							error_msg, "<br>",
							"Please upload a valid ExpressionSet or list of ExpressionSets"
						)),
						type = "error",
						duration = 10
					)
				}
				
			}, error = function(e) {
				error_message <- paste("Error loading file:", e$message)
				upload_status(paste("❌", error_message))
				upload_success(FALSE)
				
				showNotification(
					HTML(paste0("<strong>❌ Error loading ExpSet:</strong><br>", e$message)),
					type = "error",
					duration = 10
				)
				
				message("✗ Error loading ExpSet: ", e$message)
			})
		})
		
		# Render upload status with styling
		output$expset_status_ui <- renderUI({
			status <- upload_status()
			success <- upload_success()
			
			if (success) {
				div(
					style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
					icon("check-circle"),
					strong(status)
				)
			} else if (grepl("Error|❌", status)) {
				div(
					style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;",
					icon("exclamation-triangle"),
					strong(status)
				)
			} else if (status == "Loading...") {
				div(
					style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
					icon("spinner", class = "fa-spin"),
					strong(" Loading...")
				)
			} else {
				div(
					style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; color: #6c757d;",
					icon("info-circle"),
					status
				)
			}
		})
		
		# Main ExpSet_list reactive - tries upload first, then package data
		ExpSet_list <- reactive({
			# Priority 1: Use uploaded data if available
			if (!is. null(uploaded_expset())) {
				message("✓ Using uploaded ExpSet data")
				return(uploaded_expset())
			}
			
			# Priority 2: Load from package data
			tryCatch({
				data(ExpSet, package = "iOmeAiFunctions", envir = environment())
				if (exists("ExpSet", inherits = FALSE)) {
					message("✓ Using ExpSet from iOmeAiFunctions package")
					data_source("package")
					upload_status("Using default ExpSet from package")
					return(ExpSet)
				}
			}, error = function(e) {
				message("ℹ️ ExpSet not found in package: ", e$message)
			})
			
			# No data available
			data_source("none")
			upload_status("No ExpSet data available.  Please upload a file.")
			return(NULL)
		})
		
		# Return reactive values
		return(list(
			ExpSet_list = ExpSet_list,
			source = reactive(data_source()),
			status = reactive(upload_status())
		))
	})
}