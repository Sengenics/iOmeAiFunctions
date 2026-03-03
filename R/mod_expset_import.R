#' ExpressionSet Import Module UI
#'
#' UI for uploading and importing ExpressionSet data files.
#' Provides a quick-select dataset menu, file upload, validation, and status display.
#'
#' @param id Character; module namespace ID
#' @param debug Logical; show debug button. Default FALSE.
#' @param dataset_choices Named character vector of dataset labels to file paths.
#'   If NULL, the quick-select menu is hidden. Pass from global.R.
#'   Example: c("PAD4 IgA" = "~/path/to/ExpSet_list.rds", "Study B" = "~/path/B.rds")
#'
#' @return tagList with dataset selector, file input, and status UI elements
#' @export
#'
#' @note
#' Version 2.0 - Added dataset_choices quick-select menu
#' Version 1.0 - Created for modular ExpSet import
#' Original pattern from Denoiser app server.R
mod_expset_import_ui <- function(id, debug = FALSE, dataset_choices = NULL) {
	ns <- NS(id)
	
	tagList(
		
		# --- Debug button (RStudio only) ---
		if (debug) {
			fluidRow(
				column(
					width = 12,
					actionButton(
						ns("debug_import"),
						"🔍 Debug Import Module",
						icon = icon("bug"),
						class = "btn-warning btn-sm",
						style = "margin-bottom: 10px;"
					)
				)
			)
		},
		
		# --- Quick-select dataset menu (shown only if dataset_choices provided) ---
		if (!is.null(dataset_choices)) {
			fluidRow(
				column(
					width = 6,
					div(
						style = "padding: 12px; background-color: #f0f4f8; border: 1px solid #c8d8e8; border-radius: 6px; margin-bottom: 15px;",
						div(
							style = "font-weight: bold; margin-bottom: 8px; color: #2c3e50;",
							icon("database"), " Quick Load Dataset"
						),
						selectInput(
							ns("dataset_select"),
							label = NULL,
							choices = c("-- Select a dataset --" = "", dataset_choices),
							selected = "",
							width = "100%"
						),
						uiOutput(ns("dataset_select_status"))
					)
				),
				column(
					width = 6,
					div(
						style = "padding: 12px; color: #6c757d; font-size: 0.9em; margin-top: 5px;",
						icon("info-circle"),
						" Select a pre-configured dataset above, or upload your own file below."
					)
				)
			)
		},
		
		# --- File upload ---
		fluidRow(
			column(
				width = 4,
				fileInput(
					ns("expset_file"),
					"Upload ExpSet .rds File",
					accept = c(".rds", ".RDS"),
					placeholder = "No file selected",
					buttonLabel = "Browse...",
					width = "100%"
				)
			),
			column(3),
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
#' Server logic for loading ExpressionSet data via quick-select menu or file upload.
#' Priority order: (1) uploaded file, (2) quick-select dataset, (3) package data.
#'
#' @param id Character; module namespace ID
#' @param debug Logical; enable debug observer. Default FALSE.
#'
#' @return List with reactive elements:
#' \describe{
#'   \item{ExpSet_list}{Reactive returning list of ExpressionSets}
#'   \item{source}{Reactive returning "uploaded", "selected", "package", or "none"}
#'   \item{status}{Reactive returning status message string}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # In global.R - define available datasets
#' dev_datasets <- c(
#'   "PAD4 IgA (2024)"    = "~/SBI/.../ExpSet_list.rds",
#'   "Study B"            = "~/SBI/.../StudyB_ExpSet_list.rds"
#' )
#'
#' # In ui.R
#' mod_expset_import_ui("expset_import", debug = run_debug, dataset_choices = dev_datasets)
#'
#' # In server.R
#' expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
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
#' Version 2.0 - Added quick-select dataset support
#' Version 1.0 - Created for modular ExpSet import
#' Original pattern from Denoiser app server.R
mod_expset_import_server <- function(id, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# -------------------------------------------------------------------------
		# State
		# -------------------------------------------------------------------------
		uploaded_expset  <- reactiveVal(NULL)   # from file upload
		selected_expset  <- reactiveVal(NULL)   # from quick-select menu
		upload_status    <- reactiveVal("No file uploaded")
		upload_success   <- reactiveVal(FALSE)
		data_source      <- reactiveVal("none")
		
		# -------------------------------------------------------------------------
		# Debug button
		# -------------------------------------------------------------------------
		observeEvent(input$debug_import, {
			message("\n╔═══════════════════════════════════════════════════════════╗")
			message("║          🔍 DEBUG MODE - ExpSet Import Module            ║")
			message("╚═══════════════════════════════════════════════════════════╝")
			message("\n📍 Available reactives:")
			message("   uploaded_expset()   : File-uploaded data")
			message("   selected_expset()   : Quick-select data")
			message("   data_source()       : 'uploaded' | 'selected' | 'package' | 'none'")
			message("   upload_status()     : Status message")
			message("   ExpSet_list()       : Final resolved ExpSet_list")
			message("   input$dataset_select: Selected dataset path")
			message("═══════════════════════════════════════════════════════════\n")
			browser()
		})
		
		# -------------------------------------------------------------------------
		# Quick-select: load dataset from path
		# -------------------------------------------------------------------------
		observeEvent(input$dataset_select, {
			req(nchar(input$dataset_select) > 0)
			
			path <- input$dataset_select
			
			# Reset upload to give select priority
			uploaded_expset(NULL)
			
			output$dataset_select_status <- renderUI({
				div(style = "font-size: 0.85em; color: #0c5460;",
						icon("spinner", class = "fa-spin"), " Loading...")
			})
			
			tryCatch({
				if (!file.exists(path)) {
					stop("File not found: ", path)
				}
				
				expset_data <- readRDS(path)
				expset_data <- validate_and_wrap_expset(expset_data)  # see helper below
				
				selected_expset(expset_data)
				data_source("selected")
				upload_status(paste("✅ Loaded:", basename(path)))
				upload_success(TRUE)
				
				output$dataset_select_status <- renderUI({
					div(style = "font-size: 0.85em; color: #155724;",
							icon("check-circle"),
							paste(" Loaded:", length(expset_data), "ExpressionSet(s)"))
				})
				
				message("✓ Quick-select loaded: ", path)
				message("✓ ExpSets: ", paste(names(expset_data), collapse = ", "))
				
			}, error = function(e) {
				selected_expset(NULL)
				data_source("none")
				upload_status(paste("❌ Error:", e$message))
				
				output$dataset_select_status <- renderUI({
					div(style = "font-size: 0.85em; color: #721c24;",
							icon("exclamation-triangle"), " ", e$message)
				})
				
				message("✗ Quick-select error: ", e$message)
			})
		})
		
		# -------------------------------------------------------------------------
		# File upload: load dataset from uploaded file
		# -------------------------------------------------------------------------
		observeEvent(input$expset_file, {
			req(input$expset_file)
			
			upload_status("Loading...")
			upload_success(FALSE)
			
			# Reset quick-select to give upload priority
			selected_expset(NULL)
			
			tryCatch({
				message("Loading ExpSet from: ", input$expset_file$name)
				expset_data <- readRDS(input$expset_file$datapath)
				expset_data <- validate_and_wrap_expset(expset_data)
				
				uploaded_expset(expset_data)
				data_source("uploaded")
				upload_status(paste("✅ Successfully loaded:", input$expset_file$name))
				upload_success(TRUE)
				
				showNotification(
					HTML(paste0(
						"<strong>✅ ExpSet loaded successfully!</strong><br>",
						"File: ", input$expset_file$name, "<br>",
						"Contains: ", length(expset_data), " ExpressionSet(s)"
					)),
					type = "message",
					duration = 8
				)
				
				message("✓ Upload validation passed")
				message("✓ Available esets: ", paste(names(expset_data), collapse = ", "))
				
			}, error = function(e) {
				uploaded_expset(NULL)
				upload_status(paste("❌", e$message))
				upload_success(FALSE)
				data_source("none")
				
				showNotification(
					HTML(paste0("<strong>❌ Error loading ExpSet:</strong><br>", e$message)),
					type = "error",
					duration = 10
				)
				
				message("✗ Upload error: ", e$message)
			})
		})
		
		# -------------------------------------------------------------------------
		# Status UI
		# -------------------------------------------------------------------------
		output$expset_status_ui <- renderUI({
			status  <- upload_status()
			success <- upload_success()
			
			if (success) {
				div(
					style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
					icon("check-circle"), strong(status)
				)
			} else if (grepl("Error|❌", status)) {
				div(
					style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;",
					icon("exclamation-triangle"), strong(status)
				)
			} else if (status == "Loading...") {
				div(
					style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
					icon("spinner", class = "fa-spin"), strong(" Loading...")
				)
			} else {
				div(
					style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; color: #6c757d;",
					icon("info-circle"), status
				)
			}
		})
		
		# -------------------------------------------------------------------------
		# Resolved ExpSet_list: upload > quick-select > package
		# -------------------------------------------------------------------------
		ExpSet_list <- reactive({
			
			# Priority 1: uploaded file
			if (!is.null(uploaded_expset())) {
				message("✓ Using uploaded ExpSet data")
				return(uploaded_expset())
			}
			
			# Priority 2: quick-select
			if (!is.null(selected_expset())) {
				message("✓ Using quick-selected ExpSet data")
				return(selected_expset())
			}
			
			# Priority 3: package data
			tryCatch({
				data(ExpSet_list, package = "iOmeAiFunctions", envir = environment())
				if (exists("ExpSet_list", inherits = FALSE)) {
					message("✓ Using ExpSet_list from iOmeAiFunctions package")
					data_source("package")
					upload_status("Using default ExpSet from package")
					return(ExpSet_list)
				}
			}, error = function(e) {
				message("ℹ️ ExpSet_list not found in package: ", e$message)
			})
			
			# Nothing available
			data_source("none")
			upload_status("No ExpSet data available. Please select or upload a file.")
			return(NULL)
		})
		
		# -------------------------------------------------------------------------
		# Return
		# -------------------------------------------------------------------------
		return(list(
			ExpSet_list = ExpSet_list,
			source      = reactive(data_source()),
			status      = reactive(upload_status())
		))
	})
}


# =============================================================================
# Helper: validate and wrap ExpressionSet input
# =============================================================================
# Shared by both upload and quick-select paths.
# Returns a named list of ExpressionSets or throws an error with a clear message.

validate_and_wrap_expset <- function(expset_data) {
	
	# Single ExpressionSet → wrap in list
	if (inherits(expset_data, "ExpressionSet")) {
		message("✓ Single ExpressionSet detected — wrapping in list")
		expset_data <- list(uploaded_data = expset_data)
	}
	
	# Must be a list
	if (!is.list(expset_data)) {
		stop("Invalid data type: ", class(expset_data)[1],
				 ". Expected ExpressionSet or named list of ExpressionSets.")
	}
	
	# All elements must be ExpressionSets
	all_eset <- all(sapply(expset_data, function(x) inherits(x, "ExpressionSet")))
	if (!all_eset || length(expset_data) == 0) {
		stop("List does not contain valid ExpressionSet objects.")
	}
	
	# Validate pData
	valid_pdata <- sapply(expset_data, function(eset) {
		tryCatch({
			pd <- Biobase::pData(eset)
			!is.null(pd) && nrow(pd) > 0
		}, error = function(e) FALSE)
	})
	
	# Validate assay data (any named assay element)
	valid_exprs <- sapply(expset_data, function(eset) {
		tryCatch({
			assay_names <- Biobase::assayDataElementNames(eset)
			if (length(assay_names) == 0) return(FALSE)
			first_assay <- Biobase::assayDataElement(eset, assay_names[1])
			!is.null(first_assay) && nrow(first_assay) > 0 && ncol(first_assay) > 0
		}, error = function(e) FALSE)
	})
	
	if (!all(valid_pdata) || !all(valid_exprs)) {
		stop("ExpressionSets missing required components (assay data or pData).")
	}
	
	return(expset_data)
}