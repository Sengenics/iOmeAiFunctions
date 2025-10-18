#' Complete Denoiser Demo App
#'
#' Full-featured demo with auto-reload during development
#'
#' @export

# Development mode: reload package functions
if (interactive()) {
	local_path <- "../" 
	
	# Document and reload
	message("Documenting package...")
	devtools::document(local_path)
	
	message("Loading all package functions...")
	devtools::load_all(local_path)
	
	# Verify key functions loaded
	required_functions <- c(
		"plot_denoise_heatmap",
		"denoise_remove_PCs",
		"mod_denoiser_ui",
		"mod_denoiser_server",
		"mod_eset_selector_ui",
		"mod_eset_selector_server",
		"diagnose_ExpSet_list",
		"quick_inspect_eset"
	)
	
	for (fn in required_functions) {
		if (exists(fn)) {
			message("✓ ", fn, " loaded")
		} else {
			warning("✗ ", fn, " NOT loaded")
		}
	}
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(Biobase)
library(DT)
library(ggplot2)
library(pheatmap)

# If not in development, load the package normally
if (!interactive()) {
	library(iOmeAiFunctions)
}

# UI ####
ui <- dashboardPage(
	skin = "blue",
	
	dashboardHeader(
		title = "Denoiser - iOme AI",
		titleWidth = 300
	),
	
	## sidebar ####
	dashboardSidebar(
		width = 300,
		sidebarMenu(
			id = "sidebar",
			menuItem("Data Selection", tabName = "data_select", icon = icon("database")),
			menuItem("PC Visualizer", tabName = "pc_viz", icon = icon("eye")),
			menuItem("Denoiser", tabName = "denoise", icon = icon("filter")),
			menuItem("Help", tabName = "help", icon = icon("question-circle"))
		)
	),
	
	dashboardBody(
		tags$head(
			tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .debug-box {
          border: 2px solid #f39c12;
        }
        .upload-box {
          border: 2px solid #00a65a;
        }
      "))
		),
		
		## tabs ####
		tabItems(
			### Data selection tab ####
			tabItem(
				tabName = "data_select",
				
				# NEW: File Upload Box
				fluidRow(
					shinydashboard::box(
						title = "Import New ExpSet Data",
						width = 12,
						status = "success",
						solidHeader = TRUE,
						class = "upload-box",
						collapsible = TRUE,
						collapsed = FALSE,
						
						p(icon("upload"), strong("Upload a new ExpSet.rds file"), "to import custom expression data"),
						
						fluidRow(
							column(
								width = 4,
								fileInput(
									"expset_file",
									"Choose ExpSet.rds File",
									accept = c(".rds", ".RDS"),
									placeholder = "No file selected",
									buttonLabel = "Browse...",
									width = "100%"
								)
							),
							column(
								width = 3,
								br(),
								actionButton(
									"load_expset",
									"Load ExpSet File",
									icon = icon("file-upload"),
									class = "btn-success btn-lg",
									style = "margin-top: 5px;"
								)
							),
							column(
								width = 5,
								br(),
								uiOutput("expset_status_ui")
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
				),
				
				fluidRow(
					shinydashboard::box(
						title = "ExpressionSet Data Selection",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						
						p("Select the expression data to use for denoising. The app will auto-load from data/ExpSet_list.rds or use your uploaded file."),
						
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "Raw/NetI Data",
									width = NULL,
									status = "info",
									solidHeader = TRUE,
									
									p("Select the raw or NetI data for denoising:"),
									mod_eset_selector_ui("eset_raw"),
									
									hr(),
									
									p(strong("Selected Data Info:")),
									verbatimTextOutput("raw_info")
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Normalized Data (Optional)",
									width = NULL,
									status = "warning",
									solidHeader = TRUE,
									
									p("Select normalized data for background visualization:"),
									mod_eset_selector_ui("eset_norm"),
									
									hr(),
									
									p(strong("Selected Data Info:")),
									verbatimTextOutput("norm_info")
								)
							)
						),
						
						hr(),
						
						shinydashboard::box(
							title = "Complete Data Summary",
							width = NULL,
							status = "success",
							collapsible = TRUE,
							verbatimTextOutput("eset_summary")
						)
					)
				),
				
	
				
			
				fluidRow(
					shinydashboard::box(
						title = "Debug & Diagnostics",
						width = 12,
						status = "warning",
						solidHeader = TRUE,
						class = "debug-box",
						
						p(icon("tools"), strong("Development Tools"), "- Inspect data structure and troubleshoot issues"),
						
						fluidRow(
							column(
								width = 3,
								actionButton(
									"debug_data",
									"🔍 Enter Debug Mode",
									icon = icon("bug"),
									class = "btn-warning btn-block"
								),
								helpText("Pause execution and inspect objects in browser mode")
							),
							column(
								width = 3,
								actionButton(
									"run_diagnostics",
									"📊 Run Full Diagnostics",
									icon = icon("stethoscope"),
									class = "btn-info btn-block"
								),
								helpText("Print detailed structure analysis to summary box")
							),
							column(
								width = 3,
								actionButton(
									"quick_check",
									"⚡ Quick Check",
									icon = icon("bolt"),
									class = "btn-secondary btn-block"
								),
								helpText("Quick validation of data structure")
							),
							column(
								width = 3,
								actionButton(
									"goto_denoiser",
									"Proceed to Denoiser →",
									icon = icon("arrow-right"),
									class = "btn-success btn-block"
								),
								helpText("Navigate to denoiser analysis")
							)
						)
					)
				),
				
				# Status indicators
				fluidRow(
					shinydashboard::box(
						title = "Data Status",
						width = 12,
						status = "info",
						
						fluidRow(
							column(
								width = 3,
								valueBoxOutput("status_eset_list", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_raw", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_norm", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_ready", width = NULL)
							)
						)
					)
				)
			),
			
			### Denoiser tab ####
			tabItem(
				tabName = "denoise",
				mod_denoiser_ui("denoiser")
			),
			
			### PC Vis ####
			tabItem(
				tabName = "pc_viz",
				mod_pc_visualizer_ui("pc_viz")
			),
			
			# Help tab
			tabItem(
				tabName = "help",
				
				fluidRow(
					shinydashboard::box(
						title = "Denoiser Help",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						
						h3("Quick Start Guide"),
						
						h4("1. Import Data (Optional)"),
						p("You can either:"),
						tags$ul(
							tags$li(strong("Use Default Data:"), "The app will auto-load from data/ExpSet_list.rds"),
							tags$li(strong("Upload New Data:"), "Click 'Browse...' to select your own ExpSet.rds file, then click 'Load ExpSet File'")
						),
						
						hr(),
						
						h4("2. Data Selection"),
						p("Select your expression data from the dropdowns. You need:"),
						tags$ul(
							tags$li(strong("Raw/NetI Data:"), "The input data for PC removal and denoising"),
							tags$li(strong("Normalized Data:"), "(Optional) For background visualization in AAb borders plots")
						),
						
						hr(),
						
						h4("3. Configure Parameters"),
						p("In the Denoiser tab, set:"),
						tags$ul(
							tags$li(strong("Number of PCs:"), "How many principal components to remove (typically 1-7)"),
							tags$li(strong("Cutpoint Range:"), "Range of thresholds to test"),
							tags$li(strong("Expected PN AAbs:"), "List of antigens expected to be elevated in Pooled Normals"),
							tags$li(strong("Expected Count:"), "Range of AAb count expected in PNs (from limma analysis)")
						),
						
						hr(),
						
						h4("4. Run Denoising"),
						p("Click", strong("Run Denoising"), "to start the analysis. This will:"),
						tags$ol(
							tags$li("Remove principal components from the data"),
							tags$li("Test multiple cutpoints"),
							tags$li("Calculate quality metrics"),
							tags$li("Select optimal parameters"),
							tags$li("Generate AAb-called data")
						),
						
						hr(),
						
						h4("5. Explore Results"),
						p("Use the tabs to:"),
						tags$ul(
							tags$li(strong("PCA & Denoising:"), "View variance explained and denoised data"),
							tags$li(strong("Cutpoint Analysis:"), "Compare different cutpoint thresholds"),
							tags$li(strong("AAb-Called Data:"), "View final AAb calls and adjust cutpoints"),
							tags$li(strong("Visualization:"), "AAb borders and t-SNE plots"),
							tags$li(strong("Summary:"), "Statistics and export template")
						),
						
						hr(),
						
						h4("6. Export Results"),
						p("Download results as a ZIP file or generate an AAb_caller_template.R script for pipeline use.")
					)
				),
				
				fluidRow(
					shinydashboard::box(
						title = "Key Metrics Explained",
						width = 12,
						status = "warning",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("TP:FP Ratio"),
						p("True Positive to False Positive ratio. Higher is better. Measures how many legitimate PN AAbs are detected vs. noise."),
						
						h4("PN AAb Count"),
						p("Number of antigens detected in ≥67% of Pooled Normal samples. Should match expected range from limma."),
						
						h4("PN Hit Rate"),
						p("Percentage of expected PN AAbs (from limma) that are successfully detected by the denoiser."),
						
						h4("ZZ Control Rates"),
						p("Fraction of samples positive for ZZ_con2/4 controls. Should approach 0% with optimal cutpoint."),
						
						h4("PSA+ Rate"),
						p("Percentage of samples typed as PSA-positive. Should be ≤5% for good quality denoising.")
					)
				),
				
				fluidRow(
					shinydashboard::box(
						title = "Debug Mode Usage",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("Using Debug Mode"),
						p("Debug mode allows you to pause execution and inspect data at any point:"),
						
						h5("In Data Selection Tab:"),
						tags$ul(
							tags$li("Click", strong("🔍 Enter Debug Mode"), "to inspect loaded ExpressionSets"),
							tags$li("Use", code("quick_inspect_eset(eset_raw())"), "to see data structure"),
							tags$li("Use", code("diagnose_ExpSet_list(ExpSet_list())"), "for full diagnostics")
						),
						
						h5("In Denoiser Tab:"),
						tags$ul(
							tags$li("Click", strong("🔍 Enter Debug Mode"), "in the controls panel"),
							tags$li("Debug mode will show context-aware information based on pipeline stage"),
							tags$li("Inspect reactive values with", code("rv$denoise_results"), "etc.")
						),
						
						h5("Common Debug Commands:"),
						tags$pre(
							"# Check ExpressionSet structure\n",
							"str(eset_raw())\n",
							"Biobase::assayDataElementNames(eset_raw())\n",
							"\n",
							"# Check expression data\n",
							"expr <- Biobase::exprs(eset_raw())\n",
							"class(expr)\n",
							"dim(expr)\n",
							"\n",
							"# Check metadata\n",
							"meta <- Biobase::pData(eset_raw())\n",
							"colnames(meta)\n",
							"table(meta$Sample_Group)\n",
							"\n",
							"# Exit debug mode\n",
							"c  # Continue execution\n",
							"Q  # Quit browser"
						)
					)
				)
			)
		)
	)
)

# Server ####
server <- function(input, output, session) {
	
	options(shiny.maxRequestSize = 100*1024^2)
	
	# ===================================================================
	# NEW: FILE UPLOAD AND IMPORT FUNCTIONALITY
	# ===================================================================
	
	# Reactive value to store uploaded ExpSet data
	uploaded_expset <- reactiveVal(NULL)
	upload_status <- reactiveVal("No file uploaded")
	upload_success <- reactiveVal(FALSE)
	
	# Load ExpSet when button is clicked
	observeEvent(input$load_expset, {
		req(input$expset_file)
		
		upload_status("Loading...")
		upload_success(FALSE)
		
		tryCatch({
			# Read the RDS file
			message("Loading ExpSet from: ", input$expset_file$name)
			expset_data <- readRDS(input$expset_file$datapath)
			print(names(expset_data))
			
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
				(validation_results <- sapply(expset_data, function(eset) {
					#has_exprs <- !is.null(tryCatch(Biobase::exprs(eset), error = function(e) NULL))
					has_pdata <- !is.null(tryCatch(Biobase::pData(eset), error = function(e) NULL))
					#has_exprs && 
					has_pdata
				}))

				(validation_results_exprs <- sapply(expset_data, function(eset) {
					has_exprs <- !is.null(tryCatch(Biobase::exprs(eset), error = function(e) NULL))
					#has_pdata <- !is.null(tryCatch(Biobase::pData(eset), error = function(e) NULL))
					has_exprs 
				#has_pdata
			}))
				
				if (all(validation_results)) {
					# Store the loaded ExpSet
					uploaded_expset(expset_data)
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
	
	# ===================================================================
	# MODIFIED: Load ExpSet_list (default or uploaded)
	# ===================================================================
	
	# Load ExpSet_list
	ExpSet_list <- reactive({
		# Priority 1: Use uploaded data if available
		if (!is.null(uploaded_expset())) {
			message("✓ Using uploaded ExpSet data")
			return(uploaded_expset())
		}
		
		# Priority 2: Try to load from default locations
		possible_paths <- c(
			"data/ExpSet_list.rds",
			"../data/ExpSet_list.rds",
			"../../data/ExpSet_list.rds",
			"../../../data/ExpSet_list.rds",
			system.file("extdata", "ExpSet_list.rds", package = "iOmeAiFunctions")
		)
		
		for (path in possible_paths) {
			if (file.exists(path)) {
				message("✓ Loading ExpSet_list from: ", path)
				return(readRDS(path))
			}
		}
		
		showNotification(
			"ℹ️ No default ExpSet_list.rds found. Please upload an ExpSet.rds file above.",
			type = "warning",
			duration = 8
		)
		return(NULL)
	})
	
	# Raw/NetI ExpressionSet selector
	eset_raw_selected <- mod_eset_selector_server(
		"eset_raw",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_ImputedlogMeanNetI"
	)
	
	# Normalized ExpressionSet selector
	eset_norm_selected <- mod_eset_selector_server(
		"eset_norm",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_loess_normalised"
	)
	
	# Extract the actual ExpressionSets
	eset_raw <- reactive({
		req(eset_raw_selected$eset())
		eset_raw_selected$eset()
	})
	
	eset_norm <- reactive({
		if (!is.null(eset_norm_selected$eset())) {
			eset_norm_selected$eset()
		} else {
			NULL
		}
	})
	
	# Status value boxes
	output$status_eset_list <- renderValueBox({
		if (!is.null(ExpSet_list())) {
			# Check if using uploaded data
			source_label <- if (!is.null(uploaded_expset())) {
				"Uploaded ExpSets"
			} else {
				"ExpressionSets Loaded"
			}
			
			valueBox(
				value = length(ExpSet_list()),
				subtitle = source_label,
				icon = icon("database"),
				color = "green"
			)
		} else {
			valueBox(
				value = "NONE",
				subtitle = "Upload ExpSet file",
				icon = icon("upload"),
				color = "red"
			)
		}
	})
	
	output$status_raw <- renderValueBox({
		if (!is.null(eset_raw())) {
			valueBox(
				value = ncol(eset_raw()),
				subtitle = "Raw Data Samples",
				icon = icon("vial"),
				color = "blue"
			)
		} else {
			valueBox(
				value = "—",
				subtitle = "No Raw Data",
				icon = icon("vial"),
				color = "yellow"
			)
		}
	})
	
	output$status_norm <- renderValueBox({
		if (!is.null(eset_norm())) {
			valueBox(
				value = ncol(eset_norm()),
				subtitle = "Normalized Data Samples",
				icon = icon("chart-line"),
				color = "purple"
			)
		} else {
			valueBox(
				value = "Optional",
				subtitle = "No Normalized Data",
				icon = icon("chart-line"),
				color = "light-blue"
			)
		}
	})
	
	output$status_ready <- renderValueBox({
		if (!is.null(eset_raw())) {
			valueBox(
				value = "READY",
				subtitle = "Ready to Denoise",
				icon = icon("check-circle"),
				color = "green"
			)
		} else {
			valueBox(
				value = "NOT READY",
				subtitle = "Select Data First",
				icon = icon("hourglass-half"),
				color = "orange"
			)
		}
	})
	
	# Raw data info
	output$raw_info <- renderPrint({
		req(eset_raw())
		
		cat("Assay: ", eset_raw_selected$name(), "\n")
		cat("Samples: ", ncol(eset_raw()), "\n")
		cat("Features: ", nrow(eset_raw()), "\n")
	})
	
	# Normalized data info
	output$norm_info <- renderPrint({
		if (!is.null(eset_norm())) {
			cat("Assay: ", eset_norm_selected$name(), "\n")
			cat("Samples: ", ncol(eset_norm()), "\n")
			cat("Features: ", nrow(eset_norm()), "\n")
		} else {
			cat("Not selected (optional)")
		}
	})
	
	# Complete ExpressionSet summary
	output$eset_summary <- renderPrint({
		req(eset_raw())
		
		cat("═══════════════════════════════════════════════════════════\n")
		
		# Show data source
		if (!is.null(uploaded_expset())) {
			cat("DATA SOURCE: Uploaded File\n")
		} else {
			cat("DATA SOURCE: Default ExpSet_list.rds\n")
		}
		cat("═══════════════════════════════════════════════════════════\n")
		
		cat("\nRAW/NETI DATA\n")
		cat("═══════════════════════════════════════════════════════════\n")
		cat("Selected Assay: ", eset_raw_selected$name(), "\n")
		cat("Dimensions: ", nrow(eset_raw()), " features × ", ncol(eset_raw()), " samples\n")
		cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_raw()), collapse = ", "), "\n")
		cat("Metadata Columns: ", paste(colnames(Biobase::pData(eset_raw())), collapse = ", "), "\n")
		
		# Check for required controls
		controls <- c("ZZ_con1", "ZZ_con2", "ZZ_con3", "ZZ_con4")
		present_controls <- controls[controls %in% rownames(eset_raw())]
		cat("Control Antigens: ", paste(present_controls, collapse = ", "), "\n")
		
		# Check for PN samples
		metadata <- Biobase::pData(eset_raw())
		if ("Sample_Group" %in% colnames(metadata)) {
			sample_groups <- table(metadata$Sample_Group)
			cat("\nSample Groups:\n")
			print(sample_groups)
		}
		
		if (!is.null(eset_norm())) {
			cat("\n═══════════════════════════════════════════════════════════\n")
			cat("NORMALIZED DATA\n")
			cat("═══════════════════════════════════════════════════════════\n")
			cat("Selected Assay: ", eset_norm_selected$name(), "\n")
			cat("Dimensions: ", nrow(eset_norm()), " features × ", ncol(eset_norm()), " samples\n")
			cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_norm()), collapse = ", "), "\n")
		}
	})
	
	# ===================================================================
	# DEBUG FUNCTIONS
	# ===================================================================
	
	# Debug button - Data Selection
	observeEvent(input$debug_data, {
		if (!interactive()) {
			showNotification(
				"Debug mode only works in interactive R sessions",
				type = "warning",
				duration = 5
			)
			return(NULL)
		}
		
		message("\n╔═══════════════════════════════════════════════════════════╗")
		message("║          🔍 DEBUG MODE - Data Selection                  ║")
		message("╚═══════════════════════════════════════════════════════════╝")
		message("\n📊 Session Info:")
		message("   User: DrGarnett")
		message("   Date: ", Sys.time())
		message("\n📍 Available objects:")
		message("   ✓ ExpSet_list()         : Full list of ExpressionSets")
		message("   ✓ uploaded_expset()     : Uploaded ExpSet data (if any)")
		message("   ✓ eset_raw()            : Selected raw/NetI ExpressionSet")
		message("   ✓ eset_norm()           : Selected normalized ExpressionSet")
		message("   ✓ eset_raw_selected     : Selection module reactive")
		message("   ✓ eset_norm_selected    : Selection module reactive")
		message("\n💡 Useful commands:")
		message("")
		message("   # List all ExpressionSets")
		message("   names(ExpSet_list())")
		message("")
		message("   # Check if using uploaded data")
		message("   is.null(uploaded_expset())")
		message("")
		message("   # Full diagnostics")
		message("   diagnose_ExpSet_list(ExpSet_list())")
		message("")
		message("   # Quick inspect selected data")
		message("   quick_inspect_eset(eset_raw())")
		message("   quick_inspect_eset(eset_norm())")
		message("")
		message("   # Check what's selected")
		message("   eset_raw_selected$name()")
		message("   eset_norm_selected$name()")
		message("")
		message("   # Manual inspection")
		message("   str(eset_raw())")
		message("   class(Biobase::exprs(eset_raw()))")
		message("   dim(Biobase::exprs(eset_raw()))")
		message("   head(Biobase::exprs(eset_raw())[, 1:5])")
		message("")
		message("   # Check metadata")
		message("   meta <- Biobase::pData(eset_raw())")
		message("   colnames(meta)")
		message("   table(meta$Sample_Group)")
		message("")
		message("   # Validate")
		message("   validate_denoise_inputs(eset_raw())")
		message("\n⌨️  Commands:")
		message("   c    Continue")
		message("   Q    Quit browser")
		message("   n    Next (step through)")
		message("═══════════════════════════════════════════════════════════\n")
		
		browser()
	})
	
	# Run diagnostics button
	observeEvent(input$run_diagnostics, {
		
		output$eset_summary <- renderPrint({
			cat("═══════════════════════════════════════════════════════════\n")
			cat("RUNNING FULL DIAGNOSTICS...\n")
			cat("═══════════════════════════════════════════════════════════\n")
			cat("User: DrGarnett\n")
			cat("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "\n")
			
			if (!is.null(uploaded_expset())) {
				cat("Data Source: UPLOADED FILE\n")
			} else {
				cat("Data Source: DEFAULT ExpSet_list.rds\n")
			}
			cat("═══════════════════════════════════════════════════════════\n\n")
			
			if (!is.null(ExpSet_list())) {
				cat("EXPRESSIONSET LIST STRUCTURE:\n")
				cat("───────────────────────────────────────────────────────────\n")
				diagnose_ExpSet_list(ExpSet_list())
				cat("\n")
			} else {
				cat("❌ ExpSet_list is NULL\n\n")
			}
			
			if (!is.null(eset_raw())) {
				cat("SELECTED RAW DATA INSPECTION:\n")
				cat("───────────────────────────────────────────────────────────\n")
				quick_inspect_eset(eset_raw())
				cat("\n")
			} else {
				cat("❌ eset_raw is NULL\n\n")
			}
			
			if (!is.null(eset_norm())) {
				cat("SELECTED NORMALIZED DATA INSPECTION:\n")
				cat("───────────────────────────────────────────────────────────\n")
				quick_inspect_eset(eset_norm())
			} else {
				cat("ℹ️  Normalized data not selected (optional)\n\n")
			}
			
			cat("\n═══════════════════════════════════════════════════════════\n")
			cat("DIAGNOSTICS COMPLETE\n")
			cat("═══════════════════════════════════════════════════════════\n")
		})
		
		showNotification(
			"✅ Diagnostics complete! Check the Complete Data Summary box above.",
			type = "message",
			duration = 5
		)
	})
	
	# Quick check button
	observeEvent(input$quick_check, {
		
		# Run quick validation
		result <- tryCatch({
			req(eset_raw())
			validate_denoise_inputs(eset_raw())
			TRUE
		}, error = function(e) {
			showNotification(
				paste("❌ Validation Error:", e$message),
				type = "error",
				duration = 10
			)
			FALSE
		})
		
		if (result) {
			showNotification(
				"✅ Quick check passed! Data structure looks good.",
				type = "message",
				duration = 5
			)
		}
	})
	
	# Navigate to denoiser tab
	observeEvent(input$goto_denoiser, {
		updateTabItems(session, "sidebar", "denoise")
		
		showNotification(
			"📊 Ready to run denoiser analysis!",
			type = "message",
			duration = 3
		)
	})
	
	# ===================================================================
	# DENOISER MODULE
	# ===================================================================
	
	# Call denoiser module
	denoiser_results <- mod_denoiser_server(
		"denoiser",
		eset_raw = eset_raw,
		eset_norm = eset_norm
	)
	
	# Log results when denoising completes
	observe({
		req(denoiser_results())
		
		results <- denoiser_results()
		
		if (!is.null(results$optimal_cutpoint)) {
			showNotification(
				HTML(paste0(
					"<strong>🎉 Denoising Complete!</strong><br>",
					"Optimal: ", results$optimal_cutpoint$PCs_removed, " PCs removed<br>",
					"Cutpoint: ", results$optimal_cutpoint$cutpoint, "<br>",
					"AAbs detected: ", results$optimal_cutpoint$N_unique_AAbs, "<br>",
					"TP:FP ratio: ", round(results$optimal_cutpoint$TP_FP_ratio, 2)
				)),
				type = "message",
				duration = 10
			)
		}
	})
	
	## Visualisation Module #####
	# In the server function, modify the denoiser call to capture results:
	
	# Call denoiser module and capture results
	# denoiser_results <- mod_denoiser_server( 
	# 	"denoiser",
	# 	eset_raw = eset_raw,
	# 	eset_norm = eset_norm
	# )
	
	# PC Visualizer module - pass denoiser results
	pc_viz_results <- mod_pc_visualizer_server(
		"pc_viz",
		eset_raw = eset_raw, 
		denoiser_results = denoiser_results  # <-- Pass the denoiser results here
	)
}

# Run app
message("\n═══════════════════════════════════════════════════════════")
message("🚀 Starting Denoiser App")
message("═══════════════════════════════════════════════════════════")
message("User: DrGarnett")
message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
message("═══════════════════════════════════════════════════════════\n")

shinyApp(ui, server)