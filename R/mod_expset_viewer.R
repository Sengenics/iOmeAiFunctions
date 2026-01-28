#' #' ExpressionSet Viewer Module - UI
#' #'
#' #' Comprehensive viewer for ExpressionSet objects with download functionality
#' #'
#' #' @param id Module namespace ID
#' #' @export
#' mod_expset_viewer_ui <- function(id) {
#' 	ns <- NS(id)
#' 	
#' 	tagList(
#' 		fluidRow(
#' 			box(
#' 				title = "ExpressionSet Selector",
#' 				width = 12,
#' 				status = "primary",
#' 				solidHeader = TRUE,
#' 				
#' 				fluidRow(
#' 					column(
#' 						width = 8,
#' 						uiOutput(ns("expset_selector_ui"))
#' 					),
#' 					column(
#' 						width = 4,
#' 						br(),
#' 						downloadButton(
#' 							ns("download_expset"),
#' 							"Download ExpSet (.rds)",
#' 							class = "btn-success",
#' 							style = "width: 100%;"
#' 						)
#' 					)
#' 				)
#' 			)
#' 		),
#' 		
#' 		fluidRow(
#' 			box(
#' 				title = "ExpressionSet Contents",
#' 				width = 12,
#' 				status = "info",
#' 				solidHeader = TRUE,
#' 				
#' 				tabsetPanel(
#' 					id = ns("expset_tabs"),
#' 					
#' 					tabPanel(
#' 						"Overview",
#' 						icon = icon("info-circle"),
#' 						br(),
#' 						verbatimTextOutput(ns("expset_overview"))
#' 					),
#' 					
#' 					tabPanel(
#' 						"assayData",
#' 						icon = icon("table"),
#' 						br(),
#' 						uiOutput(ns("assay_data_tabs_ui"))
#' 					),
#' 					
#' 					tabPanel(
#' 						"phenoData",
#' 						icon = icon("users"),
#' 						br(),
#' 						DT::dataTableOutput(ns("pheno_data_table"))
#' 					),
#' 					
#' 					tabPanel(
#' 						"featureData",
#' 						icon = icon("dna"),
#' 						br(),
#' 						DT::dataTableOutput(ns("feature_data_table"))
#' 					),
#' 					
#' 					tabPanel(
#' 						"experimentData",
#' 						icon = icon("flask"),
#' 						br(),
#' 						verbatimTextOutput(ns("experiment_data"))
#' 					),
#' 					
#' 					tabPanel(
#' 						"Parameters",
#' 						icon = icon("cog"),
#' 						br(),
#' 						uiOutput(ns("parameters_ui"))
#' 					)
#' 				)
#' 			)
#' 		)
#' 	)
#' }
#' 
#' #' ExpressionSet Viewer Module - Server
#' #'
#' #' @param id Module namespace ID
#' #' @param ExpSet_list Reactive returning named list of ExpressionSets
#' #' @export
#' mod_expset_viewer_server <- function(id, ExpSet_list) {
#' 	moduleServer(id, function(input, output, session) {
#' 		
#' 		# Render ExpSet selector
#' 		output$expset_selector_ui <- renderUI({
#' 			req(ExpSet_list())
#' 			
#' 			expsets <- ExpSet_list()
#' 			choices <- names(expsets)
#' 			
#' 			if (length(choices) == 0) {
#' 				return(
#' 					div(
#' 						class = "alert alert-warning",
#' 						icon("exclamation-triangle"),
#' 						strong(" No ExpressionSets available")
#' 					)
#' 				)
#' 			}
#' 			
#' 			# Determine default selection
#' 			preferred <- c('clinical_loess_normalised_PN', 'clinical_ExpSet', 'NetI_ExpSet')
#' 			selected <- preferred[preferred %in% choices][1]
#' 			if (is.na(selected)) selected <- choices[1]
#' 			
#' 			selectInput(
#' 				session$ns("selected_expset"),
#' 				"Select ExpressionSet:",
#' 				choices = choices,
#' 				selected = selected,
#' 				width = "100%"
#' 			)
#' 		})
#' 		
#' 		# Get selected ExpSet
#' 		current_expset <- reactive({
#' 			req(input$selected_expset)
#' 			req(ExpSet_list())
#' 			
#' 			expsets <- ExpSet_list()
#' 			
#' 			validate(
#' 				need(input$selected_expset %in% names(expsets),
#' 						 paste("ExpSet", input$selected_expset, "not found"))
#' 			)
#' 			
#' 			expsets[[input$selected_expset]]
#' 		})
#' 		
#' 		# Download handler
#' 		output$download_expset <- downloadHandler(
#' 			filename = function() {
#' 				req(input$selected_expset)
#' 				paste0(input$selected_expset, "_", Sys.Date(), ".rds")
#' 			},
#' 			content = function(file) {
#' 				req(current_expset())
#' 				saveRDS(current_expset(), file)
#' 			}
#' 		)
#' 		
#' 		# Overview tab
#' 		output$expset_overview <- renderPrint({
#' 			req(current_expset())
#' 			
#' 			ExpSet <- current_expset()
#' 			
#' 			cat("═══════════════════════════════════════════════════════════\n")
#' 			cat("EXPRESSIONSET OVERVIEW\n")
#' 			cat("═══════════════════════════════════════════════════════════\n\n")
#' 			
#' 			cat("Name:", input$selected_expset, "\n\n")
#' 			
#' 			cat("Dimensions:\n")
#' 			cat("  Features:", nrow(ExpSet), "\n")
#' 			cat("  Samples:", ncol(ExpSet), "\n\n")
#' 			
#' 			cat("assayData elements:\n")
#' 			assay_names <- Biobase::assayDataElementNames(ExpSet)
#' 			for (name in assay_names) {
#' 				cat("  -", name, "\n")
#' 			}
#' 			cat("\n")
#' 			
#' 			cat("phenoData columns (", ncol(Biobase::pData(ExpSet)), "):\n", sep = "")
#' 			cat("  ", paste(colnames(Biobase::pData(ExpSet)), collapse = ", "), "\n\n")
#' 			
#' 			cat("featureData columns (", ncol(Biobase::fData(ExpSet)), "):\n", sep = "")
#' 			if (ncol(Biobase::fData(ExpSet)) > 0) {
#' 				cat("  ", paste(colnames(Biobase::fData(ExpSet)), collapse = ", "), "\n")
#' 			} else {
#' 				cat("  None\n")
#' 			}
#' 			
#' 			cat("\n═══════════════════════════════════════════════════════════\n")
#' 		})
#' 		
#' 		# assayData tabs
#' 		output$assay_data_tabs_ui <- renderUI({
#' 			req(current_expset())
#' 			
#' 			assay_names <- Biobase::assayDataElementNames(current_expset())
#' 			
#' 			if (length(assay_names) == 0) {
#' 				return(p("No assay data available"))
#' 			}
#' 			
#' 			do.call(tabsetPanel, c(
#' 				list(id = session$ns("assay_subtabs")),
#' 				lapply(assay_names, function(name) {
#' 					tabPanel(
#' 						name,
#' 						br(),
#' 						DT::dataTableOutput(session$ns(paste0("assay_table_", name)))
#' 					)
#' 				})
#' 			))
#' 		})
#' 		
#' 		# Render each assayData matrix
#' 		observe({
#' 			req(current_expset())
#' 			
#' 			ExpSet <- current_expset()
#' 			assay_names <- Biobase::assayDataElementNames(ExpSet)
#' 			expset_name <- input$selected_expset
#' 			
#' 			lapply(assay_names, function(name) {
#' 				output[[paste0("assay_table_", name)]] <- DT::renderDataTable({
#' 					mat <- Biobase::assayDataElement(ExpSet, name)
#' 					df <- as.data.frame(mat)
#' 					df <- cbind(Feature = rownames(df), df)
#' 					
#' 					filename_base <- paste0(expset_name, "_assayData_", name)
#' 					
#' 					DT::datatable(
#' 						df,
#' 						extensions = 'Buttons',
#' 						options = list(
#' 							scrollX = TRUE,
#' 							pageLength = 25,
#' 							dom = 'Bfrtip',
#' 							buttons = list(
#' 								list(
#' 									extend = 'csv',
#' 									text = 'TSV',
#' 									filename = filename_base,
#' 									fieldSeparator = '\t',
#' 									extension = '.tsv',
#' 									exportOptions = list(
#' 										modifier = list(page = 'all', search = 'none'),
#' 										orthogonal = 'export'
#' 									)
#' 								),
#' 								list(
#' 									extend = 'excel',
#' 									filename = filename_base,
#' 									title = NULL,
#' 									messageTop = NULL,
#' 									exportOptions = list(
#' 										modifier = list(page = 'all', search = 'none'),
#' 										orthogonal = 'export'
#' 									)
#' 								),
#' 								list(
#' 									extend = 'copy',
#' 									exportOptions = list(
#' 										modifier = list(page = 'all', search = 'none'),
#' 										orthogonal = 'export'
#' 									)
#' 								)
#' 							)
#' 						),
#' 						rownames = FALSE
#' 					)
#' 				}, server = FALSE)
#' 			})
#' 		})
#' 		
#' 		# phenoData table
#' 		output$pheno_data_table <- DT::renderDataTable({
#' 			req(current_expset())
#' 			
#' 			pheno_df <- Biobase::pData(current_expset())
#' 			pheno_df <- cbind(Sample = rownames(pheno_df), pheno_df)
#' 			filename_base <- paste0(input$selected_expset, "_phenoData")
#' 			
#' 			DT::datatable(
#' 				pheno_df,
#' 				extensions = 'Buttons',
#' 				options = list(
#' 					scrollX = TRUE,
#' 					pageLength = 25,
#' 					dom = 'Bfrtip',
#' 					buttons = list(
#' 						list(
#' 							extend = 'csv',
#' 							text = 'TSV',
#' 							filename = filename_base,
#' 							fieldSeparator = '\t',
#' 							extension = '.tsv',
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						),
#' 						list(
#' 							extend = 'excel',
#' 							filename = filename_base,
#' 							title = NULL,
#' 							messageTop = NULL,
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						),
#' 						list(
#' 							extend = 'copy',
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						)
#' 					)
#' 				),
#' 				rownames = FALSE
#' 			)
#' 		}, server = FALSE)
#' 		
#' 		# featureData table
#' 		output$feature_data_table <- DT::renderDataTable({
#' 			req(current_expset())
#' 			
#' 			feature_df <- Biobase::fData(current_expset())
#' 			
#' 			if (nrow(feature_df) == 0 || ncol(feature_df) == 0) {
#' 				return(DT::datatable(data.frame(Message = "No feature data available")))
#' 			}
#' 			
#' 			feature_df <- cbind(Feature = rownames(feature_df), feature_df)
#' 			filename_base <- paste0(input$selected_expset, "_featureData")
#' 			
#' 			DT::datatable(
#' 				feature_df,
#' 				extensions = 'Buttons',
#' 				options = list(
#' 					scrollX = TRUE,
#' 					pageLength = 25,
#' 					dom = 'Bfrtip',
#' 					buttons = list(
#' 						list(
#' 							extend = 'csv',
#' 							text = 'TSV',
#' 							filename = filename_base,
#' 							fieldSeparator = '\t',
#' 							extension = '.tsv',
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						),
#' 						list(
#' 							extend = 'excel',
#' 							filename = filename_base,
#' 							title = NULL,
#' 							messageTop = NULL,
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						),
#' 						list(
#' 							extend = 'copy',
#' 							exportOptions = list(
#' 								modifier = list(page = 'all', search = 'none'),
#' 								orthogonal = 'export'
#' 							)
#' 						)
#' 					)
#' 				),
#' 				rownames = FALSE
#' 			)
#' 		}, server = FALSE)
#' 		
#' 		# experimentData
#' 		output$experiment_data <- renderPrint({
#' 			req(current_expset())
#' 			
#' 			exp_data <- Biobase::experimentData(current_expset())
#' 			print(exp_data)
#' 		})
#' 		
#' 		# Parameters tab
#' 		output$parameters_ui <- renderUI({
#' 			req(current_expset())
#' 			
#' 			exp_data <- Biobase::experimentData(current_expset())
#' 			params <- exp_data@other
#' 			
#' 			if (is.null(params) || length(params) == 0) {
#' 				return(
#' 					div(
#' 						class = "alert alert-info",
#' 						icon("info-circle"),
#' 						strong(" No parameters available")
#' 					)
#' 				)
#' 			}
#' 			
#' 			tagList(
#' 				h3("Pre-Processing Parameters"),
#' 				
#' 				if (! is.null(params$PreProcessingParams$dat)) {
#' 					box(
#' 						title = "Data Parameters",
#' 						width = 12,
#' 						status = "info",
#' 						solidHeader = TRUE,
#' 						collapsible = TRUE,
#' 						tags$div(
#' 							style = "max-height: 400px; overflow-y: auto;",
#' 							render_params_table(params$PreProcessingParams$dat)
#' 						)
#' 					)
#' 				},
#' 				
#' 				if (!c(params$PreProcessingParams$datCollate)) {
#' 					box(
#' 						title = "Data Collate Parameters",
#' 						width = 12,
#' 						status = "info",
#' 						solidHeader = TRUE,
#' 						collapsible = TRUE,
#' 						tags$div(
#' 							style = "max-height: 400px; overflow-y: auto;",
#' 							render_params_table(params$PreProcessingParams$datCollate)
#' 						)
#' 					)
#' 				},
#' 				
#' 				if (!is.null(params$AnalysisParams)) {
#' 					box(
#' 						title = "Analysis Parameters",
#' 						width = 12,
#' 						status = "info",
#' 						solidHeader = TRUE,
#' 						collapsible = TRUE,
#' 						tags$div(
#' 							style = "max-height: 400px; overflow-y: auto;",
#' 							render_params_table(params$AnalysisParams)
#' 						)
#' 					)
#' 				}
#' 			)
#' 		})
#' 		
#' 		# Helper function to render parameters as HTML table
#' 		render_params_table <- function(param_list) {
#' 			if (is.null(param_list) || length(param_list) == 0) {
#' 				return(p("No parameters"))
#' 			}
#' 			
#' 			param_df <- data.frame(
#' 				Parameter = names(param_list),
#' 				Value = sapply(param_list, function(x) {
#' 					if (is.null(x)) return("NULL")
#' 					if (length(x) > 1) return(paste(x, collapse = ", "))
#' 					if (is.list(x)) return(paste(names(x), collapse = ", "))
#' 					return(as.character(x))
#' 				}),
#' 				stringsAsFactors = FALSE
#' 			)
#' 			
#' 			tags$table(
#' 				class = "table table-striped table-hover",
#' 				tags$thead(
#' 					tags$tr(
#' 						tags$th("Parameter"),
#' 						tags$th("Value")
#' 					)
#' 				),
#' 				tags$tbody(
#' 					lapply(1:nrow(param_df), function(i) {
#' 						tags$tr(
#' 							tags$td(tags$strong(param_df$Parameter[i])),
#' 							tags$td(param_df$Value[i])
#' 						)
#' 					})
#' 				)
#' 			)
#' 		}
#' 		
#' 		# Return the current ExpSet for downstream use if needed
#' 		return(list(
#' 			current_expset = current_expset
#' 		))
#' 	})
#' }

#' ExpressionSet Viewer Module - UI
#'
#' Comprehensive viewer for ExpressionSet objects with download functionality
#' Now with integrated subsetting and transformation via mod_eset_selector_standalone
#'
#' @param id Module namespace ID
#' @param enable_subset Enable subsetting controls (default TRUE)
#' @param enable_transform Enable transformation controls (default TRUE)
#' @param debug Enable debug mode (default FALSE)
#' @export
mod_expset_viewer_ui <- function(id, 
																 enable_subset = TRUE,
																 enable_transform = TRUE,
																 debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# ExpressionSet Selector with Subset/Transform
		fluidRow(
			box(
				title = "ExpressionSet Selection & Processing",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				fluidRow(
					column(
						width = 10,
						mod_eset_selector_standalone_ui(
							ns("eset_selector"),
							show_summary = TRUE,
							show_subset = enable_subset,
							show_transform = enable_transform,
							show_info = TRUE,
							debug = debug
						)
					),
					column(
						width = 2,
						br(),
						downloadButton(
							ns("download_expset"),
							"Download .rds",
							class = "btn-success btn-block",
							icon = icon("download")
						),
						br(), br(),
						if (debug) {
							actionButton(
								ns("debug"),
								"Debug Viewer",
								class = "btn-warning btn-block btn-sm",
								icon = icon("bug")
							)
						}
					)
				)
			)
		),
		
		# ExpressionSet Contents Viewer
		fluidRow(
			box(
				title = "ExpressionSet Contents",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				
				tabsetPanel(
					id = ns("expset_tabs"),
					
					tabPanel(
						"Overview",
						icon = icon("info-circle"),
						br(),
						verbatimTextOutput(ns("expset_overview"))
					),
					
					tabPanel(
						"assayData",
						icon = icon("table"),
						br(),
						uiOutput(ns("assay_data_tabs_ui"))
					),
					
					tabPanel(
						"phenoData",
						icon = icon("users"),
						br(),
						DT::dataTableOutput(ns("pheno_data_table"))
					),
					
					tabPanel(
						"featureData",
						icon = icon("dna"),
						br(),
						DT::dataTableOutput(ns("feature_data_table"))
					),
					
					tabPanel(
						"experimentData",
						icon = icon("flask"),
						br(),
						verbatimTextOutput(ns("experiment_data"))
					),
					
					tabPanel(
						"Parameters",
						icon = icon("cog"),
						br(),
						uiOutput(ns("parameters_ui"))
					)
				)
			)
		)
	)
}

#' ExpressionSet Viewer Module - Server
#'
#' @param id Module namespace ID
#' @param ExpSet_list Reactive returning named list of ExpressionSets
#' @param default_selection Default ExpSet to select (optional)
#' @param enable_subset Enable subsetting (default TRUE)
#' @param enable_transform Enable transformation (default TRUE)
#' @param debug Enable debug mode (default FALSE)
#' @export
mod_expset_viewer_server <- function(id, 
																		 ExpSet_list,
																		 default_selection = NULL,
																		 enable_subset = TRUE,
																		 enable_transform = TRUE,
																		 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ExpressionSet Viewer Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • current_expset() - Final processed ExpressionSet")
				message("  • selector_module$eset_original() - Original data")
				message("  • selector_module$eset_subset() - After subsetting")
				message("  • selector_module$eset_name() - Selected assay name")
				message("\nExpSet_list contains: ", paste(names(ExpSet_list()), collapse = ", "))
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# ============================================
		# ExpressionSet Selector with Subset/Transform
		# ============================================
		selector_module <- mod_eset_selector_standalone_server(
			"eset_selector",
			ExpSet_list = ExpSet_list,
			default_selection = if (is.null(default_selection)) {
				reactive(NULL)
			} else if (is.reactive(default_selection)) {
				default_selection
			} else {
				reactive(default_selection)
			},
			enable_subset = enable_subset,
			enable_transform = enable_transform,
			debug = debug
		)
		
		# ============================================
		# Current ExpressionSet (final processed data)
		# ============================================
		current_expset <- reactive({
			req(selector_module$eset())
			selector_module$eset()
		})
		
		# ExpressionSet name for file naming
		current_expset_name <- reactive({
			req(selector_module$eset_name())
			selector_module$eset_name()
		})
		
		# ============================================
		# Download Handler
		# ============================================
		output$download_expset <- downloadHandler(
			filename = function() {
				name <- current_expset_name()
				paste0(name, "_", Sys.Date(), ".rds")
			},
			content = function(file) {
				req(current_expset())
				saveRDS(current_expset(), file)
			}
		)
		
		# ============================================
		# Overview Tab
		# ============================================
		output$expset_overview <- renderPrint({
			req(current_expset())
			
			ExpSet <- current_expset()
			name <- current_expset_name()
			
			cat("═══════════════════════════════════════════════════════════\n")
			cat("EXPRESSIONSET OVERVIEW\n")
			cat("═══════════════════════════════════════════════════════════\n\n")
			
			cat("Name: ", name, "\n\n")
			
			cat("Dimensions:\n")
			cat("  Features: ", nrow(ExpSet), "\n")
			cat("  Samples: ", ncol(ExpSet), "\n\n")
			
			cat("assayData elements:\n")
			assay_names <- Biobase::assayDataElementNames(ExpSet)
			for (name in assay_names) {
				cat("  - ", name, "\n")
			}
			cat("\n")
			
			cat("phenoData columns (", ncol(Biobase::pData(ExpSet)), "):\n", sep = "")
			cat("  ", paste(colnames(Biobase::pData(ExpSet)), collapse = ", "), "\n\n")
			
			cat("featureData columns (", ncol(Biobase::fData(ExpSet)), "):\n", sep = "")
			if (ncol(Biobase::fData(ExpSet)) > 0) {
				cat("  ", paste(colnames(Biobase::fData(ExpSet)), collapse = ", "), "\n")
			} else {
				cat("  None\n")
			}
			
			cat("\n═══════════════════════════════════════════════════════════\n")
		})
		
		# ============================================
		# assayData Tabs
		# ============================================
		output$assay_data_tabs_ui <- renderUI({
			req(current_expset())
			
			assay_names <- Biobase::assayDataElementNames(current_expset())
			
			if (length(assay_names) == 0) {
				return(p("No assay data available"))
			}
			
			do.call(tabsetPanel, c(
				list(id = session$ns("assay_subtabs")),
				lapply(assay_names, function(name) {
					tabPanel(
						name,
						br(),
						DT::dataTableOutput(session$ns(paste0("assay_table_", name)))
					)
				})
			))
		})
		
		# Render each assayData matrix
		observe({
			req(current_expset())
			
			ExpSet <- current_expset()
			assay_names <- Biobase::assayDataElementNames(ExpSet)
			expset_name <- current_expset_name()
			
			lapply(assay_names, function(name) {
				output[[paste0("assay_table_", name)]] <- DT::renderDataTable({
					mat <- Biobase::assayDataElement(ExpSet, name)
					df <- as.data.frame(mat)
					df <- cbind(Feature = rownames(df), df)
					
					filename_base <- paste0(expset_name, "_assayData_", name)
					
					DT::datatable(
						df,
						extensions = 'Buttons',
						options = list(
							scrollX = TRUE,
							pageLength = 25,
							dom = 'Bfrtip',
							buttons = list(
								list(
									extend = 'csv',
									text = 'TSV',
									filename = filename_base,
									fieldSeparator = '\t',
									extension = '.tsv',
									exportOptions = list(
										modifier = list(page = 'all', search = 'none'),
										orthogonal = 'export'
									)
								),
								list(
									extend = 'excel',
									filename = filename_base,
									title = NULL,
									messageTop = NULL,
									exportOptions = list(
										modifier = list(page = 'all', search = 'none'),
										orthogonal = 'export'
									)
								),
								list(
									extend = 'copy',
									exportOptions = list(
										modifier = list(page = 'all', search = 'none'),
										orthogonal = 'export'
									)
								)
							)
						),
						rownames = FALSE
					)
				}, server = FALSE)
			})
		})
		
		# ============================================
		# phenoData Table
		# ============================================
		output$pheno_data_table <- DT::renderDataTable({
			req(current_expset())
			
			pheno_df <- Biobase::pData(current_expset())
			pheno_df <- cbind(Sample = rownames(pheno_df), pheno_df)
			filename_base <- paste0(current_expset_name(), "_phenoData")
			
			DT::datatable(
				pheno_df,
				extensions = 'Buttons',
				options = list(
					scrollX = TRUE,
					pageLength = 25,
					dom = 'Bfrtip',
					buttons = list(
						list(
							extend = 'csv',
							text = 'TSV',
							filename = filename_base,
							fieldSeparator = '\t',
							extension = '.tsv',
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						),
						list(
							extend = 'excel',
							filename = filename_base,
							title = NULL,
							messageTop = NULL,
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						),
						list(
							extend = 'copy',
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						)
					)
				),
				rownames = FALSE
			)
		}, server = FALSE)
		
		# ============================================
		# featureData Table
		# ============================================
		output$feature_data_table <- DT::renderDataTable({
			req(current_expset())
			
			feature_df <- Biobase::fData(current_expset())
			
			if (nrow(feature_df) == 0 || ncol(feature_df) == 0) {
				return(DT::datatable(data.frame(Message = "No feature data available")))
			}
			
			feature_df <- cbind(Feature = rownames(feature_df), feature_df)
			filename_base <- paste0(current_expset_name(), "_featureData")
			
			DT::datatable(
				feature_df,
				extensions = 'Buttons',
				options = list(
					scrollX = TRUE,
					pageLength = 25,
					dom = 'Bfrtip',
					buttons = list(
						list(
							extend = 'csv',
							text = 'TSV',
							filename = filename_base,
							fieldSeparator = '\t',
							extension = '.tsv',
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						),
						list(
							extend = 'excel',
							filename = filename_base,
							title = NULL,
							messageTop = NULL,
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						),
						list(
							extend = 'copy',
							exportOptions = list(
								modifier = list(page = 'all', search = 'none'),
								orthogonal = 'export'
							)
						)
					)
				),
				rownames = FALSE
			)
		}, server = FALSE)
		
		# ============================================
		# experimentData
		# ============================================
		output$experiment_data <- renderPrint({
			req(current_expset())
			
			exp_data <- Biobase::experimentData(current_expset())
			print(exp_data)
		})
		
		# ============================================
		# Parameters Tab
		# ============================================
		output$parameters_ui <- renderUI({
			req(current_expset())
			
			exp_data <- Biobase::experimentData(current_expset())
			params <- exp_data@other
			
			if (is.null(params) || length(params) == 0) {
				return(
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" No parameters available")
					)
				)
			}
			
			tagList(
				h3("Pre-Processing Parameters"),
				
				if (! is.null(params$PreProcessingParams$dat)) {
					box(
						title = "Data Parameters",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						tags$div(
							style = "max-height: 400px; overflow-y: auto;",
							render_params_table(params$PreProcessingParams$dat)
						)
					)
				},
				
				if (!is.null(params$PreProcessingParams$datCollate)) {
					box(
						title = "Data Collate Parameters",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						tags$div(
							style = "max-height: 400px; overflow-y: auto;",
							render_params_table(params$PreProcessingParams$datCollate)
						)
					)
				},
				
				if (!is.null(params$AnalysisParams)) {
					box(
						title = "Analysis Parameters",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						tags$div(
							style = "max-height: 400px; overflow-y: auto;",
							render_params_table(params$AnalysisParams)
						)
					)
				}
			)
		})
		
		# Helper function to render parameters as HTML table
		render_params_table <- function(param_list) {
			if (is.null(param_list) || length(param_list) == 0) {
				return(p("No parameters"))
			}
			
			param_df <- data.frame(
				Parameter = names(param_list),
				Value = sapply(param_list, function(x) {
					if (is.null(x)) return("NULL")
					if (length(x) > 1) return(paste(x, collapse = ", "))
					if (is.list(x)) return(paste(names(x), collapse = ", "))
					return(as.character(x))
				}),
				stringsAsFactors = FALSE
			)
			
			tags$table(
				class = "table table-striped table-hover",
				tags$thead(
					tags$tr(
						tags$th("Parameter"),
						tags$th("Value")
					)
				),
				tags$tbody(
					lapply(1:nrow(param_df), function(i) {
						tags$tr(
							tags$td(tags$strong(param_df$Parameter[i])),
							tags$td(param_df$Value[i])
						)
					})
				)
			)
		}
		
		# ============================================
		# Return Values
		# ============================================
		return(list(
			current_expset = current_expset,
			current_expset_name = current_expset_name,
			# Provide access to intermediate stages
			eset_original = selector_module$eset_original,
			eset_subset = selector_module$eset_subset,
			# Provide access to selector module for advanced use
			selector_module = selector_module
		))
	})
}