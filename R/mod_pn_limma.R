# mod_pn_limma_v2.R ####
# Enhanced Limma Differential Expression Analysis Module

# UI Function ####

#' Enhanced Limma Analysis Module - UI
#'
#' @param id Character string. Namespace identifier.
#' @export
mod_pn_limma_ui <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			box(
				title = "Enhanced Limma Differential Expression Analysis",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				## ExpSet Info ####
				fluidRow(
					column(12, verbatimTextOutput(ns("eset_info")))
				),
				
				actionButton(ns('denoise_mod_debug'), 'Mod Debug', class = "btn-warning btn-sm"),
				
				## Analysis Mode Selector ####
				fluidRow(
					column(12,
								 radioButtons(
								 	ns("analysis_mode"),
								 	"Analysis Mode:",
								 	choices = c(
								 		"Basic (2-group comparison)" = "basic",
								 		"Advanced (Custom contrasts)" = "advanced",
								 		"AutoAb (Frequency analysis)" = "autoab"
								 	),
								 	selected = "basic",
								 	inline = TRUE
								 )
					)
				),
				
				hr(),
				
				## AssayData Selector ####
				fluidRow(
					column(
						width = 4,
						selectInput(
							ns("assay_data_select"),
							"Select AssayData Matrix:",
							choices = NULL,
							selected = NULL
						),
						helpText("Choose which matrix to use for analysis")
					),
					column(
						width = 8,
						uiOutput(ns("assay_info_ui"))
					)
				),
				
				hr(),
				
				## Feature Selection ####
				fluidRow(
					column(12,
								 checkboxInput(
								 	ns("use_feature_select"),
								 	"Pre-filter features before analysis",
								 	value = FALSE
								 )
					)
				),
				
				# conditionalPanel(
				# 	condition = paste0("input['", ns("use_feature_select"), "']"),
				# 	fluidRow(
				# 		column(6,
				# 					 selectInput(
				# 					 	ns("feature_filter_var"),
				# 					 	"Filter Variable:",
				# 					 	choices = NULL
				# 					 )
				# 		),
				# 		column(6,
				# 					 # numericInput(
				# 					 # 	ns("feature_filter_threshold"),
				# 					 # 	"Minimum Expression Threshold:",
				# 					 # 	value = 0,
				# 					 # 	min = 0
				# 					 # )
				# 					 uiOutput(ns("feature_filter_threshold_ui"))
				# 		)
				# 		
				# 	),
				# 	fluidRow(
				# 		column(12,
				# 					 verbatimTextOutput(ns("feature_filter_summary"))
				# 		)
				# 	)
				# ),
				conditionalPanel(
					condition = paste0("input['", ns("use_feature_select"), "']"),
					fluidRow(
						column(6,
									 selectInput(
									 	ns("feature_filter_var"),
									 	"Filter Variable:",
									 	choices = NULL
									 )
						),
						column(6,
									 uiOutput(ns("feature_filter_input_ui"))  # ✅ Dynamic input
						)
					),
					fluidRow(
						column(12,
									 verbatimTextOutput(ns("feature_filter_summary"))
						)
					)
				),
				
				hr(),
				
				## Basic Mode Parameters ####
				conditionalPanel(
					condition = paste0("input['", ns("analysis_mode"), "'] == 'basic'"),
					
					h4("Basic Comparison Setup"),
					
					fluidRow(
						column(3, 
									 selectInput(
									 	ns("variable"),
									 	"Primary Variable",
									 	choices = NULL
									 ),
									 tableOutput(ns("variable_summary"))
						),
						column(3,
									 checkboxInput(
									 	ns("continuous"),
									 	"Continuous Variable?",
									 	value = FALSE
									 ),
									 conditionalPanel(
									 	condition = paste0("!input['", ns("continuous"), "']"),
									 	selectInput(
									 		ns("class1"),
									 		"Class 1 (Positive)",
									 		choices = NULL
									 	)
									 ),
						# ),
						# column(3,
									 conditionalPanel(
									 	condition = paste0("!input['", ns("continuous"), "']"),
									 	selectInput(
									 		ns("class2"),
									 		"Class 2 (Negative)",
									 		choices = NULL
									 	)
									 ),
						# ),
						# column(3,
									 selectInput(
									 	ns("covariates"),
									 	"Covariates",
									 	choices = NULL,
									 	multiple = TRUE
									 ),
									 tableOutput(ns("covariates_summary"))
						)
					)
				),
				
				## Advanced Mode Parameters ####
				## Advanced Mode Parameters ####
				conditionalPanel(
					condition = paste0("input['", ns("analysis_mode"), "'] == 'advanced'"),
					
					h4("Custom Contrast Builder"),
					
					fluidRow(
						column(6,
									 selectInput(
									 	ns("contrast_variable"),
									 	"Primary Variable:",
									 	choices = NULL
									 )
						),
						column(6,
									 selectInput(
									 	ns("contrast_covariates"),
									 	"Covariates:",
									 	choices = NULL,
									 	multiple = TRUE
									 )
						)
					),
					
					hr(),
					
					h5("Build Contrast:"),
					
					fluidRow(
						column(5,
									 wellPanel(
									 	style = "background-color: #e8f5e9;",
									 	h5(strong("Positive Group (Numerator)")),
									 	selectInput(
									 		ns("contrast_group_pos"),
									 		"Select Group(s):",
									 		choices = NULL,
									 		multiple = TRUE
									 	),
									 	radioButtons(
									 		ns("contrast_pos_operator"),
									 		"If multiple groups:",
									 		choices = c("Average (A+B)/2" = "avg", "Sum A+B" = "sum"),
									 		selected = "avg",
									 		inline = TRUE
									 	)
									 )
						),
						column(2,
									 div(
									 	style = "text-align: center; margin-top: 80px;",
									 	h3(strong("−"))
									 )
						),
						column(5,
									 wellPanel(
									 	style = "background-color: #ffebee;",
									 	h5(strong("Negative Group (Denominator)")),
									 	selectInput(
									 		ns("contrast_group_neg"),
									 		"Select Group(s):",
									 		choices = NULL,
									 		multiple = TRUE
									 	),
									 	radioButtons(
									 		ns("contrast_neg_operator"),
									 		"If multiple groups:",
									 		choices = c("Average (C+D)/2" = "avg", "Sum C+D" = "sum"),
									 		selected = "avg",
									 		inline = TRUE
									 	)
									 )
						)
					),
					
					hr(),
					
					fluidRow(
						column(12,
									 strong("Generated Contrast:"),
									 helpText("Auto-generated from selections above. You can edit manually if needed.")
						)
					),
					
					fluidRow(
						column(10,
									 textInput(
									 	ns("user_contrast"),
									 	NULL,
									 	value = "",
									 	placeholder = "Select groups above to auto-generate, or type manually"
									 )
						),
						column(2,
									 br(),
									 actionButton(
									 	ns("validate_contrast"),
									 	"Validate",
									 	icon = icon("check-circle"),
									 	class = "btn-info btn-block"
									 )
						)
					),
					
					fluidRow(
						column(12,
									 verbatimTextOutput(ns("contrast_validation"))
						)
					)
				),
				
				## AutoAb Mode Parameters ####
				conditionalPanel(
					condition = paste0("input['", ns("analysis_mode"), "'] == 'autoab'"),
					
					h4("AutoAntibody Frequency Analysis"),
					
					fluidRow(
						column(4,
									 selectInput(
									 	ns("autoab_variable"),
									 	"Primary Variable:",
									 	choices = NULL
									 )
						),
						column(4,
									 selectInput(
									 	ns("autoab_group_pos"),
									 	"Positive Group:",
									 	choices = NULL
									 )
						),
						column(4,
									 selectInput(
									 	ns("autoab_group_neg"),
									 	"Negative Group:",
									 	choices = NULL
									 )
						)
					),
					
					fluidRow(
						column(4,
									 numericInput(
									 	ns("delta_cutoff"),
									 	"Delta % Cutoff:",
									 	value = 10,
									 	min = 0,
									 	max = 100,
									 	step = 5
									 ),
									 helpText("Minimum difference in % positive between groups")
						),
						column(4,
									 numericInput(
									 	ns("min_freq_threshold"),
									 	"Min Frequency (small group):",
									 	value = 0.15,
									 	min = 0,
									 	max = 1,
									 	step = 0.05
									 ),
									 helpText("For unequal group sizes")
						),
						column(4,
									 checkboxInput(
									 	ns("exclude_zeros"),
									 	"Exclude zeros from group means",
									 	value = TRUE
									 )
						)
					)
				),
				
				hr(),
				
				## Common Analysis Parameters ####
				h4("Analysis Parameters"),
				
				fluidRow(
					column(3,
								 numericInput(
								 	ns("fc_cutoff"),
								 	"Fold Change Cutoff",
								 	value = 1.75,
								 	min = 1.0,
								 	max = 5.0,
								 	step = 0.25
								 )
					),
					column(3,
								 numericInput(
								 	ns("p_val"),
								 	"P-value Cutoff",
								 	value = 0.05,
								 	min = 0.001,
								 	max = 0.1,
								 	step = 0.01
								 )
					),
					column(3,
								 selectInput(
								 	ns("add_anno"),
								 	"Additional Annotations",
								 	choices = NULL,
								 	multiple = TRUE
								 )
					),
					column(3,
								 radioButtons(
								 	ns("fc_direction"),
								 	"FC Direction",
								 	choices = c(
								 		"Up" = "up",
								 		"Down" = "down",
								 		"Both" = "both"
								 	),
								 	selected = "both"
								 )
					)
				),
				
				## Analysis Options ####
				fluidRow(
					column(3,
								 checkboxInput(
								 	ns("plot_violins"),
								 	"Generate Violin Plots",
								 	value = TRUE
								 )
					),
					column(3,
								 checkboxInput(
								 	ns("eb_trend"),
								 	"eBayes Trend",
								 	value = TRUE
								 )
					),
					column(3,
								 checkboxInput(
								 	ns("eb_robust"),
								 	"eBayes Robust",
								 	value = TRUE
								 )
					),
					column(3,
								 actionButton(
								 	ns("run_limma"),
								 	"Run Analysis",
								 	icon = icon("play"),
								 	class = "btn-primary btn-lg"
								 )
					)
				),
				
				hr(),
				
				## Results Section ####
				conditionalPanel(
					condition = paste0("output['", ns("limma_complete"), "']"),
					
					h4("Analysis Summary"),
					verbatimTextOutput(ns("limma_summary")),
					
					hr(),
					
					### Results Tabs ####
					tabsetPanel(
						id = ns("results_tabs"),
						
						tabPanel(
							"Design Matrix",
							br(),
							verbatimTextOutput(ns("design_matrix_display")),
							downloadButton(ns("download_design"), "Download Design Matrix", class = "btn-success")
						),
						
						tabPanel(
							"Top Results",
							br(),
							DT::dataTableOutput(ns("limma_top_table")),
							downloadButton(ns("download_toptable"), "Download TopTable", class = "btn-success")
						),
						
						tabPanel(
							"Volcano Plot",
							br(),
							plotOutput(ns("limma_volcano"), height = "600px")
						),
						
						tabPanel(
							"Heatmaps",
							br(),
							conditionalPanel(
								condition = paste0("output['", ns("has_heatmaps"), "']"),
								fluidRow(
									column(6,
												 h4("Heatmap - Manual Sort"),
												 plotOutput(ns("heatmap_manual"), height = "600px")
									),
									column(6,
												 h4("Heatmap - Row Centered"),
												 plotOutput(ns("heatmap_rc"), height = "600px")
									)
								),
								downloadButton(ns("download_heatmaps"), "Download Heatmaps", class = "btn-success")
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_heatmaps"), "']"),
								h5("No significant features found for heatmap generation")
							)
						),
						
						tabPanel(
							"ROC Analysis",
							br(),
							conditionalPanel(
								condition = paste0("output['", ns("has_roc"), "']"),
								DT::dataTableOutput(ns("roc_table")),
								downloadButton(ns("download_roc"), "Download ROC Results", class = "btn-success")
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_roc"), "']"),
								h5("ROC analysis only available for categorical variables with significant results")
							)
						),
						
						## AutoAb Frequency Tab ####
						tabPanel(
							"Frequency Analysis",
							br(),
							conditionalPanel(
								condition = paste0("output['", ns("has_frequency"), "']"),
								h4("AutoAb Frequency Metrics"),
								DT::dataTableOutput(ns("frequency_table")),
								downloadButton(ns("download_frequency"), "Download Frequency Results", class = "btn-success")
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_frequency"), "']"),
								h5("Frequency analysis only available in AutoAb mode")
							)
						),
						
						tabPanel(
							"Violin Plots",
							br(),
							conditionalPanel(
								condition = paste0("output['", ns("has_violins"), "']"),
								uiOutput(ns("violin_plots_ui")),
								downloadButton(ns("download_violins"), "Download Violin Plots", class = "btn-success")
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_violins"), "']"),
								h5("Violin plots only available with significant results")
							)
						)
					)
				)
			)
		)
	)
}

# Server Function ####

#' Enhanced Limma Analysis Module - Server
#'
#' @param id Character string. Namespace identifier.
#' @param eset Reactive or static. ExpressionSet object.
#' @param default_assay Character or reactive. Default assayData element (default "exprs").
#'
#' @return Reactive list containing limma results
#' @export
mod_pn_limma_server <- function(id, 
																eset,
																default_assay = "exprs") {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		## Debug Handler ####
		observeEvent(input$denoise_mod_debug, {
			browser()
		})
		
		## Normalize Inputs ####
		
		### Get ExpressionSet ####
		eset_current <- reactive({
			req(eset)
			if (is.reactive(eset)) {
				eset()
			} else {
				eset
			}
		})
		
		### Normalize Default Assay ####
		default_assay_reactive <- reactive({
			if (is.reactive(default_assay)) {
				default_assay()
			} else {
				default_assay
			}
		})
		
		## Reactive Values ####
		rv <- reactiveValues(
			limma_results = NULL,
			merged_results = NULL,
			frequency_results = NULL,
			heatmap_data = NULL,
			violin_plots = NULL,
			metadata_filtered = NULL,
			design_matrix = NULL,
			feature_select = NULL
		)
		
		## UI Population ####
		
		### Populate AssayData Choices ####
		observe({
			req(eset_current())
			
			assay_names <- Biobase::assayDataElementNames(eset_current())
			selected_assay <- default_assay_reactive()
			
			if (!selected_assay %in% assay_names) {
				selected_assay <- assay_names[1]
			}
			
			updateSelectInput(
				session,
				"assay_data_select",
				choices = assay_names,
				selected = selected_assay
			)
		})
		
		### Populate Metadata Choices ####
		# observe({
		# 	req(eset_current())
		# 	
		# 	metadata <- Biobase::pData(eset_current())
		# 	col_names <- colnames(metadata)
		# 	
		# 	# Basic mode
		# 	updateSelectInput(session, "variable", choices = col_names, selected = col_names[1])
		# 	updateSelectInput(session, "covariates", choices = col_names)
		# 	
		# 	# Advanced mode
		# 	updateSelectInput(session, "contrast_variable", choices = col_names, selected = col_names[1])
		# 	updateSelectInput(session, "contrast_covariates", choices = col_names)
		# 	
		# 	# AutoAb mode
		# 	updateSelectInput(session, "autoab_variable", choices = col_names, selected = col_names[1])
		# 	
		# 	# Common
		# 	updateSelectInput(session, "add_anno", choices = col_names)
		# 	updateSelectInput(session, "feature_filter_var", choices = col_names)
		# })
		
		
		### Feature Filter Threshold UI (dynamic label) ####
		output$feature_filter_threshold_ui <- renderUI({
			req(input$feature_filter_var)
			
			if (input$feature_filter_var == "CV") {
				numericInput(
					ns("feature_filter_threshold"),
					"Maximum CV (%):",
					value = 50,
					min = 0,
					max = 200
				)
			} else if (input$feature_filter_var %in% c("Mean Expression", "Max Expression")) {
				numericInput(
					ns("feature_filter_threshold"),
					paste("Minimum", input$feature_filter_var, ":"),
					value = 0,
					min = 0
				)
			} else {
				# For fData columns
				numericInput(
					ns("feature_filter_threshold"),
					"Threshold:",
					value = 0
				)
			}
		})
		
		### Populate Metadata Choices ####
		observe({
			req(eset_current())
			
			metadata <- Biobase::pData(eset_current())
			col_names <- colnames(metadata)
			
			# Basic mode
			updateSelectInput(session, "variable", choices = col_names, selected = "Labels")
			updateSelectInput(session, "covariates", choices = col_names)
			
			# Advanced mode
			updateSelectInput(session, "contrast_variable", choices = col_names, selected = col_names[1])
			updateSelectInput(session, "contrast_covariates", choices = col_names)
			
			# AutoAb mode
			updateSelectInput(session, "autoab_variable", choices = col_names, selected = col_names[1])
			
			# Common
			updateSelectInput(session, "add_anno", choices = col_names)
			
			# ✅ FEATURE FILTER - use fData, not pData
			fdata <- Biobase::fData(eset_current())
			if (ncol(fdata) > 0) {
				fdata_cols <- c("Mean Expression", "Max Expression", "CV", colnames(fdata))
			} else {
				fdata_cols <- c("Mean Expression", "Max Expression", "CV")
			}
			updateSelectInput(session, "feature_filter_var", choices = fdata_cols, selected = "Mean Expression")
		})
		
		### Update Class Choices (Basic Mode) ####
		observe({
			req(eset_current(), input$variable)
			
			if (!input$continuous) {
				metadata <- Biobase::pData(eset_current())
				
				if (!input$variable %in% colnames(metadata)) {
					return()
				}
				
				unique_vals <- unique(metadata[[input$variable]])
				
				updateSelectInput(session, "class1", choices = unique_vals, selected = unique_vals[1])
				updateSelectInput(session, "class2", choices = unique_vals, selected = unique_vals[min(2, length(unique_vals))])
			}
		})
		
		### Update AutoAb Group Choices ####
		observe({
			req(eset_current(), input$autoab_variable)
			
			metadata <- Biobase::pData(eset_current())
			
			if (!input$autoab_variable %in% colnames(metadata)) {
				return()
			}
			
			unique_vals <- unique(metadata[[input$autoab_variable]])
			
			updateSelectInput(session, "autoab_group_pos", choices = unique_vals, selected = unique_vals[1])
			updateSelectInput(session, "autoab_group_neg", choices = unique_vals, selected = unique_vals[min(2, length(unique_vals))])
		})
		
		## Feature Selection ####
		
		### Calculate Feature Filter ####
		# observe({
		# 	req(eset_current(), input$use_feature_select)
		# 	
		# 	if (!input$use_feature_select) {
		# 		rv$feature_select <- NULL
		# 		return()
		# 	}
		# 	
		# 	req(input$assay_data_select, input$feature_filter_threshold)
		# 	
		# 	exprs_data <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
		# 	
		# 	# Simple threshold filter (can be enhanced)
		# 	feature_means <- rowMeans(exprs_data, na.rm = TRUE)
		# 	rv$feature_select <- rownames(exprs_data)[feature_means >= input$feature_filter_threshold]
		# })
		
		### Calculate Feature Filter ####
		
		### Calculate Feature Filter ####
		observe({
			req(eset_current(), input$use_feature_select)
			
			if (!input$use_feature_select) {
				rv$feature_select <- NULL
				return()
			}
			
			req(input$assay_data_select, input$feature_filter_var)
			
			exprs_data <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
			fdata <- Biobase::fData(eset_current())
			
			filter_var <- input$feature_filter_var
			
			# Calculate filter based on variable type
			if (filter_var == "Mean Expression") {
				req(input$feature_filter_threshold)
				filter_values <- rowMeans(exprs_data, na.rm = TRUE)
				keep_features <- filter_values >= input$feature_filter_threshold
				
			} else if (filter_var == "Max Expression") {
				req(input$feature_filter_threshold)
				filter_values <- apply(exprs_data, 1, max, na.rm = TRUE)
				keep_features <- filter_values >= input$feature_filter_threshold
				
			} else if (filter_var == "CV") {
				req(input$feature_filter_threshold)
				filter_values <- apply(exprs_data, 1, function(x) {
					sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
				})
				keep_features <- filter_values <= input$feature_filter_threshold
				
			} else if (filter_var %in% colnames(fdata)) {
				# fData column
				filter_values <- fdata[[filter_var]]
				
				if (is.numeric(filter_values)) {
					# Numeric fData column
					req(input$feature_filter_threshold)
					keep_features <- filter_values >= input$feature_filter_threshold
				} else {
					# Categorical fData column
					req(input$feature_filter_levels)
					keep_features <- as.character(filter_values) %in% input$feature_filter_levels
				}
			} else {
				warning("Unknown filter variable: ", filter_var)
				rv$feature_select <- NULL
				return()
			}
			
			rv$feature_select <- rownames(exprs_data)[keep_features & !is.na(keep_features)]
		})
		# observe({
		# 	req(eset_current(), input$use_feature_select)
		# 	
		# 	if (!input$use_feature_select) {
		# 		rv$feature_select <- NULL
		# 		return()
		# 	}
		# 	
		# 	req(input$assay_data_select, input$feature_filter_var, input$feature_filter_threshold)
		# 	
		# 	exprs_data <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
		# 	fdata <- Biobase::fData(eset_current())
		# 	
		# 	filter_var <- input$feature_filter_var
		# 	threshold <- input$feature_filter_threshold
		# 	
		# 	# Calculate filter metric based on selection
		# 	if (filter_var == "Mean Expression") {
		# 		filter_values <- rowMeans(exprs_data, na.rm = TRUE)
		# 		keep_features <- filter_values >= threshold
		# 		
		# 	} else if (filter_var == "Max Expression") {
		# 		filter_values <- apply(exprs_data, 1, max, na.rm = TRUE)
		# 		keep_features <- filter_values >= threshold
		# 		
		# 	} else if (filter_var == "CV") {
		# 		filter_values <- apply(exprs_data, 1, function(x) {
		# 			sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
		# 		})
		# 		keep_features <- filter_values <= threshold  # ✅ CV: LOWER is better
		# 		
		# 	} else if (filter_var %in% colnames(fdata)) {
		# 		# ✅ Use fData column
		# 		filter_values <- fdata[[filter_var]]
		# 		
		# 		if (is.numeric(filter_values)) {
		# 			keep_features <- filter_values >= threshold
		# 		} else {
		# 			# For categorical fData (e.g., "keep"/"remove")
		# 			# Assume threshold is not used, just keep non-NA
		# 			keep_features <- !is.na(filter_values)
		# 		}
		# 	} else {
		# 		warning("Unknown filter variable: ", filter_var)
		# 		rv$feature_select <- NULL
		# 		return()
		# 	}
		# 	
		# 	rv$feature_select <- rownames(exprs_data)[keep_features & !is.na(keep_features)]
		# })
		
		### Feature Filter Summary ####
		# output$feature_filter_summary <- renderPrint({
		# 	req(rv$feature_select)
		# 	
		# 	total_features <- nrow(Biobase::assayDataElement(eset_current(), input$assay_data_select))
		# 	filtered_features <- length(rv$feature_select)
		# 	
		# 	cat("Feature Filter Summary\n")
		# 	cat("======================\n")
		# 	cat("Total features:", total_features, "\n")
		# 	cat("Filtered features:", filtered_features, "\n")
		# 	cat("Removed:", total_features - filtered_features, "\n")
		# 	cat("Retained:", round(filtered_features / total_features * 100, 1), "%\n")
		# })
		
		### Dynamic Feature Filter Input (Numeric vs Categorical) ####
		output$feature_filter_input_ui <- renderUI({
			req(eset_current(), input$feature_filter_var, input$assay_data_select)
			
			filter_var <- input$feature_filter_var
			exprs_data <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
			fdata <- Biobase::fData(eset_current())
			
			# Determine if variable is numeric or categorical
			is_numeric <- FALSE
			var_data <- NULL
			
			if (filter_var == "Mean Expression") {
				is_numeric <- TRUE
				var_data <- rowMeans(exprs_data, na.rm = TRUE)
			} else if (filter_var == "Max Expression") {
				is_numeric <- TRUE
				var_data <- apply(exprs_data, 1, max, na.rm = TRUE)
			} else if (filter_var == "CV") {
				is_numeric <- TRUE
				var_data <- apply(exprs_data, 1, function(x) {
					sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
				})
			} else if (filter_var %in% colnames(fdata)) {
				var_data <- fdata[[filter_var]]
				is_numeric <- is.numeric(var_data)
			}
			
			# Generate appropriate input
			if (is_numeric) {
				# Numeric threshold input
				if (filter_var == "CV") {
					numericInput(
						ns("feature_filter_threshold"),
						"Maximum CV (%):",
						value = 50,
						min = 0,
						max = 200
					)
				} else {
					# Show range of values to help user
					val_range <- range(var_data, na.rm = TRUE)
					numericInput(
						ns("feature_filter_threshold"),
						HTML(paste0("Minimum ", filter_var, ":<br><small>Range: ", 
												round(val_range[1], 2), " - ", round(val_range[2], 2), "</small>")),
						value = round(quantile(var_data, 0.25, na.rm = TRUE), 2),  # Default: 25th percentile
						min = val_range[1],
						max = val_range[2],
						step = (val_range[2] - val_range[1]) / 100
					)
				}
			} else {
				# Categorical selection input
				unique_levels <- unique(as.character(var_data))
				unique_levels <- unique_levels[!is.na(unique_levels)]
				unique_levels <- sort(unique_levels)
				
				# Count features per level
				level_counts <- table(var_data)
				choices_with_counts <- setNames(
					unique_levels,
					paste0(unique_levels, " (n=", level_counts[unique_levels], ")")
				)
				
				selectInput(
					ns("feature_filter_levels"),
					HTML(paste0("Keep features with ", filter_var, ":")),
					choices = choices_with_counts,
					selected = unique_levels,  # Default: keep all
					multiple = TRUE
				)
			}
		})
		### Feature Filter Summary ####
		# output$feature_filter_summary <- renderPrint({
		# 	req(rv$feature_select, eset_current(), input$feature_filter_var)
		# 	
		# 	total_features <- nrow(Biobase::assayDataElement(eset_current(), input$assay_data_select))
		# 	filtered_features <- length(rv$feature_select)
		# 	
		# 	cat("Feature Filter Summary\n")
		# 	cat("======================\n")
		# 	cat("Filter Variable:", input$feature_filter_var, "\n")
		# 	cat("Threshold:", input$feature_filter_threshold, "\n")
		# 	
		# 	if (input$feature_filter_var == "CV") {
		# 		cat("(Keeping features with CV <=", input$feature_filter_threshold, "%)\n\n")
		# 	} else {
		# 		cat("(Keeping features >=", input$feature_filter_threshold, ")\n\n")
		# 	}
		# 	
		# 	cat("Total features:", total_features, "\n")
		# 	cat("Filtered features:", filtered_features, "\n")
		# 	cat("Removed:", total_features - filtered_features, "\n")
		# 	cat("Retained:", round(filtered_features / total_features * 100, 1), "%\n")
		# })
		
		### Feature Filter Summary ####
		output$feature_filter_summary <- renderPrint({
			req(rv$feature_select, eset_current(), input$feature_filter_var, input$assay_data_select)
			
			exprs_data <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
			fdata <- Biobase::fData(eset_current())
			
			total_features <- nrow(exprs_data)
			filtered_features <- length(rv$feature_select)
			
			cat("Feature Filter Summary\n")
			cat("======================\n")
			cat("Filter Variable:", input$feature_filter_var, "\n")
			
			# Show filter criteria
			filter_var <- input$feature_filter_var
			
			if (filter_var %in% c("Mean Expression", "Max Expression")) {
				cat("Threshold: >=", input$feature_filter_threshold, "\n")
			} else if (filter_var == "CV") {
				cat("Threshold: <=", input$feature_filter_threshold, "%\n")
			} else if (filter_var %in% colnames(fdata)) {
				var_data <- fdata[[filter_var]]
				if (is.numeric(var_data)) {
					cat("Threshold: >=", input$feature_filter_threshold, "\n")
				} else {
					cat("Keeping levels:", paste(input$feature_filter_levels, collapse = ", "), "\n")
				}
			}
			
			cat("\n")
			cat("Total features:", total_features, "\n")
			cat("Kept:", filtered_features, "\n")
			cat("Removed:", total_features - filtered_features, "\n")
			cat("Retained:", round(filtered_features / total_features * 100, 1), "%\n")
			
			# Show distribution if categorical
			if (filter_var %in% colnames(fdata)) {
				var_data <- fdata[[filter_var]]
				if (!is.numeric(var_data)) {
					cat("\nDistribution before filtering:\n")
					print(table(var_data, useNA = "ifany"))
					cat("\nDistribution after filtering:\n")
					print(table(var_data[rownames(exprs_data) %in% rv$feature_select], useNA = "ifany"))
				}
			}
		})
		
		
		### Update Contrast Group Choices ####
		observe({
			req(eset_current(), input$contrast_variable)
			
			metadata <- Biobase::pData(eset_current())
			
			if (!input$contrast_variable %in% colnames(metadata)) {
				return()
			}
			
			# Make names syntactically valid for preview
			var_levels <- unique(metadata[[input$contrast_variable]])
			var_levels <- var_levels[!is.na(var_levels)]
			var_levels_clean <- make.names(var_levels)
			
			# Show original names in UI, but store clean names
			choices <- setNames(var_levels_clean, var_levels)
			
			updateSelectInput(session, "contrast_group_pos", choices = choices)
			updateSelectInput(session, "contrast_group_neg", choices = choices)
		})
		
		### Auto-Generate Contrast String ####
		observe({
			# Only auto-generate if user hasn't manually edited
			# (or if groups change)
			
			req(input$contrast_variable)
			
			pos_groups <- input$contrast_group_pos
			neg_groups <- input$contrast_group_neg
			
			# Skip if no groups selected
			if (is.null(pos_groups) || is.null(neg_groups)) {
				return()
			}
			
			if (length(pos_groups) == 0 || length(neg_groups) == 0) {
				return()
			}
			
			# Build positive side
			if (length(pos_groups) == 1) {
				pos_string <- pos_groups[1]
			} else {
				if (input$contrast_pos_operator == "avg") {
					pos_string <- paste0("(", paste(pos_groups, collapse = "+"), ")/", length(pos_groups))
				} else {
					pos_string <- paste0("(", paste(pos_groups, collapse = "+"), ")")
				}
			}
			
			# Build negative side
			if (length(neg_groups) == 1) {
				neg_string <- neg_groups[1]
			} else {
				if (input$contrast_neg_operator == "avg") {
					neg_string <- paste0("(", paste(neg_groups, collapse = "+"), ")/", length(neg_groups))
				} else {
					neg_string <- paste0("(", paste(neg_groups, collapse = "+"), ")")
				}
			}
			
			# Combine
			contrast_string <- paste0(pos_string, " - ", neg_string)
			
			# Update text input (only if different to avoid infinite loop)
			if (!identical(input$user_contrast, contrast_string)) {
				updateTextInput(session, "user_contrast", value = contrast_string)
			}
		})
		
		## Contrast Validation ####
		## Contrast Validation ####
		observeEvent(input$validate_contrast, {
			req(input$contrast_variable, input$user_contrast)
			
			tryCatch({
				metadata <- Biobase::pData(eset_current())
				variable <- input$contrast_variable
				
				# Make names syntactically valid
				meta_clean <- metadata
				if (!is.numeric(meta_clean[[variable]])) {
					meta_clean[[variable]][!is.na(meta_clean[[variable]])] <- 
						make.names(meta_clean[[variable]][!is.na(meta_clean[[variable]])])
					meta_clean[[variable]] <- as.factor(meta_clean[[variable]])
				}
				
				# Build design
				if (!is.null(input$contrast_covariates) && length(input$contrast_covariates) > 0) {
					covariates <- input$contrast_covariates
					for (cov in covariates) {
						if (!is.numeric(meta_clean[[cov]])) {
							meta_clean[[cov]][!is.na(meta_clean[[cov]])] <- 
								make.names(meta_clean[[cov]][!is.na(meta_clean[[cov]])])
							meta_clean[[cov]] <- as.factor(meta_clean[[cov]])
						}
					}
					design_formula <- as.formula(paste0("~ 0 + ", paste(c(variable, covariates), collapse = " + ")))
				} else {
					design_formula <- as.formula(paste0("~ 0 + ", variable))
				}
				
				design <- model.matrix(design_formula, data = meta_clean)
				
				# Clean design column names
				colnames(design) <- gsub(paste0("^", variable), "", colnames(design))
				if (!is.null(input$contrast_covariates) && length(input$contrast_covariates) > 0) {
					for (cov in input$contrast_covariates) {
						colnames(design) <- gsub(paste0("^", cov), "", colnames(design))
					}
				}
				
				# Try to build contrast
				user_contrast <- makeContrasts(contrasts = input$user_contrast, levels = design)
				
				# Calculate sample sizes
				pos_groups <- names(user_contrast[user_contrast > 0, ])
				neg_groups <- names(user_contrast[user_contrast < 0, ])
				
				pos_samples <- sum(rowSums(design[, pos_groups, drop = FALSE]) > 0)
				neg_samples <- sum(rowSums(design[, neg_groups, drop = FALSE]) > 0)
				
				output$contrast_validation <- renderPrint({
					cat("✅ Contrast is VALID!\n\n")
					cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
					cat("Contrast String:\n")
					cat("  ", input$user_contrast, "\n\n")
					
					cat("Interpretation:\n")
					cat("  Positive group(s):", paste(pos_groups, collapse = ", "), "\n")
					cat("  Negative group(s):", paste(neg_groups, collapse = ", "), "\n\n")
					
					cat("Sample Counts:\n")
					cat("  Positive group:", pos_samples, "samples\n")
					cat("  Negative group:", neg_samples, "samples\n\n")
					
					cat("Contrast Matrix:\n")
					print(user_contrast)
					cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
				})
				
				showNotification("✅ Contrast validated successfully!", type = "message", duration = 5)
				
			}, error = function(e) {
				output$contrast_validation <- renderPrint({
					cat("❌ Contrast validation FAILED\n\n")
					cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━��━\n")
					cat("Error:\n")
					cat("  ", e$message, "\n\n")
					
					cat("Your contrast string:\n")
					cat("  ", input$user_contrast, "\n\n")
					
					cat("Available levels:\n")
					metadata <- Biobase::pData(eset_current())
					var_levels <- unique(metadata[[input$contrast_variable]])
					var_levels_clean <- make.names(var_levels[!is.na(var_levels)])
					cat("  ", paste(var_levels_clean, collapse = ", "), "\n\n")
					
					cat("Tips:\n")
					cat("  • Level names must be syntactically valid (use make.names)\n")
					cat("  • Use '+' to combine groups: A+B\n")
					cat("  • Use '/' to average: (A+B)/2\n")
					cat("  • Use '-' to subtract: (A+B)/2 - C\n")
					cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
				})
				
				showNotification(
					paste("❌ Contrast validation failed:", e$message),
					type = "error",
					duration = 10
				)
			})
		})
		
		## Outputs ####
		
		### ExpSet Info ####
		output$eset_info <- renderPrint({
			req(eset_current())
			
			cat("AssayData elements: ", paste(Biobase::assayDataElementNames(eset_current()), collapse = ", "), "\n")
			cat("Samples: ", ncol(eset_current()), "\n")
			cat("Features: ", nrow(eset_current()), "\n")
		})
		
		### Variable Summary ####
		output$variable_summary <- renderTable({
			req(eset_current())
			
			var_name <- switch(input$analysis_mode,
												 "basic" = input$variable,
												 "advanced" = input$contrast_variable,
												 "autoab" = input$autoab_variable
			)
			
			req(var_name)
			
			metadata <- Biobase::pData(eset_current())
			
			if (var_name %in% colnames(metadata)) {
				var_data <- metadata[[var_name]]
				
				if (is.numeric(var_data)) {
					data.frame(
						Statistic = c("Min", "Max", "Mean", "NAs"),
						Value = c(
							round(min(var_data, na.rm = TRUE), 2),
							round(max(var_data, na.rm = TRUE), 2),
							round(mean(var_data, na.rm = TRUE), 2),
							sum(is.na(var_data))
						)
					)
				} else {
					tbl <- table(var_data, useNA = "ifany")
					data.frame(
						Group = names(tbl),
						Count = as.numeric(tbl)
					)
				}
			}
		}, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "100%", align = "lr")
		
		### Covariates Summary ####
		output$covariates_summary <- renderTable({
			req(eset_current())
			
			covs <- if (input$analysis_mode == "basic") {
				input$covariates
			} else {
				input$contrast_covariates
			}
			
			if (is.null(covs) || length(covs) == 0) {
				return(NULL)
			}
			
			metadata <- Biobase::pData(eset_current())
			
			summary_list <- lapply(covs, function(cov) {
				if (cov %in% colnames(metadata)) {
					var_data <- metadata[[cov]]
					
					if (is.numeric(var_data)) {
						data.frame(
							Covariate = cov,
							Group = c("Range", "NAs"),
							Count = c(
								paste0(round(min(var_data, na.rm = TRUE), 1), "-", round(max(var_data, na.rm = TRUE), 1)),
								as.character(sum(is.na(var_data)))
							)
						)
					} else {
						tbl <- table(var_data, useNA = "ifany")
						data.frame(
							Covariate = cov,
							Group = names(tbl),
							Count = as.numeric(tbl)
						)
					}
				}
			})
			
			do.call(rbind, summary_list)
			
		}, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "100%", align = "llr")
		
		### AssayData Info UI ####
		output$assay_info_ui <- renderUI({
			req(eset_current(), input$assay_data_select)
			
			assay_mat <- Biobase::assayDataElement(eset_current(), input$assay_data_select)
			
			div(
				style = "background-color: #f0f0f0; padding: 10px; border-radius: 4px; margin-top: 25px;",
				tags$strong(paste0("Selected: ", input$assay_data_select)),
				br(),
				paste0("Dimensions: ", nrow(assay_mat), " features × ", ncol(assay_mat), " samples")
			)
		})
		
		## Main Analysis Logic ####
		
		observeEvent(input$run_limma, {
			req(eset_current(), input$assay_data_select, input$analysis_mode)
			
			showNotification("Running limma analysis...", type = "message", duration = NULL, id = "limma_running")
			
			tryCatch({
				# Route to appropriate analysis function
				if (input$analysis_mode == "basic") {
					run_basic_limma()
				} else if (input$analysis_mode == "advanced") {
					run_advanced_limma()
				} else if (input$analysis_mode == "autoab") {
					run_autoab_limma()
				}
				
				removeNotification("limma_running")
				
				showNotification(
					"✅ Analysis complete!",
					type = "message",
					duration = 5
				)
				
			}, error = function(e) {
				removeNotification("limma_running")
				showNotification(
					paste("❌ Analysis failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
		## Analysis Functions ####
		
		### Run Basic Limma ####
		run_basic_limma <- function() {
			# Get data
			eset_temp <- eset_current()
			metadata <- Biobase::pData(eset_temp)
			exprs_data <- Biobase::assayDataElement(eset_temp, input$assay_data_select)
			
			# Apply feature selection
			if (!is.null(rv$feature_select)) {
				exprs_data <- exprs_data[rv$feature_select, , drop = FALSE]
			}
			
			# Get parameters
			variable <- input$variable
			covariates <- input$covariates
			continuous <- input$continuous
			fc_cutoff <- input$fc_cutoff
			p_val <- input$p_val
			fc_direction <- input$fc_direction
			
			class1 <- if (!continuous) input$class1 else NULL
			class2 <- if (!continuous) input$class2 else NULL
			
			# Make names syntactically valid
			if (!is.numeric(metadata[[variable]])) {
				metadata[[variable]][!is.na(metadata[[variable]])] <- make.names(metadata[[variable]][!is.na(metadata[[variable]])])
				metadata[[variable]] <- as.factor(metadata[[variable]])
			}
			
			if (!is.null(covariates) && length(covariates) > 0) {
				for (covariate in covariates) {
					if (!is.numeric(metadata[[covariate]])) {
						metadata[[covariate]][!is.na(metadata[[covariate]])] <- make.names(metadata[[covariate]][!is.na(metadata[[covariate]])])
						metadata[[covariate]] <- as.factor(metadata[[covariate]])
					}
				}
			}
			
			# Remove samples with NA values
			covariate_cols <- c(variable, covariates)
			if (length(covariate_cols) > 1) {
				na_rows <- which(rowSums(is.na(metadata[, covariate_cols])) > 0)
			} else {
				na_rows <- which(is.na(metadata[, covariate_cols]))
			}
			
			if (length(na_rows) > 0) {
				warning(paste("Removed", length(na_rows), "samples with NA values."))
				metadata <- metadata[-na_rows, ]
				exprs_data <- exprs_data[, rownames(metadata)]
			}
			
			# Validate sample count
			if (nrow(metadata) < 3) {
				stop(paste("Not enough samples after removing NAs:", nrow(metadata), "samples remaining. Need at least 3."))
			}
			
			# Build model
			if (!continuous) {
				# Categorical
				if (!is.null(covariates) && length(covariates) > 0) {
					design_formula <- as.formula(paste0("~ 0 + ", paste(c(variable, covariates), collapse = " + ")))
				} else {
					design_formula <- as.formula(paste0("~ 0 + ", variable))
				}
				
				design <- model.matrix(design_formula, data = metadata)
				
				# Validate groups
				class1_clean <- make.names(class1)
				class2_clean <- make.names(class2)
				
				n_class1 <- sum(metadata[[variable]] == class1_clean, na.rm = TRUE)
				n_class2 <- sum(metadata[[variable]] == class2_clean, na.rm = TRUE)
				
				if (n_class1 < 2 || n_class2 < 2) {
					stop(paste0(
						"Not enough samples in each group: ",
						class1, ": ", n_class1, " samples, ",
						class2, ": ", n_class2, " samples. ",
						"Need at least 2 samples per group."
					))
				}
				
				# Build contrast
				contrast_string <- paste0(variable, class1_clean, "-", variable, class2_clean)
				user_contrast <- makeContrasts(contrasts = contrast_string, levels = design)
				
				# Fit model
				fit <- lmFit(exprs_data, design)
				fit2 <- contrasts.fit(fit, user_contrast)
				fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
				
			} else {
				# Continuous
				if (!is.null(covariates) && length(covariates) > 0) {
					design_formula <- as.formula(paste0("~ ", paste(c(variable, covariates), collapse = " + ")))
				} else {
					design_formula <- as.formula(paste0("~ ", variable))
				}
				
				design <- model.matrix(design_formula, data = metadata)
				
				# Fit model
				fit <- lmFit(exprs_data, design)
				fit2 <- contrasts.fit(fit, coeff = 2)
				fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
			}
			
			# Get topTable
			TT <- topTable(fit2, number = Inf)
			TT <- TT %>% arrange(P.Value)
			
			# Store design matrix
			rv$design_matrix <- design
			
			# Store results
			rv$limma_results <- list(
				topTable_full = TT,
				topTable = TT,
				design = design,
				fit = fit2,
				metadata = metadata,
				expression = exprs_data,
				variable = variable,
				continuous = continuous,
				class1 = class1,
				class2 = class2,
				fc_cutoff = fc_cutoff,
				p_val = p_val,
				fc_direction = fc_direction,
				analysis_mode = "basic"
			)
			
			rv$metadata_filtered <- metadata
			
			# Check for significant results
			sig_rows_all <- which(TT$P.Value < p_val & abs(TT$logFC) > log2(fc_cutoff))
			
			if (length(sig_rows_all) < 1) {
				showNotification(
					"⚠️ No significant features found. Try adjusting cutoffs.",
					type = "warning",
					duration = 10
				)
				return()
			}
			
			# Apply FC direction filter
			TT_filtered <- filter_by_fc_direction(TT, "logFC", fc_direction)
			sig_rows <- which(TT_filtered$P.Value < p_val & abs(TT_filtered$logFC) > log2(fc_cutoff))
			
			rv$limma_results$topTable <- TT_filtered
			
			if (length(sig_rows) < 1) {
				direction_text <- switch(fc_direction,
																 "up" = "up-regulated (positive FC)",
																 "down" = "down-regulated (negative FC)",
																 "both" = "in either direction"
				)
				
				showNotification(
					HTML(paste0(
						"⚠️ No significant <strong>", direction_text, "</strong> features found.<br>",
						"Total significant features (both directions): ", length(sig_rows_all), "<br>",
						"Try changing FC direction filter or adjusting cutoffs."
					)),
					type = "warning",
					duration = 10
				)
				return()
			}
			
			# Generate additional outputs
			generate_standard_outputs(TT_filtered, sig_rows, metadata, exprs_data, variable, 
																continuous, class1_clean, class2_clean)
		}
		
		### Run Advanced Limma (Custom Contrasts) ####
		run_advanced_limma <- function() {
			req(input$contrast_variable, input$user_contrast)
			
			# Get data
			eset_temp <- eset_current()
			metadata <- Biobase::pData(eset_temp)
			exprs_data <- Biobase::assayDataElement(eset_temp, input$assay_data_select)
			
			# Apply feature selection
			if (!is.null(rv$feature_select)) {
				exprs_data <- exprs_data[rv$feature_select, , drop = FALSE]
			}
			
			# Get parameters
			variable <- input$contrast_variable
			covariates <- input$contrast_covariates
			user_contrast_string <- input$user_contrast
			fc_cutoff <- input$fc_cutoff
			p_val <- input$p_val
			fc_direction <- input$fc_direction
			
			# Make names syntactically valid
			if (!is.numeric(metadata[[variable]])) {
				metadata[[variable]][!is.na(metadata[[variable]])] <- make.names(metadata[[variable]][!is.na(metadata[[variable]])])
				metadata[[variable]] <- as.factor(metadata[[variable]])
			}
			
			if (!is.null(covariates) && length(covariates) > 0) {
				for (covariate in covariates) {
					if (!is.numeric(metadata[[covariate]])) {
						metadata[[covariate]][!is.na(metadata[[covariate]])] <- make.names(metadata[[covariate]][!is.na(metadata[[covariate]])])
						metadata[[covariate]] <- as.factor(metadata[[covariate]])
					}
				}
			}
			
			# Remove samples with NA values
			covariate_cols <- c(variable, covariates)
			if (length(covariate_cols) > 1) {
				na_rows <- which(rowSums(is.na(metadata[, covariate_cols])) > 0)
			} else {
				na_rows <- which(is.na(metadata[, covariate_cols]))
			}
			
			if (length(na_rows) > 0) {
				warning(paste("Removed", length(na_rows), "samples with NA values."))
				metadata <- metadata[-na_rows, ]
				exprs_data <- exprs_data[, rownames(metadata)]
			}
			
			# Build design with contrasts
			if (!is.null(covariates) && length(covariates) > 0) {
				design_formula <- as.formula(paste0("~ 0 + ", paste(c(variable, covariates), collapse = " + ")))
			} else {
				design_formula <- as.formula(paste0("~ 0 + ", variable))
			}
			
			design <- model.matrix(design_formula, data = metadata)
			
			# Clean column names for makeContrasts
			colnames(design) <- gsub(paste0("^", variable), "", colnames(design))
			if (!is.null(covariates) && length(covariates) > 0) {
				for (cov in covariates) {
					colnames(design) <- gsub(paste0("^", cov), "", colnames(design))
				}
			}
			
			# Build contrast
			user_contrast <- makeContrasts(contrasts = user_contrast_string, levels = design)
			
			# Fit model
			fit <- lmFit(exprs_data, design)
			fit2 <- contrasts.fit(fit, user_contrast)
			fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
			
			# Get topTable
			TT <- topTable(fit2, number = Inf)
			TT <- TT %>% arrange(P.Value)
			
			# Store design matrix
			rv$design_matrix <- design
			
			# Extract groups from contrast
			contrast_groups <- rownames(user_contrast)[user_contrast != 0]
			has_complex_contrast <- any(abs(user_contrast) == 0.5)
			
			if (has_complex_contrast) {
				groupPos_levels <- names(user_contrast[user_contrast > 0, ])
				groupNeg_levels <- names(user_contrast[user_contrast < 0, ])
				groupPos <- paste(groupPos_levels, collapse = "+")
				groupNeg <- paste(groupNeg_levels, collapse = "+")
			} else {
				groupPos <- names(which(user_contrast > 0))
				groupNeg <- names(which(user_contrast < 0))
			}
			
			# Store results
			rv$limma_results <- list(
				topTable_full = TT,
				topTable = TT,
				design = design,
				fit = fit2,
				metadata = metadata,
				expression = exprs_data,
				variable = variable,
				continuous = FALSE,
				class1 = groupPos,
				class2 = groupNeg,
				fc_cutoff = fc_cutoff,
				p_val = p_val,
				fc_direction = fc_direction,
				analysis_mode = "advanced",
				user_contrast = user_contrast,
				has_complex_contrast = has_complex_contrast
			)
			
			rv$metadata_filtered <- metadata
			
			# Check for significant results
			sig_rows_all <- which(TT$P.Value < p_val & abs(TT$logFC) > log2(fc_cutoff))
			
			if (length(sig_rows_all) < 1) {
				showNotification(
					"⚠️ No significant features found. Try adjusting cutoffs.",
					type = "warning",
					duration = 10
				)
				return()
			}
			
			# Apply FC direction filter
			TT_filtered <- filter_by_fc_direction(TT, "logFC", fc_direction)
			sig_rows <- which(TT_filtered$P.Value < p_val & abs(TT_filtered$logFC) > log2(fc_cutoff))
			
			rv$limma_results$topTable <- TT_filtered
			
			if (length(sig_rows) < 1) {
				direction_text <- switch(fc_direction,
																 "up" = "up-regulated (positive FC)",
																 "down" = "down-regulated (negative FC)",
																 "both" = "in either direction"
				)
				
				showNotification(
					HTML(paste0(
						"⚠️ No significant <strong>", direction_text, "</strong> features found.<br>",
						"Total significant features (both directions): ", length(sig_rows_all)
					)),
					type = "warning",
					duration = 10
				)
				return()
			}
			
			# Generate additional outputs
			generate_standard_outputs(TT_filtered, sig_rows, metadata, exprs_data, variable, 
																FALSE, groupPos, groupNeg)
		}
		
		### Run AutoAb Limma (Frequency Analysis) ####
		run_autoab_limma <- function() {
			req(input$autoab_variable, input$autoab_group_pos, input$autoab_group_neg)
			
			# Get data
			eset_temp <- eset_current()
			metadata <- Biobase::pData(eset_temp)
			exprs_data <- Biobase::assayDataElement(eset_temp, input$assay_data_select)
			
			# Apply feature selection
			if (!is.null(rv$feature_select)) {
				exprs_data <- exprs_data[rv$feature_select, , drop = FALSE]
			}
			
			# Get parameters
			variable <- input$autoab_variable
			groupPos <- input$autoab_group_pos
			groupNeg <- input$autoab_group_neg
			fc_cutoff <- input$fc_cutoff
			p_val <- input$p_val
			delta_cutoff <- input$delta_cutoff
			exclude_zeros <- input$exclude_zeros
			fc_direction <- input$fc_direction
			
			# Make names syntactically valid
			if (!is.numeric(metadata[[variable]])) {
				metadata[[variable]][!is.na(metadata[[variable]])] <- make.names(metadata[[variable]][!is.na(metadata[[variable]])])
				metadata[[variable]] <- as.factor(metadata[[variable]])
			}
			
			groupPos_clean <- make.names(groupPos)
			groupNeg_clean <- make.names(groupNeg)
			
			# Remove samples with NA values
			na_rows <- which(is.na(metadata[[variable]]))
			
			if (length(na_rows) > 0) {
				warning(paste("Removed", length(na_rows), "samples with NA values."))
				metadata <- metadata[-na_rows, ]
				exprs_data <- exprs_data[, rownames(metadata)]
			}
			
			# Filter to relevant groups
			metadata <- metadata[metadata[[variable]] %in% c(groupPos_clean, groupNeg_clean), ]
			exprs_data <- exprs_data[, rownames(metadata)]
			
			# Reorder metadata by groupPos then groupNeg
			metadata[[variable]] <- factor(metadata[[variable]], levels = c(groupPos_clean, groupNeg_clean))
			metadata <- metadata %>% arrange(across(all_of(variable)))
			exprs_data <- exprs_data[, rownames(metadata)]
			
			# Build design
			design_formula <- as.formula(paste0("~ 0 + ", variable))
			design <- model.matrix(design_formula, data = metadata)
			colnames(design) <- gsub(paste0("^", variable), "", colnames(design))
			
			# Build contrast
			contrast_string <- paste0(groupPos_clean, "-", groupNeg_clean)
			user_contrast <- makeContrasts(contrasts = contrast_string, levels = design)
			
			# Fit model
			fit <- lmFit(exprs_data, design)
			fit2 <- contrasts.fit(fit, user_contrast)
			fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
			
			# Get topTable
			TT <- topTable(fit2, number = Inf)
			TT <- TT %>% arrange(P.Value)
			
			# Store design matrix
			rv$design_matrix <- design
			
			# Calculate local FC (excluding zeros if requested)
			gap_tab <- table(metadata[[variable]])
			gap_finder <- gap_tab[1]
			
			groupPos_dat <- exprs_data[, 1:gap_finder, drop = FALSE]
			groupNeg_dat <- exprs_data[, (gap_finder + 1):ncol(exprs_data), drop = FALSE]
			
			if (exclude_zeros) {
				# Handle all-zero AAbs
				zero_Pos <- rownames(groupPos_dat)[rowSums(groupPos_dat) == 0]
				zero_Neg <- rownames(groupNeg_dat)[rowSums(groupNeg_dat) == 0]
				
				if (length(zero_Pos) > 0) {
					groupPos_dat[zero_Pos, ] <- 0.1
				}
				if (length(zero_Neg) > 0) {
					groupNeg_dat[zero_Neg, ] <- 0.1
				}
				
				# Calculate means excluding zeros
				groupPos_means <- apply(groupPos_dat, 1, function(x) mean(x[x != 0], na.rm = TRUE))
				groupNeg_means <- apply(groupNeg_dat, 1, function(x) mean(x[x != 0], na.rm = TRUE))
			} else {
				groupPos_means <- rowMeans(groupPos_dat, na.rm = TRUE)
				groupNeg_means <- rowMeans(groupNeg_dat, na.rm = TRUE)
			}
			
			# Calculate AAb FC
			Aab_FCs <- groupPos_means - groupNeg_means
			TT$Aab_FC <- logratio2foldchange(Aab_FCs)
			
			# Frequency analysis
			freq_results <- Aab_counter(
				call_mat = exprs_data,
				meta_use = metadata,
				var = variable,
				groupPos = groupPos_clean,
				groupNeg = groupNeg_clean
			)
			
			# Merge with limma results
			TT$Feature <- rownames(TT)
			freq_results$Feature <- rownames(freq_results)
			
			merged_results <- left_join(TT, freq_results, by = "Feature")
			rownames(merged_results) <- merged_results$Feature
			merged_results <- merged_results %>% select(-Feature)
			
			# Calculate delta %
			groupPos_freq_col <- paste0("AAb_frequency_", groupPos_clean)
			groupNeg_freq_col <- paste0("AAb_frequency_", groupNeg_clean)
			
			merged_results$delta <- merged_results[[groupPos_freq_col]] - merged_results[[groupNeg_freq_col]]
			
			# Reorder columns
			merged_results <- merged_results %>%
				select(logFC, Aab_FC, AveExpr, P.Value, adj.P.Val, everything(), -c(t, B))
			
			# Store results
			rv$limma_results <- list(
				topTable_full = TT,
				topTable = merged_results,
				design = design,
				fit = fit2,
				metadata = metadata,
				expression = exprs_data,
				variable = variable,
				continuous = FALSE,
				class1 = groupPos_clean,
				class2 = groupNeg_clean,
				fc_cutoff = fc_cutoff,
				p_val = p_val,
				fc_direction = fc_direction,
				analysis_mode = "autoab",
				delta_cutoff = delta_cutoff,
				exclude_zeros = exclude_zeros
			)
			
			rv$frequency_results <- merged_results
			rv$metadata_filtered <- metadata
			
			# Filter by delta cutoff
			delta_sig <- merged_results %>%
				filter(abs(delta) >= delta_cutoff)
			
			# Check group size imbalance
			group_size_metric <- ncol(groupNeg_dat) / ncol(groupPos_dat) * 100
			
			if (group_size_metric < 70 || group_size_metric > 140) {
				min_freq_threshold <- input$min_freq_threshold
				
				if (group_size_metric < 70) {
					# groupNeg is small
					small_group_col <- paste0("AAb_count_", groupNeg_clean)
					delta_sig <- delta_sig %>%
						filter((delta < -delta_cutoff & .data[[small_group_col]] >= min_freq_threshold * ncol(groupNeg_dat)) |
									 	delta > delta_cutoff)
				} else {
					# groupPos is small
					small_group_col <- paste0("AAb_count_", groupPos_clean)
					delta_sig <- delta_sig %>%
						filter((delta > delta_cutoff & .data[[small_group_col]] >= min_freq_threshold * ncol(groupPos_dat)) |
									 	delta < -delta_cutoff)
				}
			}
			
			if (nrow(delta_sig) < 1) {
				showNotification(
					paste0("⚠️ No features found with delta % >= ", delta_cutoff),
					type = "warning",
					duration = 10
				)
				return()
			}
			
			# Apply FC direction filter to limma sig hits
			TT_filtered <- filter_by_fc_direction(merged_results, "logFC", fc_direction)
			sig_rows <- which(TT_filtered$P.Value < p_val & abs(TT_filtered$logFC) > log2(fc_cutoff))
			
			rv$limma_results$topTable <- TT_filtered
			
			# Generate outputs for both limma hits and delta hits
			if (length(sig_rows) > 0) {
				generate_standard_outputs(TT_filtered, sig_rows, metadata, exprs_data, variable, 
																	FALSE, groupPos_clean, groupNeg_clean)
			}
			
			# Additional delta-specific outputs
			if (nrow(delta_sig) >= 3) {
				data_sig_delta <- exprs_data[rownames(delta_sig), rownames(metadata), drop = FALSE]
				
				# Generate delta heatmaps
				rv$heatmap_data_delta <- generate_heatmap_data(
					data_sig = data_sig_delta,
					data_sig_RC = data.frame(t(scale(t(exprs_data), scale = FALSE, center = TRUE)), check.names = FALSE)[rownames(delta_sig), colnames(data_sig_delta), drop = FALSE],
					meta_2 = metadata[, variable, drop = FALSE],
					variable = variable,
					continuous = FALSE,
					class1 = groupPos_clean,
					class2 = groupNeg_clean,
					add_anno = NULL
				)
			}
		}
		
		### Generate Standard Outputs ####
		generate_standard_outputs <- function(TT_filtered, sig_rows, metadata, exprs_data, variable, 
																					continuous, class1, class2) {
			
			# Prepare filtered metadata
			if (!continuous) {
				if (rv$limma_results$analysis_mode == "advanced" && rv$limma_results$has_complex_contrast) {
					# Complex contrast - need to merge groups
					meta_filtered <- metadata
				} else {
					meta_filtered <- metadata[metadata[[variable]] %in% c(class1, class2), ]
				}
			} else {
				meta_filtered <- metadata
			}
			
			# Create meta_2
			if (!is.null(input$add_anno) && length(input$add_anno) > 0) {
				cols_anno <- unique(c(variable, input$add_anno))
				cols_anno <- cols_anno[cols_anno %in% colnames(meta_filtered)]
				meta_2 <- as.data.frame(meta_filtered[, cols_anno, drop = FALSE], stringsAsFactors = FALSE)
			} else {
				meta_2 <- as.data.frame(meta_filtered[, variable, drop = FALSE], stringsAsFactors = FALSE)
				if (colnames(meta_2)[1] != variable) {
					colnames(meta_2) <- variable
				}
			}
			
			# Filter significant features
			data_sig <- exprs_data[rownames(TT_filtered[sig_rows, ]), rownames(meta_2), drop = FALSE]
			data_RC <- data.frame(t(scale(t(exprs_data), scale = FALSE, center = TRUE)), check.names = FALSE)
			data_sig_RC <- data_RC[rownames(data_sig), colnames(data_sig), drop = FALSE]
			
			# Generate heatmaps
			if (nrow(data_sig) >= 3) {
				rv$heatmap_data <- generate_heatmap_data(
					data_sig = data_sig,
					data_sig_RC = data_sig_RC,
					meta_2 = meta_2,
					variable = variable,
					continuous = continuous,
					class1 = class1,
					class2 = class2,
					add_anno = input$add_anno
				)
			}
			
			# Generate ROC
			if (!continuous && nrow(data_sig) > 0) {
				rv$merged_results <- generate_roc_results(
					exprs_data = exprs_data,
					data_sig = data_sig,
					meta_2 = meta_2,
					TT = TT_filtered,
					variable = variable,
					class1 = class1,
					class2 = class2,
					sig_rows = sig_rows,
					fc_direction = input$fc_direction
				)
				
				# ✅ FIX: If ROC failed, set to NULL
				if (is.null(rv$merged_results)) {
					warning("ROC analysis failed - continuing without ROC results")
				}
			}
			
			# Generate violins
			if (input$plot_violins && !continuous && nrow(data_sig) > 0) {
				tryCatch({
					violin_input <- if (!is.null(rv$merged_results)) {
						rv$merged_results
					} else {
						TT_filtered[sig_rows, ]
					}
					
					rv$violin_plots <- generate_violin_plots(
						exprs_data = exprs_data,
						merged_results = violin_input,
						meta_2 = meta_2,
						variable = variable
					)
				}, error = function(e) {
					warning("Failed to generate violin plots: ", e$message)
					rv$violin_plots <- NULL
				})
			}
		}
		
		### Helper: Filter by FC Direction ####
		filter_by_fc_direction <- function(data, logFC_col = "logFC", direction = "up") {
			if (direction == "up") {
				return(data[data[[logFC_col]] > 0, , drop = FALSE])
			} else if (direction == "down") {
				return(data[data[[logFC_col]] < 0, , drop = FALSE])
			} else {
				return(data)
			}
		}
		
		## Results Outputs ####
		
		### Completion Flags ####
		output$limma_complete <- reactive({
			!is.null(rv$limma_results)
		})
		outputOptions(output, "limma_complete", suspendWhenHidden = FALSE)
		
		output$has_heatmaps <- reactive({
			!is.null(rv$heatmap_data)
		})
		outputOptions(output, "has_heatmaps", suspendWhenHidden = FALSE)
		
		output$has_roc <- reactive({
			!is.null(rv$merged_results)
		})
		outputOptions(output, "has_roc", suspendWhenHidden = FALSE)
		
		output$has_violins <- reactive({
			!is.null(rv$violin_plots)
		})
		outputOptions(output, "has_violins", suspendWhenHidden = FALSE)
		
		output$has_frequency <- reactive({
			!is.null(rv$frequency_results)
		})
		outputOptions(output, "has_frequency", suspendWhenHidden = FALSE)
		
		### Design Matrix Display ####
		output$design_matrix_display <- renderPrint({
			req(rv$design_matrix)
			
			cat("Design Matrix:\n")
			cat("==============\n\n")
			print(head(rv$design_matrix, 20))
			cat("\n...")
			cat("\nDimensions:", nrow(rv$design_matrix), "samples ×", ncol(rv$design_matrix), "coefficients\n")
		})
		
		### Summary ####
		output$limma_summary <- renderPrint({
			req(rv$limma_results)
			
			TT <- rv$limma_results$topTable
			TT_full <- rv$limma_results$topTable_full
			fc_direction <- rv$limma_results$fc_direction
			
			sig_count <- sum(TT$P.Value < rv$limma_results$p_val & abs(TT$logFC) > log2(rv$limma_results$fc_cutoff), na.rm = TRUE)
			sig_count_full <- sum(TT_full$P.Value < rv$limma_results$p_val & abs(TT_full$logFC) > log2(rv$limma_results$fc_cutoff), na.rm = TRUE)
			
			cat("Limma Analysis Summary\n")
			cat("======================\n\n")
			cat("Analysis Mode:", rv$limma_results$analysis_mode, "\n")
			cat("Variable:", rv$limma_results$variable, "\n")
			cat("Type:", if (rv$limma_results$continuous) "Continuous" else "Categorical", "\n")
			if (!rv$limma_results$continuous) {
				cat("Comparison:", rv$limma_results$class1, "vs", rv$limma_results$class2, "\n")
			}
			cat("Fold Change Cutoff:", rv$limma_results$fc_cutoff, "\n")
			cat("P-value Cutoff:", rv$limma_results$p_val, "\n")
			cat("Sample Count:", ncol(rv$limma_results$expression), "\n")
			
			direction_text <- switch(fc_direction,
															 "up" = "Up-regulated (positive FC)",
															 "down" = "Down-regulated (negative FC)",
															 "both" = "Both directions"
			)
			
			cat("FC Direction Filter:", direction_text, "\n\n")
			cat("Significant Features (filtered):", sig_count, "\n")
			
			if (fc_direction != "both") {
				cat("Total Significant (all directions):", sig_count_full, "\n")
			}
			
			if (rv$limma_results$analysis_mode == "autoab") {
				cat("\nAutoAb Mode:\n")
				cat("Delta % Cutoff:", rv$limma_results$delta_cutoff, "\n")
				cat("Exclude Zeros:", rv$limma_results$exclude_zeros, "\n")
				
				if (!is.null(rv$frequency_results)) {
					delta_hits <- sum(abs(rv$frequency_results$delta) >= rv$limma_results$delta_cutoff, na.rm = TRUE)
					cat("Features with delta % >=", rv$limma_results$delta_cutoff, ":", delta_hits, "\n")
				}
			}
		})
		
		### Top Table ####
		output$limma_top_table <- DT::renderDataTable({
			req(rv$limma_results)
			
			TT <- rv$limma_results$topTable
			sig_rows <- which(TT$P.Value < rv$limma_results$p_val & abs(TT$logFC) > log2(rv$limma_results$fc_cutoff))
			
			if (length(sig_rows) > 0) {
				sig_table <- TT[sig_rows, ] %>% arrange(P.Value)
			} else {
				sig_table <- TT[1:min(10, nrow(TT)), ]
			}
			
			if (!"FC" %in% colnames(sig_table)) {
				sig_table$FC <- logratio2foldchange(sig_table$logFC)
			}
			
			# Select columns to display
			display_cols <- intersect(
				c("logFC", "FC", "AveExpr", "t", "P.Value", "adj.P.Val", "B", "Aab_FC", "delta"),
				colnames(sig_table)
			)
			
			sig_table %>%
				select(all_of(display_cols)) %>%
				DT::datatable(
					options = list(pageLength = 20, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = intersect(c("logFC", "FC", "AveExpr", "t", "B", "Aab_FC", "delta"), display_cols), digits = 3) %>%
				DT::formatSignif(columns = c("P.Value", "adj.P.Val"), digits = 3)
		})
		
		### Frequency Table ####
		output$frequency_table <- DT::renderDataTable({
			req(rv$frequency_results)
			
			rv$frequency_results %>%
				arrange(desc(abs(delta))) %>%
				DT::datatable(
					options = list(pageLength = 20, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = c("logFC", "Aab_FC", "delta"), digits = 2) %>%
				DT::formatSignif(columns = c("P.Value", "adj.P.Val"), digits = 3)
		})
		
		### Volcano Plot ####
		output$limma_volcano <- renderPlot({
			req(rv$limma_results)
			
			TT_full <- rv$limma_results$topTable_full
			fc_cut <- rv$limma_results$fc_cutoff
			p_cut <- rv$limma_results$p_val
			fc_direction <- rv$limma_results$fc_direction
			
			sigs_ordered <- TT_full[order(TT_full$P.Value), ]
			
			if (fc_direction == "up") {
				sigs_ordered$genelabels <- sigs_ordered$P.Value < p_cut & sigs_ordered$logFC > log2(fc_cut)
			} else if (fc_direction == "down") {
				sigs_ordered$genelabels <- sigs_ordered$P.Value < p_cut & sigs_ordered$logFC < -log2(fc_cut)
			} else {
				sigs_ordered$genelabels <- sigs_ordered$P.Value < p_cut & abs(sigs_ordered$logFC) > log2(fc_cut)
			}
			
			sigs_ordered$threshold <- sigs_ordered$genelabels
			sigs_ordered$symbol <- rownames(sigs_ordered)
			
			p <- ggplot(sigs_ordered, aes(x = logFC, y = -log10(P.Value))) +
				geom_point(aes(colour = threshold), alpha = 0.6, size = 2) +
				scale_color_brewer(palette = "Dark2") +
				geom_text_repel(
					aes(label = ifelse(genelabels, symbol, "")),
					size = 3,
					max.overlaps = 50
				) +
				geom_hline(yintercept = -log10(p_cut), linetype = "dashed", color = "blue") +
				theme_minimal() +
				theme(
					legend.position = "none",
					plot.title = element_text(size = rel(1.5), hjust = 0.5),
					axis.title = element_text(size = rel(1.25))
				)
			
			if (fc_direction == "both") {
				p <- p +
					geom_vline(xintercept = log2(fc_cut), linetype = "dashed", color = "blue") +
					geom_vline(xintercept = -log2(fc_cut), linetype = "dashed", color = "blue") +
					labs(
						title = paste("Volcano Plot:", rv$limma_results$variable, "(Both Directions)"),
						x = "log2 Fold Change",
						y = "-log10(P-value)"
					)
			} else if (fc_direction == "up") {
				p <- p +
					geom_vline(xintercept = log2(fc_cut), linetype = "dashed", color = "red") +
					labs(
						title = paste("Volcano Plot:", rv$limma_results$variable, "(Up-regulated Only)"),
						x = "log2 Fold Change",
						y = "-log10(P-value)"
					)
			} else {
				p <- p +
					geom_vline(xintercept = -log2(fc_cut), linetype = "dashed", color = "red") +
					labs(
						title = paste("Volcano Plot:", rv$limma_results$variable, "(Down-regulated Only)"),
						x = "log2 Fold Change",
						y = "-log10(P-value)"
					)
			}
			
			return(p)
		})
		
		### Heatmaps ####
		output$heatmap_manual <- renderPlot({
			req(rv$heatmap_data)
			
			pheatmap(
				rv$heatmap_data$toplot,
				gaps_col = rv$heatmap_data$gap_finder,
				annotation_col = rv$heatmap_data$meta_plot,
				annotation_colors = rv$heatmap_data$anno_col,
				cluster_cols = FALSE,
				show_rownames = TRUE,
				show_colnames = FALSE,
				main = "Heatmap - Manual Sort"
			)
		})
		
		output$heatmap_rc <- renderPlot({
			req(rv$heatmap_data)
			
			pheatmap(
				rv$heatmap_data$toplot_RC,
				gaps_col = rv$heatmap_data$gap_finder,
				annotation_col = rv$heatmap_data$meta_plot_RC,
				annotation_colors = rv$heatmap_data$anno_col,
				cluster_cols = TRUE,
				show_rownames = TRUE,
				show_colnames = FALSE,
				main = "Heatmap - Row Centered"
			)
		})
		
		### ROC Table ####
		output$roc_table <- DT::renderDataTable({
			req(rv$merged_results)
			
			display_cols <- intersect(
				c("logFC", "FC", "P.Value", "adj.P.Val", "AUC", "Sensitivity", "Specificity", "Optimal.Cutoff"),
				colnames(rv$merged_results)
			)
			
			rv$merged_results %>%
				select(all_of(display_cols)) %>%
				DT::datatable(
					options = list(pageLength = 20, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = intersect(c("logFC", "FC", "AUC", "Sensitivity", "Specificity", "Optimal.Cutoff"), display_cols), digits = 3) %>%
				DT::formatSignif(columns = c("P.Value", "adj.P.Val"), digits = 3)
		})
		
		### Violin Plots ####
		output$violin_plots_ui <- renderUI({
			req(rv$violin_plots)
			
			plot_output_list <- lapply(seq_along(rv$violin_plots), function(i) {
				plotname <- paste0("violin_", i)
				plotOutput(ns(plotname), height = "600px")
			})
			
			do.call(tagList, plot_output_list)
		})
		
		observe({
			req(rv$violin_plots)
			
			for (i in seq_along(rv$violin_plots)) {
				local({
					my_i <- i
					plotname <- paste0("violin_", my_i)
					output[[plotname]] <- renderPlot({
						rv$violin_plots[[my_i]]
					})
				})
			}
		})
		
		## Downloads ####
		
		### Download Design Matrix ####
		output$download_design <- downloadHandler(
			filename = function() {
				paste0("design_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$design_matrix)
				write.csv(rv$design_matrix, file, row.names = TRUE)
			}
		)
		
		### Download TopTable ####
		output$download_toptable <- downloadHandler(
			filename = function() {
				mode <- rv$limma_results$analysis_mode
				direction <- rv$limma_results$fc_direction
				paste0("limma_toptable_", mode, "_", direction, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$limma_results)
				write.csv(rv$limma_results$topTable, file, row.names = TRUE)
			}
		)
		
		### Download ROC ####
		output$download_roc <- downloadHandler(
			filename = function() {
				paste0("limma_ROC_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$merged_results)
				write.csv(rv$merged_results, file, row.names = TRUE)
			}
		)
		
		### Download Frequency ####
		output$download_frequency <- downloadHandler(
			filename = function() {
				paste0("autoab_frequency_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$frequency_results)
				write.csv(rv$frequency_results, file, row.names = TRUE)
			}
		)
		
		### Download Heatmaps ####
		output$download_heatmaps <- downloadHandler(
			filename = function() {
				paste0("heatmaps_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
			},
			content = function(file) {
				req(rv$heatmap_data)
				
				pdf(file, width = 20, height = 10)
				
				print(pheatmap(
					rv$heatmap_data$toplot,
					gaps_col = rv$heatmap_data$gap_finder,
					annotation_col = rv$heatmap_data$meta_plot,
					annotation_colors = rv$heatmap_data$anno_col,
					cluster_cols = FALSE,
					show_rownames = TRUE,
					show_colnames = FALSE,
					main = "Heatmap - Manual Sort"
				))
				
				print(pheatmap(
					rv$heatmap_data$toplot_RC,
					gaps_col = rv$heatmap_data$gap_finder,
					annotation_col = rv$heatmap_data$meta_plot_RC,
					annotation_colors = rv$heatmap_data$anno_col,
					cluster_cols = TRUE,
					show_rownames = TRUE,
					show_colnames = FALSE,
					main = "Heatmap - Row Centered"
				))
				
				dev.off()
			}
		)
		
		### Download Violins ####
		output$download_violins <- downloadHandler(
			filename = function() {
				paste0("violin_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
			},
			content = function(file) {
				req(rv$violin_plots)
				
				pdf(file, width = 10, height = 7)
				for (i in seq_along(rv$violin_plots)) {
					print(rv$violin_plots[[i]])
				}
				dev.off()
			}
		)
		
		## Return Values ####
		return(reactive({
			list(
				limma_results = rv$limma_results,
				merged_results = rv$merged_results,
				frequency_results = rv$frequency_results,
				heatmap_data = rv$heatmap_data,
				violin_plots = rv$violin_plots,
				design_matrix = rv$design_matrix
			)
		}))
	})
}

# Helper Functions (continued) ####

## Generate Heatmap Data ####

#' Generate Heatmap Data
#'
#' @param data_sig Matrix of significant features
#' @param data_sig_RC Matrix of significant features (row-centered)
#' @param meta_2 Metadata data frame
#' @param variable Character, primary variable name
#' @param continuous Logical, is variable continuous?
#' @param class1 Character, class 1 name (categorical only)
#' @param class2 Character, class 2 name (categorical only)
#' @param add_anno Character vector, additional annotation columns
#'
#' @return List with heatmap data components
#' @keywords internal
generate_heatmap_data <- function(data_sig, data_sig_RC, meta_2, variable, 
																	continuous, class1, class2, add_anno) {
	
	if (!continuous) {
		# Categorical variable
		get_groups <- c(class1, class2)
		meta_2[[variable]] <- factor(meta_2[[variable]], levels = get_groups)
		
		# Manual sort heatmap
		toplot <- NULL
		for (j in seq_along(get_groups)) {
			ns <- data_sig[, which(meta_2[[variable]] == get_groups[j]), drop = FALSE]
			if (ncol(ns) > 1) {
				ns_clust <- hclust(vegan::vegdist(t(ns), method = "euclidean"))
				order <- ns_clust$labels[ns_clust$order]
				if (j == 1) {
					toplot <- ns[, order, drop = FALSE]
				} else {
					toplot <- cbind(toplot, ns[, order, drop = FALSE])
				}
			} else {
				if (j == 1) {
					toplot <- ns
				} else {
					toplot <- cbind(toplot, ns)
				}
			}
		}
		meta_plot <- meta_2[colnames(toplot), , drop = FALSE]
		
		# Row-centered heatmap
		toplot_RC <- NULL
		for (j in seq_along(get_groups)) {
			ns <- data_sig_RC[, which(meta_2[[variable]] == get_groups[j]), drop = FALSE]
			if (ncol(ns) > 1) {
				ns_clust <- hclust(vegan::vegdist(t(ns), method = "euclidean"))
				order <- ns_clust$labels[ns_clust$order]
				if (j == 1) {
					toplot_RC <- ns[, order, drop = FALSE]
				} else {
					toplot_RC <- cbind(toplot_RC, ns[, order, drop = FALSE])
				}
			} else {
				if (j == 1) {
					toplot_RC <- ns
				} else {
					toplot_RC <- cbind(toplot_RC, ns)
				}
			}
		}
		meta_plot_RC <- meta_2[colnames(toplot_RC), , drop = FALSE]
		
		# Gap finder
		gap_tab <- table(meta_2[[variable]])
		gap_finder <- gap_tab[1]
		
	} else {
		# Continuous variable
		meta_plot <- meta_2 %>% arrange(across(all_of(variable)))
		meta_plot_RC <- meta_plot
		toplot <- data_sig[, rownames(meta_plot), drop = FALSE]
		toplot_RC <- data_sig_RC[, rownames(meta_plot_RC), drop = FALSE]
		gap_finder <- NULL
	}
	
	# Get colors for annotation
	if (is.null(add_anno) || length(add_anno) == 0) {
		anno_col <- color_distinct(meta_colors = meta_plot, variables = 1)
	} else {
		anno_col <- color_distinct(meta_colors = meta_plot, variables = seq_len(ncol(meta_plot)))
	}
	
	return(list(
		toplot = toplot,
		toplot_RC = toplot_RC,
		meta_plot = meta_plot,
		meta_plot_RC = meta_plot_RC,
		anno_col = anno_col,
		gap_finder = gap_finder
	))
}

## Generate ROC Results ####

#' Generate ROC Results
#'
#' @param exprs_data Matrix, full expression data
#' @param data_sig Matrix, significant features only
#' @param meta_2 Metadata data frame
#' @param TT TopTable results
#' @param variable Character, primary variable name
#' @param class1 Character, class 1 name
#' @param class2 Character, class 2 name
#' @param sig_rows Integer vector, indices of significant rows
#' @param fc_direction Character, "up", "down", or "both" (default "both")
#'
#' @return Data frame with merged limma + ROC results
#' @keywords internal
## Generate ROC Results ####
generate_roc_results <- function(exprs_data, data_sig, meta_2, TT, variable, 
																 class1, class2, sig_rows, fc_direction = "both") {
	
	tryCatch({
		# ✅ FIX: Ensure meta_2 has the variable column properly named
		if (ncol(meta_2) == 1 && colnames(meta_2)[1] != variable) {
			colnames(meta_2) <- variable
		}
		
		# ✅ FIX: Ensure variable exists in meta_2
		if (!variable %in% colnames(meta_2)) {
			warning("Variable '", variable, "' not found in metadata for ROC analysis")
			return(NULL)
		}
		
		# Run ROC analysis based on direction
		ROC_results_up <- NULL
		ROC_results_down <- NULL
		
		if (fc_direction == "up" || fc_direction == "both") {
			ROC_results_up <- ROC_mini(
				input = exprs_data[rownames(data_sig), rownames(meta_2), drop = FALSE],
				metadata = meta_2,
				variable = variable,
				groupPos = class1,
				groupNeg = class2,
				descriptor = "up",
				folder = NULL
			)
			
			# ✅ FIX: Check if ROC returned valid results
			if (is.null(ROC_results_up) || nrow(ROC_results_up) == 0) {
				warning("ROC analysis (up) returned no results")
				ROC_results_up <- NULL
			}
		}
		
		if (fc_direction == "down" || fc_direction == "both") {
			ROC_results_down <- ROC_mini(
				input = exprs_data[rownames(data_sig), rownames(meta_2), drop = FALSE],
				metadata = meta_2,
				variable = variable,
				groupPos = class2,
				groupNeg = class1,
				descriptor = "down",
				folder = NULL
			)
			
			# ✅ FIX: Check if ROC returned valid results
			if (is.null(ROC_results_down) || nrow(ROC_results_down) == 0) {
				warning("ROC analysis (down) returned no results")
				ROC_results_down <- NULL
			}
		}
		
		# ✅ FIX: Handle case where ROC fails
		if (is.null(ROC_results_up) && is.null(ROC_results_down)) {
			warning("ROC analysis failed for both directions")
			return(NULL)
		}
		
		# Merge with limma results
		limma_results <- TT[rownames(data_sig), ]
		limma_results$Protein <- rownames(limma_results)
		
		if (fc_direction == "up" && !is.null(ROC_results_up)) {
			merged_results <- left_join(limma_results, ROC_results_up, by = "Protein")
			
		} else if (fc_direction == "down" && !is.null(ROC_results_down)) {
			merged_results <- left_join(limma_results, ROC_results_down, by = "Protein")
			
		} else {
			# Both directions
			limma_up <- limma_results[limma_results$logFC > 0, ]
			limma_down <- limma_results[limma_results$logFC < 0, ]
			
			merged_up <- if (!is.null(ROC_results_up) && nrow(limma_up) > 0) {
				left_join(limma_up, ROC_results_up, by = "Protein")
			} else {
				limma_up
			}
			
			merged_down <- if (!is.null(ROC_results_down) && nrow(limma_down) > 0) {
				left_join(limma_down, ROC_results_down, by = "Protein")
			} else {
				limma_down
			}
			
			merged_results <- rbind(merged_up, merged_down)
		}
		
		rownames(merged_results) <- merged_results$Protein
		
		# Clean up columns
		merged_results <- merged_results %>%
			dplyr::select(-Protein)
		
		# Only remove columns if they exist
		cols_to_remove <- intersect(c("AveExpr", "B", "t"), colnames(merged_results))
		if (length(cols_to_remove) > 0) {
			merged_results <- merged_results %>% dplyr::select(-all_of(cols_to_remove))
		}
		
		# Format p-values and add FC
		merged_results <- merged_results %>%
			mutate(
				P.Value = format(P.Value, scientific = TRUE, digits = 3),
				adj.P.Val = format(adj.P.Val, scientific = TRUE, digits = 3),
				FC = logratio2foldchange(logFC)
			) %>%
			arrange(desc(abs(FC)))
		
		# Reorder columns
		col_order <- c("logFC", "FC", setdiff(names(merged_results), c("logFC", "FC")))
		merged_results <- merged_results[, col_order]
		
		return(merged_results)
		
	}, error = function(e) {
		warning("Error in generate_roc_results: ", e$message)
		print(e)
		return(NULL)
	})
}

## Generate Violin Plots ####

#' Generate Violin Plots
#'
#' @param exprs_data Matrix, full expression data
#' @param merged_results Data frame, merged limma + ROC results OR topTable subset
#' @param meta_2 Metadata data frame
#' @param variable Character, primary variable name
#'
#' @return List of ggplot objects
#' @keywords internal
generate_violin_plots <- function(exprs_data, merged_results, meta_2, variable) {
	
	tryCatch({
		sig_f <- rownames(merged_results)
		
		if (length(sig_f) == 0) {
			warning("No significant features to plot")
			return(NULL)
		}
		
		sig_f <- sig_f[sig_f %in% rownames(exprs_data)]
		
		if (length(sig_f) == 0) {
			warning("No matching features in expression data")
			return(NULL)
		}
		
		plot_data <- exprs_data[sig_f, rownames(meta_2), drop = FALSE]
		
		if (nrow(plot_data) == 0 || ncol(plot_data) == 0) {
			warning("Empty plot data after filtering")
			return(NULL)
		}
		
		df <- as.matrix(plot_data) %>%
			reshape2::melt()
		
		colnames(df)[1:2] <- c("feature", "Sample")
		
		meta_2$Sample <- rownames(meta_2)
		
		merge.df <- merge(meta_2, df, by = "Sample")
		
		if (nrow(merge.df) == 0) {
			warning("No data after merging with metadata")
			return(NULL)
		}
		
		# Grid size
		if (length(sig_f) <= 6) {
			n_col <- 3
			n_row <- 2
		} else if (length(sig_f) <= 12) {
			n_col <- 3
			n_row <- 4
		} else {
			n_col <- 4
			n_row <- 4
		}
		
		# Colors
		violin_cols <- c("#009E73", "#BEAED4", "#80B1D3", "goldenrod2", "coral2", "palevioletred2")
		n_levels <- nlevels(as.factor(meta_2[[variable]]))
		color_select <- violin_cols[seq_len(min(n_levels, length(violin_cols)))]
		
		if (!variable %in% colnames(merge.df)) {
			warning("Variable '", variable, "' not found in merged data")
			return(NULL)
		}
		
		violins <- ggplot(merge.df, aes(x = .data[[variable]], y = value, color = .data[[variable]])) +
			geom_violin(alpha = 0.5) +
			scale_colour_manual(values = color_select) +
			scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
			theme_minimal() +
			geom_point(position = position_jitter(seed = 1, width = 0.2)) +
			theme(legend.position = "none") +
			facet_wrap_paginate(~ feature, ncol = n_col, nrow = n_row, scales = "free")
		
		if (!requireNamespace("ggforce", quietly = TRUE)) {
			warning("ggforce package not available, returning single page plot")
			return(list(violins))
		}
		
		n_pages <- ggforce::n_pages(violins)
		
		if (n_pages == 0) {
			warning("No pages generated for violin plots")
			return(list(violins))
		}
		
		plot_list <- lapply(seq_len(n_pages), function(i) {
			violins + facet_wrap_paginate(~ feature, ncol = n_col, nrow = n_row, page = i, scales = "free")
		})
		
		return(plot_list)
		
	}, error = function(e) {
		warning("Error in generate_violin_plots: ", e$message)
		print(e)
		return(NULL)
	})
}

## ROC Mini Function ####

#' ROC Analysis for Features
#'
#' @param input Matrix, expression data
#' @param metadata Data frame, sample metadata
#' @param variable Character, grouping variable name
#' @param groupPos Character, positive group name
#' @param groupNeg Character, negative group name
#' @param descriptor Character, descriptor for output
#' @param folder Character, output folder (NULL to skip saving)
#'
#' @return Data frame with ROC metrics
#' @keywords internal
ROC_mini <- function(input, metadata, variable, groupPos, groupNeg, descriptor, folder = NULL) {
	
	tryCatch({
		# Filter metadata to relevant groups
		meta_filtered <- metadata[metadata[[variable]] %in% c(groupPos, groupNeg), ]
		
		# Match expression data
		data_use <- input[, rownames(meta_filtered), drop = FALSE]
		
		# Create binary response (1 = groupPos, 0 = groupNeg)
		response <- ifelse(meta_filtered[[variable]] == groupPos, 1, 0)
		
		# Calculate ROC for each feature
		roc_results <- lapply(rownames(data_use), function(feature) {
			feature_data <- as.numeric(data_use[feature, ])
			
			# Skip if all values are the same
			if (length(unique(feature_data)) == 1) {
				return(data.frame(
					Protein = feature,
					AUC = NA,
					Sensitivity = NA,
					Specificity = NA,
					Optimal.Cutoff = NA
				))
			}
			
			# Calculate ROC
			roc_obj <- pROC::roc(response, feature_data, quiet = TRUE)
			
			# Find optimal cutoff (Youden's index)
			coords <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), 
														 best.method = "youden", quiet = TRUE)
			
			data.frame(
				Protein = feature,
				AUC = as.numeric(pROC::auc(roc_obj)),
				Sensitivity = coords$sensitivity,
				Specificity = coords$specificity,
				Optimal.Cutoff = coords$threshold
			)
		})
		
		roc_df <- do.call(rbind, roc_results)
		rownames(roc_df) <- roc_df$Protein
		
		# Save if folder provided
		if (!is.null(folder)) {
			write.csv(roc_df, file.path(folder, paste0("ROC_results_", descriptor, ".csv")))
		}
		
		return(roc_df)
		
	}, error = function(e) {
		warning("Error in ROC_mini: ", e$message)
		return(NULL)
	})
}

## AutoAb Counter Function ####

#' Count AutoAntibody Frequencies by Group
#'
#' @param call_mat Matrix, expression data (features x samples)
#' @param meta_use Data frame, metadata
#' @param var Character, variable name for grouping
#' @param groupPos Character, positive group name
#' @param groupNeg Character, negative group name
#' @param descriptor Character, descriptor for output (optional)
#'
#' @return Data frame with frequency counts
#' @keywords internal
Aab_counter <- function(call_mat, meta_use, var, groupPos, groupNeg, descriptor = NULL) {
	
	tryCatch({
		# Filter metadata to relevant groups
		meta_filtered <- meta_use[meta_use[[var]] %in% c(groupPos, groupNeg), ]
		
		# Match expression data
		data_use <- call_mat[, rownames(meta_filtered), drop = FALSE]
		
		# Get sample indices for each group
		groupPos_samples <- rownames(meta_filtered)[meta_filtered[[var]] == groupPos]
		groupNeg_samples <- rownames(meta_filtered)[meta_filtered[[var]] == groupNeg]
		
		# Count positive calls (non-zero) per feature per group
		groupPos_counts <- apply(data_use[, groupPos_samples, drop = FALSE], 1, function(x) sum(x != 0))
		groupNeg_counts <- apply(data_use[, groupNeg_samples, drop = FALSE], 1, function(x) sum(x != 0))
		
		# Calculate frequencies
		groupPos_freq <- groupPos_counts / length(groupPos_samples)
		groupNeg_freq <- groupNeg_counts / length(groupNeg_samples)
		
		# Build results data frame
		results <- data.frame(
			row.names = rownames(data_use),
			AAb_count_pos = groupPos_counts,
			AAb_frequency_pos = groupPos_freq,
			AAb_count_neg = groupNeg_counts,
			AAb_frequency_neg = groupNeg_freq
		)
		
		# Rename columns with actual group names
		colnames(results) <- gsub("_pos$", paste0("_", groupPos), colnames(results))
		colnames(results) <- gsub("_neg$", paste0("_", groupNeg), colnames(results))
		
		return(results)
		
	}, error = function(e) {
		warning("Error in Aab_counter: ", e$message)
		return(NULL)
	})
}

## Performance Metrics Function ####

#' Calculate Performance Metrics for AutoAbs
#'
#' @param input Matrix, expression data
#' @param metadata Data frame, sample metadata
#' @param variable Character, grouping variable
#' @param flush Data frame, limma results with frequency data
#' @param groupPos Character, positive group name
#' @param groupNeg Character, negative group name
#'
#' @return Data frame with performance metrics
#' @keywords internal
performance_metrics <- function(input, metadata, variable, flush, groupPos, groupNeg) {
	
	tryCatch({
		# Filter metadata to relevant groups
		meta_filtered <- metadata[metadata[[variable]] %in% c(groupPos, groupNeg), ]
		
		# Match expression data
		data_use <- input[rownames(flush), rownames(meta_filtered), drop = FALSE]
		
		# Create binary response
		response <- ifelse(meta_filtered[[variable]] == groupPos, 1, 0)
		
		# Calculate metrics for each feature
		metrics_list <- lapply(rownames(data_use), function(feature) {
			feature_data <- as.numeric(data_use[feature, ])
			
			# Binary calls (positive if > 0)
			calls <- ifelse(feature_data > 0, 1, 0)
			
			# Calculate confusion matrix elements
			TP <- sum(calls == 1 & response == 1)
			TN <- sum(calls == 0 & response == 0)
			FP <- sum(calls == 1 & response == 0)
			FN <- sum(calls == 0 & response == 1)
			
			# Calculate metrics
			sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else NA
			specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA
			PPV <- if ((TP + FP) > 0) TP / (TP + FP) else NA
			NPV <- if ((TN + FN) > 0) TN / (TN + FN) else NA
			accuracy <- (TP + TN) / length(response)
			
			data.frame(
				AAb = feature,
				Sensitivity = sensitivity,
				Specificity = specificity,
				PPV = PPV,
				NPV = NPV,
				Accuracy = accuracy
			)
		})
		
		metrics_df <- do.call(rbind, metrics_list)
		rownames(metrics_df) <- metrics_df$AAb
		metrics_df <- metrics_df %>% select(-AAb)
		
		return(metrics_df)
		
	}, error = function(e) {
		warning("Error in performance_metrics: ", e$message)
		return(NULL)
	})
}

## Color Distinct Function ####

#' Generate Distinct Colors for Heatmap Annotations
#'
#' @param meta_colors Data frame, metadata for coloring
#' @param variables Integer vector, which columns to color
#'
#' @return Named list of color vectors
#' @keywords internal
color_distinct <- function(meta_colors, variables) {
	
	# Define color palettes
	palette_list <- list(
		RColorBrewer::brewer.pal(8, "Dark2"),
		RColorBrewer::brewer.pal(8, "Set2"),
		RColorBrewer::brewer.pal(9, "Set1"),
		RColorBrewer::brewer.pal(8, "Accent"),
		RColorBrewer::brewer.pal(12, "Set3")
	)
	
	all_colors <- unlist(palette_list)
	
	anno_col <- list()
	
	for (i in variables) {
		var_name <- colnames(meta_colors)[i]
		var_data <- meta_colors[[var_name]]
		
		if (is.numeric(var_data)) {
			# Continuous variable - use gradient
			anno_col[[var_name]] <- colorRampPalette(c("blue", "white", "red"))(100)
		} else {
			# Categorical variable - assign distinct colors
			levels <- unique(as.character(var_data))
			levels <- levels[!is.na(levels)]
			
			n_levels <- length(levels)
			
			if (n_levels <= length(all_colors)) {
				colors <- all_colors[1:n_levels]
			} else {
				# Generate more colors if needed
				colors <- colorRampPalette(all_colors)(n_levels)
			}
			
			names(colors) <- levels
			anno_col[[var_name]] <- colors
		}
	}
	
	return(anno_col)
}

## Log Ratio to Fold Change ####

#' Convert Log Ratio to Fold Change
#'
#' @param x Numeric vector, log ratios
#' @param base Numeric, log base (default 2)
#'
#' @return Numeric vector, fold changes
#' @keywords internal
logratio2foldchange <- function(x, base = 2) {
	ifelse(x >= 0, base^x, -1 / (base^x))
}

## Utility: Make Names Safe ####

#' Make Factor Levels Syntactically Valid
#'
#' @param metadata Data frame
#' @param variable Character, variable name
#'
#' @return Data frame with cleaned variable
#' @keywords internal
clean_factor_levels <- function(metadata, variable) {
	if (!is.numeric(metadata[[variable]]) && !is.integer(metadata[[variable]])) {
		metadata[[variable]][!is.na(metadata[[variable]])] <- 
			make.names(metadata[[variable]][!is.na(metadata[[variable]])])
		metadata[[variable]] <- as.factor(metadata[[variable]])
	}
	return(metadata)
}
		
