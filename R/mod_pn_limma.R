# mod_pn_limma.R ####
# Limma Differential Expression Analysis Module

# UI Function ####

#' PN Limma Analysis Module - UI
#'
#' @param id Character string. Namespace identifier.
#' @export
mod_pn_limma_ui <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			box(
				title = "Limma Differential Expression Analysis",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				## ExpSet Info ####
				fluidRow(
					column(12, verbatimTextOutput(ns("eset_info")))
				),
				
				actionButton(ns('denoise_mod_debug'), 'Mod Debug', class = "btn-warning btn-sm"),
				
				p("Perform differential expression analysis using limma with optional covariates and comprehensive outputs."),
				
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
						helpText("Choose which matrix to use for analysis (exprs, cv, flag, etc.)")
					),
					column(
						width = 8,
						uiOutput(ns("assay_info_ui"))
					)
				),
				
				hr(),
				
				## Analysis Parameters ####
				fluidRow(
					column(3, 
								 selectInput(
								 	ns("variable"),
								 	"Primary Variable",
								 	choices = NULL
								 ),
								 tableOutput(ns("variable_summary")),
					# ),
					# column(3,
								 checkboxInput(
								 	ns("continuous"),
								 	"Continuous Variable?",
								 	value = FALSE
								 )
					),
					column(3,
								 conditionalPanel(
								 	condition = paste0("!input['", ns("continuous"), "']"),
								 	ns = ns,
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
								 	ns = ns,
								 	selectInput(
								 		ns("class2"),
								 		"Class 2 (Negative)",
								 		choices = NULL
								 	)
								 )
					),
					column(3,
								 selectInput(
								 	ns("covariates"),
								 	"Covariates",
								 	choices = NULL,
								 	multiple = TRUE
								 ),
								 tableOutput(ns("covariates_summary"))
					)
				),
				
				fluidRow(
					# column(3,
					# 			 selectInput(
					# 			 	ns("covariates"),
					# 			 	"Covariates",
					# 			 	choices = NULL,
					# 			 	multiple = TRUE
					# 			 ),
					# 			 tableOutput(ns("covariates_summary"))
					# ),
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
					)
				),
				
				## Analysis Options ####
				fluidRow(
					column(3,
								 radioButtons(
								 	ns("fc_direction"),
								 	"Fold Change Direction",
								 	choices = c(
								 		"Up-regulated (positive FC)" = "up",
								 		"Down-regulated (negative FC)" = "down",
								 		"Both directions" = "both"
								 	),
								 	selected = "up"
								 )
					),
					column(3,
								 br(),
								 checkboxInput(
								 	ns("plot_violins"),
								 	"Generate Violin Plots",
								 	value = TRUE
								 ),
								 checkboxInput(
								 	ns("eb_trend"),
								 	"eBayes Trend",
								 	value = TRUE
								 )
					),
					column(3,
								 br(),
								 checkboxInput(
								 	ns("eb_robust"),
								 	"eBayes Robust",
								 	value = TRUE
								 )
					),
					column(3,
								 br(),
								 actionButton(
								 	ns("run_limma"),
								 	"Run Limma Analysis",
								 	icon = icon("play"),
								 	class = "btn-primary btn-lg",
								 	style = "margin-top: 5px;"
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
							"Top Results",
							br(),
							DT::dataTableOutput(ns("limma_top_table"))
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
								)
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
								DT::dataTableOutput(ns("roc_table"))
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_roc"), "']"),
								h5("ROC analysis only available for categorical variables with significant results")
							)
						),
						
						tabPanel(
							"Violin Plots",
							br(),
							conditionalPanel(
								condition = paste0("output['", ns("has_violins"), "']"),
								uiOutput(ns("violin_plots_ui"))
							),
							conditionalPanel(
								condition = paste0("!output['", ns("has_violins"), "']"),
								h5("Violin plots only available for categorical variables with significant results")
							)
						)
					),
					
					hr(),
					
					### Download Buttons ####
					fluidRow(
						column(3,
									 downloadButton(ns("download_toptable"), "Download TopTable", class = "btn-success")
						),
						column(3,
									 downloadButton(ns("download_roc"), "Download ROC Results", class = "btn-success")
						),
						column(3,
									 downloadButton(ns("download_heatmaps"), "Download Heatmaps", class = "btn-success")
						),
						column(3,
									 downloadButton(ns("download_violins"), "Download Violin Plots", class = "btn-success")
						)
					)
				)
			)
		)
	)
}

# Server Function ####

#' PN Limma Analysis Module - Server
#'
#' @param id Character string. Namespace identifier.
#' @param eset Reactive or static. ExpressionSet object.
#' @param mode Character or reactive. "basic" or "advanced". Default "basic".
#' @param features List or reactive. Feature flags controlling functionality.
#' @param default_assay Character or reactive. Default assayData element to select (default "exprs").
#'
#' @return Reactive list containing limma results
#' @export
mod_pn_limma_server <- function(id, 
																eset,
																mode = "basic",
																features = list(),
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
		
		### Normalize Mode ####
		mode_reactive <- reactive({
			if (is.reactive(mode)) mode() else mode
		})
		
		### Normalize Features ####
		features_reactive <- reactive({
			if (is.reactive(features)) {
				f <- features()
			} else {
				f <- features
			}
			
			# Merge with defaults
			default_features <- list(
				generate_heatmaps = TRUE,
				generate_roc = TRUE,
				generate_violins = TRUE,
				allow_continuous = TRUE
			)
			
			for (name in names(default_features)) {
				if (is.null(f[[name]])) {
					f[[name]] <- default_features[[name]]
				}
			}
			
			return(f)
		})
		
		### Normalize Default Assay ####
		default_assay_reactive <- reactive({
			if (is.reactive(default_assay)) {
				default_assay()
			} else {
				default_assay
			}
		})
		
		## Setup ####
		
		### Reactive Values ####
		rv <- reactiveValues(
			limma_results = NULL,
			merged_results = NULL,
			exp_PN_AAbs = NULL,
			PN_AAbs = NULL,
			heatmap_data = NULL,
			violin_plots = NULL,
			metadata_filtered = NULL
		)
		
		## UI Population ####
		
		### Populate AssayData Choices ####
		observe({
			req(eset_current())
			
			assay_names <- Biobase::assayDataElementNames(eset_current())
			selected_assay <- default_assay_reactive()
			
			# Select default or first available
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
		observe({
			req(eset_current())
			
			metadata <- Biobase::pData(eset_current())
			col_names <- colnames(metadata)
			
			updateSelectInput(session, "variable", choices = col_names, selected = col_names[1])
			updateSelectInput(session, "covariates", choices = col_names)
			updateSelectInput(session, "add_anno", choices = col_names)
		})
		
		### Update Class Choices ####
		observe({
			req(eset_current(), input$variable)
			
			if (!input$continuous) {
				metadata <- Biobase::pData(eset_current())
				
				# Check variable exists
				if (!input$variable %in% colnames(metadata)) {
					return()
				}
				
				unique_vals <- unique(metadata[[input$variable]])
				
				updateSelectInput(session, "class1", choices = unique_vals, selected = unique_vals[1])
				updateSelectInput(session, "class2", choices = unique_vals, selected = unique_vals[min(2, length(unique_vals))])
			}
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
		### Variable Summary ####
		output$variable_summary <- renderTable({
			req(eset_current(), input$variable)
			
			metadata <- Biobase::pData(eset_current())
			
			if (input$variable %in% colnames(metadata)) {
				var_data <- metadata[[input$variable]]
				
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
					# Categorical - create clean table
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
			req(eset_current(), input$covariates)
			
			if (length(input$covariates) == 0) {
				return(NULL)
			}
			
			metadata <- Biobase::pData(eset_current())
			
			# Build summary for all selected covariates
			summary_list <- lapply(input$covariates, function(cov) {
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
		
		### Run Limma Analysis ####
		observeEvent(input$run_limma, {
			req(eset_current(), input$assay_data_select)
			
			showNotification("Running limma analysis...", type = "message", duration = NULL, id = "limma_running")
			
			tryCatch({
				# Get data from SELECTED assayData element
				# Get data from ExpressionSet
				eset_temp <- eset_current()
				metadata <- Biobase::pData(eset_temp)
				exprs_data <- Biobase::assayDataElement(eset_temp, input$assay_data_select)
				
				# Get parameters
				variable <- input$variable
				covariates <- input$covariates
				continuous <- input$continuous
				fc_cutoff <- input$fc_cutoff
				p_val <- input$p_val
				fc_direction <- input$fc_direction
				
				class1 <- if (!continuous) input$class1 else NULL
				class2 <- if (!continuous) input$class2 else NULL
				
				feature_select <- rownames(exprs_data)
				
				# Make names syntactically valid (required by limma)
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
				
				# Remove samples with NA values in the variable or covariates
				covariate_cols <- c(variable, covariates)
				if (length(covariate_cols) > 1) {
					na_rows <- which(rowSums(is.na(metadata[, covariate_cols])) > 0)
				} else {
					na_rows <- which(is.na(metadata[, covariate_cols]))
				}
				
				if (length(na_rows) > 0) {
					warning(paste("Removed", length(na_rows), "samples with NA values."))
					# ✅ Remove NA samples from BOTH metadata and expression data
					metadata <- metadata[-na_rows, ]
					exprs_data <- exprs_data[, rownames(metadata)]
				}
				
				# Validate we have enough samples after NA removal
				if (nrow(metadata) < 3) {
					removeNotification("limma_running")
					showNotification(
						paste("❌ Not enough samples after removing NAs:", nrow(metadata), 
									"samples remaining. Need at least 3 samples."),
						type = "error",
						duration = 10
					)
					return()
				}
				
				# Build model based on continuous vs categorical
				if (!continuous) {
					# === CATEGORICAL VARIABLE ===
					
					# Build design matrix
					if (!is.null(covariates) && length(covariates) > 0) {
						design_formula <- as.formula(paste0("~ 0 + ", paste(c(variable, covariates), collapse = " + ")))
					} else {
						design_formula <- as.formula(paste0("~ 0 + ", variable))
					}
					
					design <- model.matrix(design_formula, data = metadata)
					
					# Validate we have samples in both groups
					class1_clean <- make.names(class1)
					class2_clean <- make.names(class2)
					
					n_class1 <- sum(metadata[[variable]] == class1_clean, na.rm = TRUE)
					n_class2 <- sum(metadata[[variable]] == class2_clean, na.rm = TRUE)
					
					if (n_class1 < 2 || n_class2 < 2) {
						removeNotification("limma_running")
						showNotification(
							HTML(paste0(
								"❌ Not enough samples in each group:<br>",
								class1, ": ", n_class1, " samples<br>",
								class2, ": ", n_class2, " samples<br>",
								"Need at least 2 samples per group."
							)),
							type = "error",
							duration = 10
						)
						return()
					}
					
					# Build contrast
					contrast_string <- paste0(variable, class1_clean, "-", variable, class2_clean)
					user_contrast <- makeContrasts(contrasts = contrast_string, levels = design)
					
					# ✅ FIT MODEL: Use exprs_data matrix directly (already subset correctly above)
					# DON'T use eset_temp because it still has the original 87 samples
					fit <- lmFit(exprs_data, design)
					fit2 <- contrasts.fit(fit, user_contrast)
					fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
					
				} else {
					# === CONTINUOUS VARIABLE ===
					
					# Build design matrix
					if (!is.null(covariates) && length(covariates) > 0) {
						design_formula <- as.formula(paste0("~ ", paste(c(variable, covariates), collapse = " + ")))
					} else {
						design_formula <- as.formula(paste0("~ ", variable))
					}
					
					design <- model.matrix(design_formula, data = metadata)
					
					# ✅ FIT MODEL: Use exprs_data matrix directly
					fit <- lmFit(exprs_data, design)
					fit2 <- contrasts.fit(fit, coeff = 2)
					fit2 <- eBayes(fit2, trend = input$eb_trend, robust = input$eb_robust)
				}
				
				# Get topTable
				TT <- topTable(fit2, number = Inf)
				TT <- TT[feature_select, ]
				TT <- TT %>% arrange(P.Value)
				
				# Store full results
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
					fc_direction = fc_direction
				)
				
				rv$metadata_filtered <- metadata
				
				# Check for significant results
				sig_rows_all <- which(TT$P.Value < p_val & abs(TT$logFC) > log2(fc_cutoff))
				
				if (length(sig_rows_all) < 1) {
					removeNotification("limma_running")
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
					removeNotification("limma_running")
					
					direction_text <- switch(fc_direction,
																	 "up" = "up-regulated (positive FC)",
																	 "down" = "down-regulated (negative FC)",
																	 "both" = "in either direction")
					
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
				
				# Calculate expected PN AAbs
				sig_AAbs <- rownames(TT_filtered[sig_rows, ])
				n_sig <- length(sig_AAbs)
				rv$exp_PN_AAbs <- max(1, n_sig - 2):min(nrow(exprs_data), n_sig + 4)
				rv$PN_AAbs <- sig_AAbs
				
				# Generate additional outputs
				feat <- features_reactive()
				
				# Prepare filtered metadata
				if (!continuous) {
					meta_filtered <- metadata[metadata[[variable]] %in% c(class1_clean, class2_clean), ]
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
				if (feat$generate_heatmaps && nrow(data_sig) > 2) {
					rv$heatmap_data <- generate_heatmap_data(
						data_sig = data_sig,
						data_sig_RC = data_sig_RC,
						meta_2 = meta_2,
						variable = variable,
						continuous = continuous,
						class1 = class1_clean,
						class2 = class2_clean,
						add_anno = input$add_anno
					)
				}
				
				# Generate ROC
				if (feat$generate_roc && !continuous && nrow(data_sig) > 0) {
					rv$merged_results <- generate_roc_results(
						exprs_data = exprs_data,
						data_sig = data_sig,
						meta_2 = meta_2,
						TT = TT_filtered,
						variable = variable,
						class1 = class1_clean,
						class2 = class2_clean,
						sig_rows = sig_rows,
						fc_direction = fc_direction
					)
				}
				
				# Generate violins
				if (feat$generate_violins && input$plot_violins && !continuous && nrow(data_sig) > 0) {
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
				
				removeNotification("limma_running")
				
				direction_text <- switch(fc_direction,
																 "up" = "up-regulated",
																 "down" = "down-regulated",
																 "both" = "in both directions")
				
				showNotification(
					HTML(paste0(
						"<strong>✅ Limma analysis complete!</strong><br>",
						"Found ", n_sig, " significant ", direction_text, " features<br>",
						"Expected range: ", min(rv$exp_PN_AAbs), "-", max(rv$exp_PN_AAbs)
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				removeNotification("limma_running")
				showNotification(
					paste("❌ Limma analysis failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
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
		
		### Summary ####
		output$limma_summary <- renderPrint({
			req(rv$limma_results)
			
			TT <- rv$limma_results$topTable
			TT_full <- rv$limma_results$topTable_full
			fc_direction <- rv$limma_results$fc_direction
			
			sig_count <- sum(TT$P.Value < rv$limma_results$p_val & abs(TT$logFC) > log2(rv$limma_results$fc_cutoff))
			sig_count_full <- sum(TT_full$P.Value < rv$limma_results$p_val & abs(TT_full$logFC) > log2(rv$limma_results$fc_cutoff))
			
			cat("Limma Analysis Summary\n")
			cat("======================\n\n")
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
															 "both" = "Both directions")
			
			cat("FC Direction Filter:", direction_text, "\n\n")
			cat("Significant Features (filtered):", sig_count, "\n")
			
			if (fc_direction != "both") {
				cat("Total Significant (all directions):", sig_count_full, "\n")
			}
			
			if (!is.null(rv$exp_PN_AAbs)) {
				cat("\nExpected PN AAb Count:", paste(rv$exp_PN_AAbs, collapse = ", "), "\n")
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
			
			sig_table %>%
				select(logFC, FC, AveExpr, t, P.Value, adj.P.Val, B) %>%
				DT::datatable(
					options = list(pageLength = 20, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = c("logFC", "FC", "AveExpr", "t", "B"), digits = 3) %>%
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
			
			rv$merged_results %>%
				DT::datatable(
					options = list(pageLength = 20, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = c("logFC", "FC", "AUC", "Sensitivity", "Specificity", "Optimal.Cutoff"), digits = 3)
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
		
		### Download TopTable ####
		output$download_toptable <- downloadHandler(
			filename = function() {
				direction <- rv$limma_results$fc_direction
				paste0("limma_full_table_", direction, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$limma_results)
				write.csv(rv$limma_results$topTable, file, row.names = TRUE)
			}
		)
		
		### Download ROC ####
		output$download_roc <- downloadHandler(
			filename = function() {
				direction <- rv$limma_results$fc_direction
				paste0("limma_ROC_results_", direction, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$merged_results)
				write.csv(rv$merged_results, file, row.names = TRUE)
			}
		)
		
		### Download Heatmaps ####
		output$download_heatmaps <- downloadHandler(
			filename = function() {
				direction <- rv$limma_results$fc_direction
				paste0("heatmaps_", direction, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
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
				direction <- rv$limma_results$fc_direction
				paste0("violin_plots_", direction, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
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
				exp_PN_AAbs = rv$exp_PN_AAbs,
				PN_AAbs = rv$PN_AAbs,
				limma_results = rv$limma_results,
				merged_results = rv$merged_results,
				heatmap_data = rv$heatmap_data,
				violin_plots = rv$violin_plots
			)
		}))
	})
}

# Helper Functions ####

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
generate_roc_results <- function(exprs_data, data_sig, meta_2, TT, variable, 
																 class1, class2, sig_rows, fc_direction = "both") {
	
	# Run ROC analysis based on direction
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
		ROC_results_up <- data.frame(ROC_results_up)
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
		ROC_results_down <- data.frame(ROC_results_down)
	}
	
	# Merge with limma results
	limma_results <- TT[rownames(data_sig), ]
	limma_results$Protein <- rownames(limma_results)
	
	if (fc_direction == "up") {
		merged_results <- left_join(limma_results, ROC_results_up, by = "Protein")
	} else if (fc_direction == "down") {
		merged_results <- left_join(limma_results, ROC_results_down, by = "Protein")
	} else {
		limma_up <- limma_results[limma_results$logFC > 0, ]
		limma_down <- limma_results[limma_results$logFC < 0, ]
		
		merged_up <- left_join(limma_up, ROC_results_up, by = "Protein")
		merged_down <- left_join(limma_down, ROC_results_down, by = "Protein")
		
		merged_results <- rbind(merged_up, merged_down)
	}
	
	rownames(merged_results) <- merged_results$Protein
	
	# Clean up columns
	merged_results <- merged_results %>%
		dplyr::select(-c(AveExpr, B, t, Protein)) %>%
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