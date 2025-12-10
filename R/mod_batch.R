
# BATCH COMBINED #####
#' Combined Batch Analysis Module - UI
#'
#' Combined ANOVA batch effects and distribution confounding tests
#'
#' @param id Module namespace ID
#' @param show_auto_run_toggle Logical. Show auto-run toggle switch (default TRUE).
#' @param debug Show debug button
#' @export
mod_batch_combined_analysis_ui <- function(id, 
																					 show_auto_run_toggle = TRUE,
																					 debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Effect & Confounding Analysis",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				# ✅ MAIN CONTENT: Plot (always visible at top)
				shinycssloaders::withSpinner(
					plotOutput(ns("combined_plot"), height = "700px"),
					type = 4,
					color = "#337ab7"
				),
				
				# ✅ Download button overlaid on top-right (below box collapse button)
				div(
					style = "position:  absolute; top: 45px; right: 5px; z-index: 1000;",
					downloadButton(
						ns("download_plot"),
						label = NULL,
						icon = icon("download"),
						class = "btn-primary btn-sm",
						style = "opacity: 0.9;",
						title = "Download plot as PNG"
					)
				),
				hr(),
				
				# ✅ Advanced Options button at bottom
				fluidRow(
					column(
						width = 12,
						actionButton(
							ns("toggle_advanced"),
							"Advanced Options & Settings",
							icon = icon("cog"),
							class = "btn-default",
							style = "width:  100%; margin-bottom: 15px;"
						)
					)
				),
				
				# ✅ Collapsible Advanced Options Panel
				# ✅ Collapsible Advanced Options Panel
				conditionalPanel(
					condition = "input.toggle_advanced % 2 == 1",
					ns = ns,
					
					box(
						#title = "Advanced Options",
						width = 12,
						#status = "warning",
						#solidHeader = TRUE,
						collapsible = FALSE,
						
						# ✅ 1. INTERPRETATION GUIDE (collapsed by default)
						box(
							title = "Interpretation Guide",
							width = 12,
							status = "info",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							p("Each batch column is plotted showing: "),
							tags$ul(
								tags$li(strong("Y-axis (Batch Effect):"), "-log10(p-value) - higher = stronger batch effect"),
								tags$li(strong("X-axis (Confounding):"), "-log10(Fisher p-value) - higher = more confounded with sample groups"),
								tags$li(strong("Color:"), "Significance category"),
								tags$li(strong("Size:"), "Number of groups in batch column")
							),
							
							h4("Quadrant Interpretation: "),
							tags$ul(
								tags$li(
									strong("Top-Right (High Batch + High Confounding):"), 
									tags$span("⚠️ CRITICAL", style = "color: #d32f2f; font-weight: bold;"),
									" - Strong batch effect AND confounded.  Correction may remove real biology!"
								),
								tags$li(
									strong("Top-Left (High Batch + Low Confounding):"), 
									tags$span("✅ SAFE TO CORRECT", style = "color: #388e3c; font-weight:  bold;"),
									" - Strong batch effect but evenly distributed.  Correction recommended."
								),
								tags$li(
									strong("Bottom-Right (Low Batch + High Confounding):"), 
									tags$span("⚠️ CAUTION", style = "color: #f57c00; font-weight: bold;"),
									" - Uneven distribution but weak effect. May indicate biological correlation."
								),
								tags$li(
									strong("Bottom-Left (Low Batch + Low Confounding):"), 
									tags$span("✅ NO ACTION NEEDED", style = "color:  #757575; font-weight:  bold;"),
									" - No significant batch effect or confounding."
								)
							)
						),
						
						# ✅ 2. RESULTS TABLE (collapsed by default)
						box(
							title = "Results Table",
							width = 12,
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							# ✅ Download button at top of table box
							fluidRow(
								column(
									width = 12,
									div(
										style = "text-align: right; margin-bottom: 10px;",
										downloadButton(
											ns("download_table"),
											"Download Table (TSV)",
											icon = icon("download"),
											class = "btn-primary btn-sm"
										)
									)
								)
							),
							
							uiOutput(ns("analysis_status")),
							
							helpText(
								strong("PERMANOVA: "), "Tests if groups have different expression profiles (recommended).",
								br(),
								strong("Beta Dispersion:"), "Tests if groups have different variances."
							),
							
							hr(),
							
							DTOutput(ns("combined_table"))
						),
						
						# ✅ 3. SETTINGS (NOT collapsible - always open)
						box(
							title = "Analysis Settings",
							width = 12,
							status = "primary",
							solidHeader = TRUE,
							collapsible = FALSE,
							
							# Row 1: Test Method, Permutations, Distance
							fluidRow(
								column(
									width = 4,
									selectInput(
										ns("test_method"),
										"Statistical Test Method:",
										choices = c(
											"PERMANOVA (adonis2)" = "permanova",
											"Beta Dispersion (betadisper)" = "betadisper"
										),
										selected = "permanova"
									),
									helpText(
										icon("info-circle", style = "color:  #337ab7;"),
										tags$small("PERMANOVA tests group differences (recommended)")
									)
								),
								column(
									width = 4,
									numericInput(
										ns("n_permutations"),
										"Number of Permutations:",
										value = 199,
										min = 99,
										max = 9999,
										step = 100
									),
									helpText(
										icon("info-circle", style = "color: #337ab7;"),
										tags$small("Lower = faster, Higher = more accurate")
									)
								),
								column(
									width = 4,
									selectInput(
										ns("distance_method"),
										"Distance Method:",
										choices = c(
											"Euclidean" = "euclidean",
											"Manhattan" = "manhattan",
											"Canberra" = "canberra",
											"Bray-Curtis" = "bray",
											"Kulczynski" = "kulczynski",
											"Jaccard" = "jaccard",
											"Gower" = "gower",
											"Morisita" = "morisita",
											"Horn" = "horn",
											"Mountford" = "mountford",
											"Raup" = "raup",
											"Binomial" = "binomial",
											"Chao" = "chao",
											"Cao" = "cao"
										),
										selected = "euclidean"
									),
									helpText(
										icon("info-circle", style = "color: #337ab7;"),
										tags$small("Euclidean is standard for proteomics")
									)
								)
							),
							
							hr(),
							
							# Row 2: By Method, Parallel Toggle, Auto-run Toggle
							fluidRow(
								column(
									width = 4,
									selectInput(
										ns("permanova_by"),
										"Sequential Test (by):",
										choices = c(
											"Margin (each term independently)" = "margin",
											"Terms (sequential order)" = "terms",
											"NULL (overall test)" = "NULL"
										),
										selected = "margin"
									),
									helpText(
										icon("info-circle", style = "color: #337ab7;"),
										tags$small("Margin tests each batch column independently")
									)
								),
								column(
									width = 4,
									div(
										style = "padding-top: 5px;",
										shinyWidgets:: materialSwitch(
											inputId = ns("use_parallel"),
											label = "Enable Parallel Processing",
											value = TRUE,
											status = "success"
										)
									),
									helpText(
										icon("bolt", style = "color: #337ab7;"),
										tags$small("Uses optimal number of CPU cores")
									)
								),
								if (show_auto_run_toggle) {
									column(
										width = 4,
										div(
											style = "padding-top: 5px;",
											shinyWidgets::materialSwitch(
												inputId = ns("auto_run_analysis"),
												label = "Auto-Run Analysis",
												value = TRUE,
												status = "success"
											)
										),
										helpText(
											icon("sync", style = "color: #337ab7;"),
											tags$small("Re-run when parameters change")
										)
									)
								}
							),
							
							# Manual run UI (shows when auto-run is OFF)
							if (show_auto_run_toggle) {
								fluidRow(
									column(
										width = 12,
										uiOutput(ns("manual_run_ui"))
									)
								)
							}
						),
						
						# ✅ 4. DEBUG BUTTON (if enabled)
						if (debug) {
								actionButton(
									ns("debug"),
									"Debug:  mod_batch_combined_analysis",
									icon = icon("bug"),
									class = "btn-warning",
									style = "width: 100%;"
								)
						
						}
					)
				)
			)
		)
	)
}

#' Combined Batch Analysis Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character Sample grouping column name
#' @param batch_columns Reactive character vector Batch columns to test
#' @param debug Enable debug mode
#' @export
mod_batch_combined_analysis_server <- function(id,
																							 eset,
																							 sample_group_column,
																							 batch_columns,
																							 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		

		# Render debug button
		# output$debug_ui <- renderUI({
		# 	if (debug) {
		# 		tagList(
		# 			actionButton(
		# 				session$ns("debug"),
		# 				"Debug",
		# 				icon = icon("bug"),
		# 				class = "btn-warning btn-sm"
		# 			),
		# 			hr()
		# 		)
		# 	}
		# })
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Combined Batch Analysis Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		
		# ✅ Track analysis state
		analysis_running <- reactiveVal(FALSE)
		analysis_trigger <- reactiveVal(0)  # Increment to trigger re-run
		
		# ✅ Auto-run status text
		output$auto_run_status <- renderText({
			if (input$auto_run_analysis) {
				"ON"
			} else {
				"OFF"
			}
		})
		
		output$manual_run_ui <- renderUI({
			if (! isTRUE(input$auto_run_analysis)) {
				tagList(
					hr(),
					
					actionButton(
						ns("manual_run_analysis"),
						"Run Batch Analysis",
						icon = icon("play"),
						class = "btn-primary",
						style = "width: 100%;"
					),
					
					tags$div(
						style = "margin-top: 10px;",
						uiOutput(ns("manual_run_status"))
					)
				)
			}
		})
		
		
		# ✅ Manual run status
		output$manual_run_status <- renderUI({
			if (analysis_running()) {
				div(
					class = "alert alert-info",
					style = "margin-bottom: 0;",
					icon("spinner", class = "fa-spin"),
					strong(" Running analysis.. .", style = "margin-left: 5px;")
				)
			} else if (analysis_trigger() > 0) {
				div(
					class = "alert alert-success",
					style = "margin-bottom: 0;",
					icon("check-circle"),
					strong(" Analysis complete!", style = "margin-left:  5px;")
				)
			} else {
				div(
					class = "alert alert-warning",
					style = "margin-bottom: 0;",
					icon("pause-circle"),
					strong(" Analysis paused.  Click 'Run Batch Analysis' to continue.", style = "margin-left: 5px;")
				)
			}
		})
		
		# ✅ Manual run button
		observeEvent(input$manual_run_analysis, {
			analysis_trigger(analysis_trigger() + 1)
		})
		
		# # ✅ Auto-run trigger (when data/params change and toggle is ON)
		# observe({
		# 	req(input$auto_run_analysis)  # Only if toggle is ON
		# 	req(eset())
		# 	req(sample_group_column())
		# 	req(batch_columns())
		# 	input$test_method  # Dependency
		# 	
		# 	# Trigger analysis
		# 	analysis_trigger(analysis_trigger() + 1)
		# })
		observe({
			message("Toggle state:  ", input$auto_run_analysis)
			message("Analysis trigger: ", analysis_trigger())
		})
		# ✅ Auto-run trigger (when data/params change and toggle is ON)
		observeEvent(list(eset(), 
											sample_group_column(), 
											batch_columns(), 
											input$test_method,
											input$n_permutations,
											input$permanova_by,
											input$use_parallel), {
			# Check toggle state - if OFF, don't trigger
			if (isTRUE(input$auto_run_analysis)) {
				isolate({
					analysis_trigger(analysis_trigger() + 1)
				})
			}
		}, ignoreInit = TRUE, ignoreNULL = TRUE)

		
		# Run combined analysis ####
		combined_results <- reactive({
			req(analysis_trigger() > 0)
			
			# ✅ Stop if toggle is OFF
			if (!isTRUE(input$auto_run_analysis)) {
				return(NULL)
			}
			
			req(eset())
			req(sample_group_column())
			req(batch_columns())
			
			analysis_running(TRUE)  # ✅ Set running state
			on.exit(analysis_running(FALSE))  # ✅ Clear on exit
			
			ExpSet <- eset()
			m <- Biobase::exprs(ExpSet)
			meta <- Biobase::pData(ExpSet)
			sample_group <- sample_group_column()
			# if('ComBat' %in% colnames(meta)){
			# 	batch_cols <- c(batch_columns(),'ComBat')
			# }else{
			batch_cols <- batch_columns()
			#}
			test_method <- input$test_method  # Get selected method
			
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Validate columns
			if (!sample_group %in% colnames(meta)) {
				warning("Sample group column not found")
				return(NULL)
			}
			
			batch_cols <- intersect(batch_cols, colnames(meta))
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Run analyses based on selected method
			results_list <- list()
			
			for (batch_col in batch_cols) {
				result <- tryCatch({
					# 1 Batch effect test (method-dependent)
					if (test_method == "permanova") {
						# Use PERMANOVA
						perm_result <- permanova_function(m, meta, batch_col, 
																							method = input$distance_method,
																							permutations = input$n_permutations,
																							by = if (input$permanova_by == "NULL") NULL else input$permanova_by,
																							use_parallel = input$use_parallel)
						result_df <- as.data.frame(perm_result)
						batch_p <- result_df$`Pr(>F)`[1]
					} else {
						# Use betadisper + ANOVA
						anova_result <- anova_betadine_function(m, meta, batch_col)
						anova_tidy <- broom::tidy(anova_result)
						batch_p <- anova_tidy$p.value[1]
					}
					
					# 2Fisher's test for confounding (same for both methods)
					tab <- table(meta[[sample_group]], meta[[batch_col]])
					fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
					fisher_p <- fisher_result$p.value
					
					# 3 Additional info
					n_batch_groups <- length(unique(meta[[batch_col]]))
					n_sample_groups <- length(unique(meta[[sample_group]]))
					
					list(
						batch_column = batch_col,
						batch_p = batch_p,
						fisher_p = fisher_p,
						n_batch_groups = n_batch_groups,
						n_sample_groups = n_sample_groups,
						interpretation = interpret_combined(batch_p, fisher_p)
					)
				}, error = function(e) {
					warning(sprintf("Analysis failed for '%s': %s", batch_col, e$message))
					NULL
				})
				
				if (!is.null(result)) {
					results_list[[batch_col]] <- result
				}
			}
			
			if (length(results_list) == 0) {
				return(NULL)
			}
			
			# Convert to data frame
			method_label <- ifelse(test_method == "permanova", "PERMANOVA_p_value", "ANOVA_p_value")
			
			df <- data.frame(
				Batch_Column = sapply(results_list, function(x) x$batch_column),
				Batch_p_value = signif(sapply(results_list, function(x) x$batch_p), 3),
				Fisher_p_value = signif(sapply(results_list, function(x) x$fisher_p), 3),
				Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
				Interpretation = sapply(results_list, function(x) x$interpretation),
				stringsAsFactors = FALSE
			)
			
			# Rename column based on method
			colnames(df)[2] <- method_label
			
			df
		})
		
		# Interpretation helper function ####
		interpret_combined <- function(anova_p, fisher_p) {
			batch_sig <- anova_p < 0.05
			confound_sig <- fisher_p < 0.05
			
			if (batch_sig && confound_sig) {
				"⚠️ CRITICAL: Batch effect + Confounding"
			} else if (batch_sig && !confound_sig) {
				"✅ SAFE: Batch effect only"
			} else if (!batch_sig && confound_sig) {
				"⚠️ CAUTION: Confounding only"
			} else {
				"✅ OK: No issues"
			}
		}
		
		# Status message ####
		output$analysis_status <- renderUI({
			req(sample_group_column())
			req(batch_columns())
			
			batch_cols <- batch_columns()
			
			if (length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No batch columns selected")
					)
				)
			}
			
			if (is.null(combined_results())) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Analysis failed")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Analysis complete: '%s' vs %d batch columns", 
											 sample_group_column(), length(batch_cols)))
			)
		})
		
		# Combined table ####
		# Combined table
		output$combined_table <- renderDT({
			req(combined_results())
			
			df <- combined_results()
			
			# Get the batch effect column name dynamically
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "Combined batch effect and confounding analysis"
			) %>%
				formatStyle(
					batch_col_name,  # Use dynamic column name
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				) %>%
				formatStyle(
					'Fisher_p_value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				)
		})
		
		
		# Combined visualization ####
		output$combined_plot <- renderPlot({
			req(combined_results())
			
			df <- combined_results()
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Dynamically get the batch effect column name
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			method_type <- input$test_method
			
			# Prepare plot data
			df_plot <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%  # Rename to generic name
				mutate(
					neg_log_batch = -log10(batch_p),
					neg_log_fisher = -log10(Fisher_p_value),
					batch_sig = batch_p < 0.05,
					confound_sig = Fisher_p_value < 0.05,
					category = case_when(
						batch_sig & confound_sig ~ "Critical: Both significant",
						batch_sig & !confound_sig ~ "Safe: Batch effect only",
						!batch_sig & confound_sig ~ "Caution: Confounding only",
						TRUE ~ "OK: Neither significant"
					)
				)
			
			# Calculate dynamic positions for annotations
			ymax <- max(c(max(df_plot$neg_log_batch), -log10(0.05) * 2))
			xmax <- max(c(max(df_plot$neg_log_fisher), -log10(0.05) * 2))
			a <- -log10(0.05)
			low <- a / 2
			high <- a + low
			xhigh <- ((xmax - a) / 2) + a
			yhigh <- ((ymax - a) / 2) + a
			
			# Create scatter plot
			ggplot(df_plot, aes(x = neg_log_fisher, y = neg_log_batch)) +
				# Quadrant lines
				geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
				
				# Quadrant labels with dynamic positioning
				annotate("text", x = low, y = yhigh, 
								 label = "Low Confounding\nHigh Batch Effect\n✅ SAFE TO CORRECT", 
								 hjust = 0, vjust = 1, color = "darkgreen", size = 3.5, fontface = "bold") +
				annotate("text", x = xhigh, y = yhigh, 
								 label = "High Confounding\nHigh Batch Effect\n⚠️ CRITICAL", 
								 hjust = 1, vjust = 1, color = "darkred", size = 3.5, fontface = "bold") +
				annotate("text", x = low, y = low, 
								 label = "✅ NO ACTION\nNEEDED", 
								 hjust = 0, vjust = 0, color = "gray50", size = 3.5) +
				annotate("text", x = xhigh, y = low, 
								 label = "⚠️ CAUTION\nPossible biological\ncorrelation", 
								 hjust = 1, vjust = 0, color = "darkorange", size = 3.5) +
				
				# Points
				geom_point(aes(color = category, size = Batch_Groups), alpha = 0.7) +
				
				# Labels
				ggrepel::geom_text_repel(
					aes(label = Batch_Column),
					size = 3.5,
					max.overlaps = 20,
					box.padding = 0.5
				) +
				
				# Colors
				scale_color_manual(
					values = c(
						"Critical: Both significant" = "#d32f2f",
						"Safe: Batch effect only" = "#388e3c",
						"Caution: Confounding only" = "#f57c00",
						"OK: Neither significant" = "#757575"
					)
				) +
				
				# Size
				scale_size_continuous(range = c(3, 10)) +
				
				# Labels
				labs(
					title = sprintf("Batch Effect (%s) vs Confounding: '%s'", 
													ifelse(method_type == "permanova", "PERMANOVA", "Beta Dispersion"),
													sample_group_column()),
					subtitle = "Dashed lines indicate p = 0.05 threshold",
					x = "Confounding with Sample Groups\n-log10(Fisher's p-value) →",
					y = sprintf("← Batch Effect on Expression\n-log10(%s p-value)", 
											ifelse(method_type == "permanova", "PERMANOVA", "ANOVA")),
					color = "Category",
					size = "Number of\nBatch Groups"
				) +
				
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					legend.box = "vertical",
					panel.grid.minor = element_blank(),
					plot.title = element_text(face = "bold"),
					axis.title = element_text(face = "bold")
				) + 
				coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax))
		})
		
		# })
		
		
		# ✅ Download plot as PNG
		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("batch_confounding_analysis_", Sys.Date(), ".png")
			},
			content = function(file) {
				req(combined_results())
				
				df <- combined_results()
				
				if (nrow(df) == 0) {
					stop("No data to plot")
				}
				
				# Dynamically get the batch effect column name
				batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
				method_type <- input$test_method
				
				# Prepare plot data
				df_plot <- df %>%
					rename(batch_p = !!sym(batch_col_name)) %>%
					mutate(
						neg_log_batch = -log10(batch_p),
						neg_log_fisher = -log10(Fisher_p_value),
						batch_sig = batch_p < 0.05,
						confound_sig = Fisher_p_value < 0.05,
						category = case_when(
							batch_sig & confound_sig ~ "Critical: Both significant",
							batch_sig & !confound_sig ~ "Safe:  Batch effect only",
							!batch_sig & confound_sig ~ "Caution: Confounding only",
							TRUE ~ "OK: Neither significant"
						)
					)
				
				# Calculate dynamic positions for annotations
				ymax <- max(c(max(df_plot$neg_log_batch), -log10(0.05) * 2))
				xmax <- max(c(max(df_plot$neg_log_fisher), -log10(0.05) * 2))
				a <- -log10(0.05)
				low <- a / 2
				high <- a + low
				xhigh <- ((xmax - a) / 2) + a
				yhigh <- ((ymax - a) / 2) + a
				
				# Create plot
				p <- ggplot(df_plot, aes(x = neg_log_fisher, y = neg_log_batch)) +
					# Quadrant lines
					geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
					geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
					
					# Quadrant labels with dynamic positioning
					annotate("text", x = low, y = yhigh, 
									 label = "Low Confounding\nHigh Batch Effect\n✅ SAFE TO CORRECT", 
									 hjust = 0, vjust = 1, color = "darkgreen", size = 3.5, fontface = "bold") +
					annotate("text", x = xhigh, y = yhigh, 
									 label = "High Confounding\nHigh Batch Effect\n⚠️ CRITICAL", 
									 hjust = 1, vjust = 1, color = "darkred", size = 3.5, fontface = "bold") +
					annotate("text", x = low, y = low, 
									 label = "✅ NO ACTION\nNEEDED", 
									 hjust = 0, vjust = 0, color = "gray50", size = 3.5) +
					annotate("text", x = xhigh, y = low, 
									 label = "⚠️ CAUTION\nPossible biological\ncorrelation", 
									 hjust = 1, vjust = 0, color = "darkorange", size = 3.5) +
					
					# Points
					geom_point(aes(color = category, size = Batch_Groups), alpha = 0.7) +
					
					# Labels
					ggrepel::geom_text_repel(
						aes(label = Batch_Column),
						size = 3.5,
						max.overlaps = 20,
						box.padding = 0.5
					) +
					
					# Colors
					scale_color_manual(
						values = c(
							"Critical: Both significant" = "#d32f2f",
							"Safe: Batch effect only" = "#388e3c",
							"Caution: Confounding only" = "#f57c00",
							"OK: Neither significant" = "#757575"
						)
					) +
					
					# Size
					scale_size_continuous(range = c(3, 10)) +
					
					# Labels
					labs(
						title = sprintf("Batch Effect (%s) vs Confounding: '%s'", 
														ifelse(method_type == "permanova", "PERMANOVA", "Beta Dispersion"),
														sample_group_column()),
						subtitle = "Dashed lines indicate p = 0.05 threshold",
						x = "Confounding with Sample Groups\n-log10(Fisher's p-value) →",
						y = sprintf("← Batch Effect on Expression\n-log10(%s p-value)", 
												ifelse(method_type == "permanova", "PERMANOVA", "ANOVA")),
						color = "Category",
						size = "Number of\nBatch Groups"
					) +
					
					theme_minimal(base_size = 14) +
					theme(
						legend.position = "bottom",
						legend.box = "vertical",
						panel.grid.minor = element_blank(),
						plot.title = element_text(face = "bold"),
						axis.title = element_text(face = "bold")
					) + 
					coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax))
				
				# Save as high-res PNG
				ggsave(
					filename = file,
					plot = p,
					width = 14,
					height = 10,
					dpi = 300,
					bg = "white"
				)
			}
		)
		
		# ✅ Download table as TSV
		output$download_table <- downloadHandler(
			filename = function() {
				test_method <- input$test_method
				paste0("batch_confounding_results_", test_method, "_", Sys.Date(), ".tsv")
			},
			content = function(file) {
				req(combined_results())
				
				# Get the results data
				results_df <- combined_results()
				
				# Write as tab-separated file
				data.table::fwrite(
					results_df,
					file = file,
					sep = "\t",
					row.names = FALSE
				)
			}
		)
		
		# Return results
		return(list(
			results = combined_results,
			auto_run_enabled = reactive(input$auto_run_analysis)  # ✅ ADD
		))
	})
}

#. #####
# BATCH DISTRIBUTION #####
#' Batch Distribution Testing Module - UI
#'
#' Tests if sample groups are evenly distributed across batch columns
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_distribution_test_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Sample Group Distribution Across Batch Columns",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Tests whether the selected sample grouping column is evenly distributed across batch columns."),
				p("Uses Fisher's exact test to detect uneven distribution (potential confounding). "),
				
				uiOutput(ns("debug_ui")),
				
				uiOutput(ns("test_status")),
				
				hr(),
				
				h4("Distribution Test Results"),
				DTOutput(ns("distribution_table")),
				# 	)
				# ),
				
				fluidRow(
					box(
						title = "Visualization",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						collapsible = TRUE,
						
						plotOutput(ns("distribution_plot"), height = "600px")
					)
				)
			)
		)
	)
}

#' Batch Distribution Testing Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character.   Sample grouping column name
#' @param batch_columns Reactive character vector.  Batch column names to test
#' @param debug Enable debug mode
#' @export
mod_batch_distribution_test_server <- function(id, 
																							 eset, 
																							 sample_group_column,
																							 batch_columns,
																							 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Render debug button
		output$debug_ui <- renderUI({
			if (debug) {
				tagList(
					actionButton(
						session$ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					),
					hr()
				)
			}
		})
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Batch Distribution Test Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • sample_group_column() - Sample grouping column")
				message("  • batch_columns() - Batch columns to test")
				message("  • distribution_results() - Test results")
				message("\nUseful commands:")
				message("  sample_group_column()")
				message("  batch_columns()")
				message("  str(distribution_results())")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Run distribution tests
		distribution_results <- reactive({
			req(eset())
			req(sample_group_column())
			req(batch_columns())
			
			meta <- Biobase::pData(eset())
			sample_group <- sample_group_column()
			batch_cols <- batch_columns()
			
			# Need at least one batch column
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Validate columns exist
			if (! sample_group %in% colnames(meta)) {
				warning("Sample group column not found in metadata")
				return(NULL)
			}
			
			missing_cols <- setdiff(batch_cols, colnames(meta))
			if (length(missing_cols) > 0) {
				warning("Batch columns not found: ", paste(missing_cols, collapse = ", "))
				batch_cols <- intersect(batch_cols, colnames(meta))
			}
			
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Test each batch column
			results_list <- list()
			
			for (batch_col in batch_cols) {
				result <- tryCatch({
					# Create contingency table
					tab <- table(meta[[sample_group]], meta[[batch_col]])
					
					# Calculate percentage distribution
					pct_dist <- apply(tab, 1, function(x) x/sum(x)*100)
					
					# Fisher's exact test with simulation for large tables
					fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
					
					# Chi-square test (if possible)
					chisq_result <- tryCatch({
						chisq.test(tab, simulate.p.value = TRUE, B = 10000)
					}, error = function(e) NULL)
					
					list(
						batch_column = batch_col,
						n_sample_groups = nrow(tab),
						n_batch_groups = ncol(tab),
						fisher_p = fisher_result$p.value,
						chisq_p = if (! is.null(chisq_result)) chisq_result$p.value else NA,
						contingency_table = tab,
						percent_distribution = pct_dist
					)
				}, error = function(e) {
					warning(sprintf("Distribution test failed for '%s': %s", batch_col, e$message))
					NULL
				})
				
				if (!is.null(result)) {
					results_list[[batch_col]] <- result
				}
			}
			
			if (length(results_list) == 0) {
				return(NULL)
			}
			
			# Convert to data frame for table display
			df <- data.frame(
				Batch_Column = sapply(results_list, function(x) x$batch_column),
				Sample_Groups = sapply(results_list, function(x) x$n_sample_groups),
				Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
				Fisher_p_value = signif(sapply(results_list, function(x) x$fisher_p), 3),
				ChiSq_p_value = signif(sapply(results_list, function(x) x$chisq_p), 3),
				stringsAsFactors = FALSE
			)
			
			list(
				df = df,
				details = results_list
			)
		})
		
		# Status message
		output$test_status <- renderUI({
			req(sample_group_column())
			req(batch_columns())
			
			batch_cols <- batch_columns()
			
			if (length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No batch columns selected"),
						p("Please select batch columns in the Batch Column Selection tab.")
					)
				)
			}
			
			if (is.null(distribution_results())) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Testing failed"),
						p("Distribution test encountered an error.")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Testing complete: Sample group '%s' tested against %d batch columns", 
											 sample_group_column(), length(batch_cols)))
			)
		})
		
		# Results table
		output$distribution_table <- renderDT({
			req(distribution_results())
			
			df <- distribution_results()$df
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "Distribution test results (lower p-values indicate uneven distribution)"
			) %>%
				formatStyle(
					'Fisher_p_value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				)
		})
		
		# Visualization
		output$distribution_plot <- renderPlot({
			req(distribution_results())
			
			df <- distribution_results()$df
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Create -log10(p-value) plot similar to batch testing
			df_plot <- df %>%
				mutate(
					neg_log_p_fisher = -log10(Fisher_p_value),
					neg_log_p_chisq = -log10(ChiSq_p_value),
					significance = case_when(
						Fisher_p_value < 0.001 ~ "p < 0.001",
						Fisher_p_value < 0.01 ~ "p < 0.01",
						Fisher_p_value < 0.05 ~ "p < 0. 05",
						TRUE ~ "n.s."
					)
				) %>%
				arrange(desc(neg_log_p_fisher))
			
			ggplot(df_plot, aes(x = reorder(Batch_Column, neg_log_p_fisher), 
													y = neg_log_p_fisher, 
													fill = significance)) +
				geom_col() +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
				geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
				geom_hline(yintercept = -log10(0.001), linetype = "dashed", color = "darkred", size = 1) +
				scale_fill_manual(
					values = c(
						"p < 0. 001" = "#d32f2f",
						"p < 0.01" = "#f57c00",
						"p < 0.05" = "#fbc02d",
						"n.s." = "#757575"
					)
				) +
				coord_flip() +
				labs(
					title = sprintf("Distribution of '%s' Across Batch Columns", sample_group_column()),
					subtitle = "Higher values = more uneven distribution (potential confounding)",
					x = "Batch Column",
					y = "-log10(Fisher's p-value)",
					fill = "Significance",
					caption = "Red dashed lines: p = 0.05, 0.01, 0.001"
				) +
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					panel.grid.major.y = element_blank()
				)
		})
		
		# Return results
		return(list(
			results = distribution_results
		))
	})
}


#. ####
# BATCH TESTING ####
#' Batch Testing Module - UI
#'
#' Run ANOVA tests on selected batch columns
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_testing_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Effect Testing (ANOVA)",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Tests for batch effects using ANOVA on selected phenoData columns."),
				p("Results show which columns have significant effects on protein expression."),
				
				
				uiOutput(ns("debug_ui")),
				
				uiOutput(ns("testing_status")),
				
				hr(),
				
				h4("ANOVA Results"),
				DTOutput(ns("anova_table")),
				# 	)
				# ),
				
				fluidRow(
					box(
						title = "Visualization",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						
						plotOutput(ns("anova_plot"), height = "600px")
					)
				)
			)
		)
	)
}

#' Batch Testing Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param selected_columns Reactive vector of column names to test
#' @param debug Enable debug mode
#' @export
mod_batch_testing_server <- function(id, eset, selected_columns, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Render debug button
		output$debug_ui <- renderUI({
			if (debug) {
				tagList(
					actionButton(
						session$ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					),
					hr()
				)
			}
		})
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Batch Testing Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • selected_columns() - Columns to test")
				message("  • batch_testing_results() - ANOVA results")
				message("\nUseful commands:")
				message("  str(batch_testing_results())")
				message("  selected_columns()")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Run ANOVA testing
		# Run ANOVA testing
		batch_testing_results <- reactive({
			req(eset())
			req(selected_columns())
			
			annotations <- selected_columns()
			
			# Need at least one column
			if (length(annotations) == 0) {
				return(NULL)
			}
			
			ExpSet <- eset()
			m <- Biobase::exprs(ExpSet)
			meta <- Biobase::pData(ExpSet)
			
			# Validate annotations exist in metadata
			if (!all(annotations %in% colnames(meta))) {
				missing <- setdiff(annotations, colnames(meta))
				message("Warning: Annotations not found in metadata: ", paste(missing, collapse = ", "))
				annotations <- intersect(annotations, colnames(meta))
				
				if (length(annotations) == 0) {
					return(data.frame())
				}
			}
			
			# Diagnostic: Check each annotation
			message("\n═══ Batch Testing Diagnostics ═══")
			for (ann in annotations) {
				groups <- table(meta[[ann]], useNA = "ifany")
				message(sprintf("Annotation: %s", ann))
				message(sprintf("  Groups: %d", length(groups)))
				message(sprintf("  Samples: %s", paste(names(groups), "=", groups, collapse = ", ")))
			}
			message("═════════════════════════════════\n")
			
			# Run ANOVA
			anova_df <- anova_betadine_tidy_function(m, meta, annotations)
			
			return(anova_df)
		})
		
		# Status message
		output$testing_status <- renderUI({
			req(selected_columns())
			
			annotations <- selected_columns()
			
			if (length(annotations) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No columns selected"),
						p("Please select columns in the Column Selection tab.")
					)
				)
			}
			
			if (is.null(batch_testing_results()) || nrow(batch_testing_results()) == 0) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Testing failed"),
						p("ANOVA computation encountered an error.   Check that selected columns have valid groupings.")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Testing complete: %d columns tested", length(annotations)))
			)
		})
		
		# ANOVA results table
		output$anova_table <- renderDT({
			req(batch_testing_results())
			
			df <- batch_testing_results()
			
			if (nrow(df) == 0) {
				return(datatable(
					data.frame(Message = "No results available"),
					options = list(dom = 't')
				))
			}
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "ANOVA results for batch effect testing"
			) %>%
				formatStyle(
					'p.value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				) %>%
				formatRound(columns = c('statistic', 'p.value'), digits = 3)
		})
		
		# Visualization
		output$anova_plot <- renderPlot({
			req(batch_testing_results())
			
			df <- batch_testing_results()
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Create -log10(p-value) plot
			df_plot <- df %>%
				mutate(
					neg_log_p = -log10(p.value),
					significance = case_when(
						p.value < 0.001 ~ "p < 0.001",
						p.value < 0.01 ~ "p < 0.01",
						p.value < 0.05 ~ "p < 0.05",
						TRUE ~ "n.s."
					)
				) %>%
				arrange(desc(neg_log_p))
			
			ggplot(df_plot, aes(x = reorder(Annotation, neg_log_p), y = neg_log_p, fill = significance)) +
				geom_col() +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
				geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
				scale_fill_manual(
					values = c(
						"p < 0.001" = "#d32f2f",
						"p < 0.01" = "#f57c00",
						"p < 0.05" = "#fbc02d",
						"n.s." = "#757575"
					)
				) +
				coord_flip() +
				labs(
					title = "Batch Effect Significance",
					subtitle = "Higher values indicate stronger batch effects",
					x = "Annotation Column",
					y = "-log10(p-value)",
					fill = "Significance"
				) +
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					panel.grid.major.y = element_blank()
				)
		})
		
		# Return results for other modules
		return(list(
			results = batch_testing_results
		))
	})
}

#. ####
# BATCH VISULAISATION #####
#' Batch Effect Visualization Module - UI
#'
#' Compare batch effects before and after correction using multiple visualization methods
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_visualization_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		
		fluidRow(
			box(
				title = "Batch Effect Visualization Settings",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				uiOutput(ns("debug_ui")),
				textOutput(ns('eset_name_text')),
				
				fluidRow(
					box(
						title = "Corrected Data Selection",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						
						fluidRow(
							column(
								width = 4,
								radioButtons(
									ns("corrected_source"),
									"Corrected Data Source:",
									choices = c(
										"Use ComBat from this session" = "session",
										"Load from ExpressionSet assay" = "assay"
									),
									selected = "session"
								)
							),
							column(
								width = 8,
								conditionalPanel(
									condition = "input.corrected_source == 'assay'",
									ns = ns,
									textInput(
										ns("corrected_suffix"),
										"Corrected Assay Suffix:",
										value = "_ComBat",
										placeholder = "_ComBat"
									),
									helpText("Will append this to the current assay name"),
									uiOutput(ns("corrected_assay_selector"))  # Shows status, not dropdown
								)
							)
							
						),
						
						hr(),
						
						uiOutput(ns("corrected_data_status"))
					)
				),
				
				fluidRow(
					column(
						width = 3,
						selectInput(
							ns("color_by"),
							"Color by:",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("shape_by"),
							"Shape by (t-SNE only):",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("qc_column"),
							"QC Column (for TR identification):",
							choices = NULL,
							selected = "QC"
						),
						helpText("Used to identify technical replicates on dendrogram")
					),
					column(
						width = 3,
						selectInput(
							ns("label_column"),
							"Label Column (for TR grouping):",
							choices = NULL,
							selected = "Labels"
						),
						helpText("TR samples with same Label will be colored together")
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 3,
						numericInput(
							ns("tsne_perplexity"),
							"t-SNE Perplexity:",
							value = 30,
							min = 5,
							max = 50
						)
					),
					column(
						width = 3,
						numericInput(
							ns("tsne_iterations"),
							"t-SNE Iterations:",
							value = 1000,
							min = 250,
							max = 5000,
							step = 250
						)
					),
					column(
						width = 3,
						checkboxInput(
							ns("show_corrected"),
							"Show Corrected Data",
							value = TRUE
						),
						helpText("Enable to compare before/after ComBat correction")
					),
					column(
						width = 3,
						actionButton(
							ns("run_analysis"),
							"Generate Visualizations",
							icon = icon("chart-line"),
							class = "btn-primary btn-lg",
							style = "margin-top: 25px;"
						)
					)
				)
			)
		),
		
		# PN Correlations ####
		
		fluidRow(
			box(
				title = "Technical Replicate Correlation Analysis",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Correlation between technical replicates should be high (>0.9).  ComBat correction should improve correlation."),
				
				tabsetPanel(
					id = ns("correlation_tabs"),
					
					tabPanel(
						"Correlation Matrix",
						br(),
						fluidRow(
							column(
								width = 6,
								h4("Original Data"),
								htmlOutput(ns("correlation_stats_original")),
								plotOutput(ns("correlation_matrix_original"), height = "600px")
							),
							column(
								width = 6,
								h4("Corrected Data"),
								htmlOutput(ns("correlation_stats_corrected")),
								plotOutput(ns("correlation_matrix_corrected"), height = "600px")
							)
						)
					),
					
					tabPanel(
						"Correlation Distribution",
						br(),
						plotOutput(ns("correlation_histogram"), height = "500px")
					),
					
					tabPanel(
						"By Batch",
						br(),
						plotOutput(ns("correlation_by_batch"), height = "600px")
					),
					
					tabPanel(
						"Intra vs Inter Batch",
						br(),
						DT::dataTableOutput(ns("correlation_summary_table")),
						br(),
						plotOutput(ns("correlation_intra_inter"), height = "600px")
					)
				)
			)
		),
		
		# Sample Correlation Analysis (Non-TR) #####
		fluidRow(
			box(
				title = "Sample Correlation Analysis (Non-Technical Replicates)",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Summary statistics for all pairwise correlations between non-TR samples. "),
				tabsetPanel(
					id = ns("sample_correlation_tabs"),
					
					tabPanel(
						"Summary Statistics",
						fluidRow(
							column(
								width = 6,
								h4("Original Data"),
								htmlOutput(ns("sample_correlation_stats_original"))
							),
							column(
								width = 6,
								h4("Corrected Data"),
								htmlOutput(ns("sample_correlation_stats_corrected"))
							)
						),
						
						hr(),
						
						fluidRow(
							column(
								width = 12,
								h4("Comparison Table"),
								DT::dataTableOutput(ns("sample_correlation_comparison"))
							)
						)
					),
					tabPanel(
						"Distribution",
						br(),
						fluidRow(
							column(
								width = 6,
								h4("Distribution - Original"),
								plotOutput(ns("sample_correlation_hist_original"), height = "400px")
							),
							column(
								width = 6,
								h4("Distribution - Corrected"),
								plotOutput(ns("sample_correlation_hist_corrected"), height = "400px")
							)
						)
					),
					
					tabPanel(
						"By Batch",
						br(),
						p("Mean correlation for each sample, grouped by batch.  Samples should have consistent correlations across batches."),
						shinycssloaders::withSpinner(
							plotOutput(ns("sample_correlation_by_batch"), height = "600px"),
							type = 4,
							color = "#3c8dbc"
						)
					),
					
					tabPanel(
						"Intra vs Inter Batch",
						br(),
						p("Compare correlations within batches vs between batches.  ComBat should reduce differences. "),
						DT::dataTableOutput(ns("sample_correlation_intra_inter_table")),
						br(),
						shinycssloaders::withSpinner(
							plotOutput(ns("sample_correlation_intra_inter"), height = "600px"),
							type = 4,
							color = "#3c8dbc"
						)
					)
				)
			)
		),
		
		# Dendrogram #####
		fluidRow(
			box(
				title = "Hierarchical Clustering Dendrogram",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Technical replicates should cluster together if batch effects are minimal."),
				
				tabsetPanel(selected = 'Side-by-Side',
										id = ns("dendro_tabs"),
										tabPanel(
											"Original Data",
											plotOutput(ns("dendrogram_original"), height = "600px")
										),
										tabPanel(
											"Corrected Data",
											plotOutput(ns("dendrogram_corrected"), height = "600px")
										),
										tabPanel('Side-by-Side',
														 plotOutput(ns("dendrogram_original_2"), height = "300px"),
														 plotOutput(ns("dendrogram_corrected_2"), height = "300px")
										)
				)
			)
		),
		
		# t-SNE #####
		fluidRow(
			box(
				title = "t-SNE Dimensionality Reduction",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Samples should separate by biological groups, not batch factors."),
				
				tabsetPanel(selected = "Side-by-Side",
										id = ns("tsne_tabs"),
										tabPanel(
											"Original Data",
											plotOutput(ns("tsne_original"), height = "600px")
										),
										tabPanel(
											"Corrected Data",
											plotOutput(ns("tsne_corrected"), height = "600px")
										),
										tabPanel(
											"Side-by-Side",
											plotOutput(ns("tsne_comparison"), height = "600px")
										)
				)
			)
		),
		
		# PCA
		fluidRow(
			box(
				title = "Principal Component Analysis (PCA)",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("pca_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("pca_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("pca_corrected"), height = "600px")
					),
					tabPanel(
						"Variance Explained",
						plotOutput(ns("pca_variance"), height = "400px")
					)
				)
			)
		),
		
		# Heatmap
		fluidRow(
			box(
				title = "Sample Distance Heatmap",
				width = 12,
				status = "danger",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("heatmap_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("heatmap_original"), height = "700px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("heatmap_corrected"), height = "700px")
					)
				)
			)
		)
	)
}

#' Batch Effect Visualization Module - Server
#'
#' @param id Module namespace ID
#' @param eset_original Reactive ExpressionSet (original data)
#' @param eset_corrected Reactive ExpressionSet (ComBat corrected data)
#' @param debug Enable debug mode
#' @export
mod_batch_visualization_server <- function(id,
																					 eset_original_name = reactive(NULL),
																					 eset_original,
																					 eset_corrected = reactive(NULL),
																					 sample_group_column = reactive(NULL),
																					 batch_factors = reactive(NULL),
																					 ExpSet_list = NULL,
																					 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns 
		# Render debug button
		output$debug_ui <- renderUI({
			if (debug) {
				tagList(
					actionButton(
						session$ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					),
					hr()
				)
			}
		})
		
		output$eset_name_text = renderText({
			print(eset_original_name())
		})
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Batch Visualization Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset_original() - Original ExpressionSet")
				message("  • eset_corrected() - Corrected ExpressionSet")
				message("  • input$color_by - Coloring factor")
				message("  • input$shape_by - Shape factor")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		observe({
			req(eset_original())
			message("\n🔄 BATCH VIZ: eset_original CHANGED")
			message("  Samples: ", ncol(eset_original()))
			message("  Features: ", nrow(eset_original()))
			message("  First sample: ", colnames(eset_original())[1])
		})
		
		#Check assay compatibility #####
		
		app_corrected_name <- reactive({
			if (is.null(eset_corrected) || is.null(eset_corrected())) {
				return(NULL)
			}
			
			ExpSet <- eset_corrected()
			if (is.null(ExpSet)) {
				return(NULL)
			}
			
			notes <- Biobase::notes(ExpSet)
			notes$name
		})
		
		current_assay_name <- reactive({
			eset_original_name()
		})
		
		# ✅ Build the expected corrected assay name
		expected_corrected_assay_name <- reactive({
			req(current_assay_name())
			
			suffix <- input$corrected_suffix
			if (is.null(suffix) || suffix == "") {
				suffix <- "_ComBat"
			}
			
			paste0(current_assay_name(), suffix)
		})
		
		# ✅ Check if the expected corrected assay exists in the ExpressionSet
		corrected_assay_exists <- reactive({
			req(eset_original())
			req(expected_corrected_assay_name())
			
			ExpSet <- eset_original()
			all_assays <- Biobase::assayDataElementNames(ExpSet)
			
			expected <- expected_corrected_assay_name()
			exists <- expected %in% all_assays
			
			message("🔍 BATCH VIZ: Checking for corrected assay")
			message("  Current assay: ", current_assay_name())
			message("  Expected corrected assay: ", expected)
			message("  All available assays: ", paste(all_assays, collapse = ", "))
			message("  Exists: ", exists)
			
			exists
		})
		
		# ✅ Check if session ComBat can be used (ONLY if assay names match)
		combat_is_compatible <- reactive({
			req(current_assay_name())
			
			app_corrected <- app_corrected_name()
			current <- current_assay_name()
			
			message("🔍 BATCH VIZ: Checking ComBat compatibility")
			message("  Current assay: ", current)
			message("  App corrected assay: ", app_corrected %||% "NULL")
			
			if (is.null(app_corrected)) {
				message("  ❌ No app corrected data")
				return(FALSE)
			}
			
			# ONLY compatible if names match exactly
			compatible <- (current == app_corrected)
			message("  Compatible: ", compatible)
			
			return(compatible)
		})
		
		
		
		# ✅ REPLACE the dropdown with a status display
		output$corrected_assay_selector <- renderUI({
			req(input$corrected_source == "assay")
			req(expected_corrected_assay_name())
			
			expected <- expected_corrected_assay_name()
			available <- available_corrected_assays()
			exists <- expected %in% available
			
			message("🔍 BATCH VIZ: Rendering corrected assay status")
			message("  Expected: ", expected)
			message("  Exists: ", exists)
			
			if (exists) {
				div(
					class = "alert alert-info",
					style = "margin-top: 10px;",
					icon("info-circle"),
					strong(" Corrected Assay Name: "),
					tags$code(expected),
					p(style = "margin-top: 5px; margin-bottom: 0;", 
						"This assay will be loaded from the ExpressionSet.")
				)
			} else {
				div(
					class = "alert alert-warning",
					style = "margin-top: 10px;",
					icon("exclamation-triangle"),
					strong(" Corrected Assay Name: "),
					tags$code(expected),
					p(style = "margin-top: 5px; margin-bottom: 0;", 
						strong("⚠️ This assay does not exist in the ExpressionSet"))
				)
			}
		})
		
		# ✅ Dynamic corrected eset - simplified since no user selection
		eset_corrected_dynamic <- reactive({
			
			if (input$corrected_source == "session") {
				# Use ComBat from this session - ONLY IF COMPATIBLE
				message("🔍 BATCH VIZ: Attempting to use session ComBat")
				
				if (!  combat_is_compatible()) {
					message("  ❌ Not compatible - assay names don't match")
					return(NULL)
				}
				
				req(eset_corrected())
				corrected <- eset_corrected()
				
				if (is.null(corrected)) {
					message("  ❌ Corrected is NULL")
					return(NULL)
				}
				
				message("  ✅ Using session ComBat")
				return(corrected)
				
			} else if (input$corrected_source == "assay") {
				# Load from ExpressionSet assay using expected name
				message("🔍 BATCH VIZ: Loading from ExpressionSet assay")
				
				req(eset_original())
				req(expected_corrected_assay_name())
				
				ExpSet <- eset_original()
				corrected_assay_name <- expected_corrected_assay_name()
				
				message("  Looking for assay: ", corrected_assay_name)
				
				# Check if it exists
				all_assays <- Biobase::assayDataElementNames(ExpSet)
				
				if (! corrected_assay_name %in% all_assays) {
					message("  ❌ Assay '", corrected_assay_name, "' not found")
					return(NULL)
				}
				
				message("  ✅ Found assay: ", corrected_assay_name)
				
				# Create a copy with the corrected data as exprs
				corrected_ExpSet <- ExpSet
				Biobase::exprs(corrected_ExpSet) <- Biobase::assayDataElement(ExpSet, corrected_assay_name)
				
				# Add note about source
				notes <- Biobase::notes(corrected_ExpSet)
				notes$visualization_source <- list(
					assay_name = corrected_assay_name,
					base_assay_name = current_assay_name(),
					loaded_from = "ExpressionSet assayData"
				)
				Biobase::notes(corrected_ExpSet) <- notes
				
				return(corrected_ExpSet)
			}
			
			return(NULL)
		})
		
		
		
		
		# ✅ Status display with all warnings
		output$corrected_data_status <- renderUI({
			
			if (input$corrected_source == "session") {
				app_corrected <- app_corrected_name()
				current <- current_assay_name()
				
				if (is.null(app_corrected)) {
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No ComBat-corrected data available from this session"),
						p("Run ComBat correction in the 'Batch Correction' tab or switch to 'Load from ExpressionSet assay' mode.")
					)
				} else if (! combat_is_compatible()) {
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" ComBat data is for a different assay"),
						tags$ul(
							tags$li("ComBat was run on: ", strong(app_corrected)),
							tags$li("Currently viewing: ", strong(current))
						),
						p(strong("Solutions:")),
						tags$ul(
							tags$li("Select '", app_corrected, "' in the Input Data selector above, OR"),
							tags$li("Re-run ComBat correction on '", current, "', OR"),
							tags$li("Switch to 'Load from ExpressionSet assay' mode below")
						)
					)
				} else {
					notes <- Biobase::notes(eset_corrected())
					combat_info <- notes$combat_correction
					
					div(
						class = "alert alert-success",
						icon("check-circle"),
						strong(" Using ComBat data from this session ✓"),
						tags$ul(
							tags$li("Assay: ", strong(app_corrected)),
							tags$li("Batch factors: ", paste(combat_info$batch_factors, collapse = ", ")),
							tags$li("Correction date: ", as.character(combat_info$correction_date))
						)
					)
				}
				
			} else if (input$corrected_source == "assay") {
				# ✅ Check if the expected corrected assay exists
				expected <- expected_corrected_assay_name()
				current <- current_assay_name()
				available_assays <- Biobase::assayDataElementNames(eset_original())
				
				if (! expected %in% available_assays) {
					# ✅ WARNING: Expected corrected assay doesn't exist
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Corrected assay not found in ExpressionSet"),
						tags$ul(
							tags$li("Current assay: ", strong(current)),
							tags$li("Expected corrected assay: ", strong(expected)),
							tags$li("Available assays: ", paste(available_assays, collapse = ", "))
						),
						p(strong("Solutions:")),
						tags$ul(
							tags$li("Run ComBat correction on '", current, "' to create '", expected, "'"),
							tags$li("Adjust the 'Corrected Assay Suffix' above if it uses a different naming convention"),
							tags$li("Select a different input assay that has a corrected version available"),
							tags$li("Switch to 'Use ComBat from this session' if you've just run correction")
						)
					)
				} else {
					# ✅ Corrected assay exists - try to load it
					corrected <- eset_corrected_dynamic()
					
					if (is.null(corrected)) {
						div(
							class = "alert alert-danger",
							icon("times-circle"),
							strong(" Error loading corrected assay"),
							p("The assay '", expected, "' exists but could not be loaded.")
						)
					} else {
						notes <- Biobase::notes(corrected)
						source_info <- notes$visualization_source
						
						div(
							class = "alert alert-success",
							icon("check-circle"),
							strong(" Using corrected data from ExpressionSet ✓"),
							tags$ul(
								tags$li("Base assay: ", strong(source_info$base_assay_name)),
								tags$li("Corrected assay: ", strong(source_info$assay_name)),
								tags$li("Dimensions: ", nrow(corrected), " features × ", ncol(corrected), " samples")
							)
						)
					}
				}
			}
		})
		
		#####
		
		
		
		
		available_corrected_assays <- reactive({
			req(eset_original())
			
			message("🔍 BATCH VIZ: Updating available_corrected_assays")
			
			ExpSet <- eset_original()
			all_assays <- Biobase::assayDataElementNames(ExpSet)
			
			message("  All assays: ", paste(all_assays, collapse = ", "))
			
			# Filter for assays that look like ComBat-corrected
			corrected <- grep("combat|corrected|batch", all_assays, ignore.case = TRUE, value = TRUE)
			
			message("  Corrected assays found: ", paste(corrected, collapse = ", "))
			
			if (length(corrected) == 0) {
				return(c("No corrected assays found" = ""))
			}
			
			setNames(corrected, corrected)
		})
		
		
		
		# Update column choices
		
		# Update column choices (keep your existing code)
		observe({
			req(eset_original())
			
			message("🔍 BATCH VIZ: Updating column choices")
			message("  Current assay: ", current_assay_name())
			
			cols <- colnames(Biobase::pData(eset_original()))
			
			
			#message("  Available columns: ", paste(cols, collapse = ", "))
			
			sample_group <- sample_group_column()
			batch_cols <- batch_factors()
			
			color_default <- if (! is.null(batch_cols) && length(batch_cols) > 0) {
				# Use first batch factor that was corrected
				batch_cols[1]
			} else if ("Batch_ID" %in% cols) {
				"Batch_ID"
			} else if ("Batch" %in% cols) {
				"Batch"
			} else {
				cols[1]
			}
			
			# Determine default for shape_by
			shape_default <- if (!is.null(sample_group) && length(sample_group) > 1) {
				# If multiple batch factors, use second for shape
				sample_group[1]
			} else if ("Sample_Group" %in% cols) {
				"Sample_Group"
			} else if ("Labels" %in% cols) {
				"Labels"
			} else {
				""
			}
			
			updateSelectInput(session, "color_by", choices = cols, selected = color_default)
			updateSelectInput(session, "shape_by", choices = c("None" = "", cols), selected = shape_default)
			
			# Auto-select QC and Labels
			qc_default <- if ("QC" %in% cols) "QC" else cols[1]
			label_default <- if ("Labels" %in% cols) "Labels" else cols[1]
			
			updateSelectInput(session, "qc_column", 
												choices = cols, 
												selected = qc_default)
			
			updateSelectInput(session, "label_column", 
												choices = cols, 
												selected = shape_default)
			# updateSelectInput(session, "replicate_column", choices = c("None" = "", cols), selected = "QC")
		})
		
		# Display what's being visualized
		output$correction_info <- renderUI({
			sample_group <- sample_group_column()
			batch_cols <- batch_factors()
			
			if (is.null(batch_cols) || length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" Batch Analysis Configuration:"),
						p(
							"Sample Group: ", 
							if (! is.null(sample_group)) tags$code(sample_group) else tags$em("Not selected")
						),
						p("No batch correction applied yet.")
					)
				)
			}
			
			tagList(
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Batch Correction Applied"),
					p(
						"Sample Group Preserved: ", 
						if (!is.null(sample_group)) tags$code(sample_group) else tags$em("None")
					),
					p(
						strong(sprintf("Corrected for %d batch factor(s):", length(batch_cols)))
					),
					tags$ul(
						style = "margin-bottom: 0;",
						lapply(batch_cols, function(x) tags$li(tags$code(x)))
					)
				),
				div(
					class = "alert alert-info",
					icon("palette"),
					strong(" Current Visualization Settings:"),
					p(
						"Color by: ", tags$code(input$color_by %||% "Not set"), " | ",
						"Shape by: ", tags$code(if (is.null(input$shape_by) || input$shape_by == "") "None" else input$shape_by)
					)
				)
			)
		})
		
		# Storage for analysis results
		analysis_results <- reactiveVal(NULL)
		
		# Run analysis
		observeEvent(input$run_analysis, {
			req(eset_original()) 
			req(input$color_by)
			
			showNotification("Running visualization analysis...", id = "viz_progress", duration = NULL, type = "message")
			
			tryCatch({
				results <- list()
				
				# Original data analysis
				results$original <- perform_analysis(
					eset_original(),
					perplexity = input$tsne_perplexity,
					iterations = input$tsne_iterations
				)
				
				# Corrected data analysis (if available)
				if (input$show_corrected && !is.null(eset_corrected_dynamic())) {
					results$corrected <- perform_analysis(
						eset_corrected_dynamic(),
						perplexity = input$tsne_perplexity,
						iterations = input$tsne_iterations
					)
				}
				
				analysis_results(results)
				
				removeNotification("viz_progress")
				showNotification("✅ Visualization complete!", type = "message", duration = 3)
				
			}, error = function(e) {
				removeNotification("viz_progress")
				showNotification(paste("❌ Analysis failed:", e$message), type = "error", duration = 10)
			})
		})
		
		# Helper function to perform all analyses
		perform_analysis <- function(eset, perplexity, iterations) {
			expr_data <- Biobase::exprs(eset)
			meta <- Biobase::pData(eset)
			
			# Row scale for distance calculations
			expr_scaled <- row_scale_function(expr_data)
			
			# Hierarchical clustering
			dist_mat <- dist(t(expr_scaled))
			hclust_res <- hclust(dist_mat, method = "ward.D2")
			
			# t-SNE
			set.seed(42)
			tsne_res <- Rtsne::Rtsne(
				t(expr_scaled),
				dims = 2,
				perplexity = perplexity,
				max_iter = iterations,
				check_duplicates = FALSE
			)
			
			# PCA
			pca_res <- prcomp(t(expr_scaled), scale.  = FALSE)
			
			list(
				hclust = hclust_res,
				dist = dist_mat,
				tsne = tsne_res,
				pca = pca_res,
				meta = meta,
				expr = expr_data,
				expr_scaled = expr_scaled
			)
		}
		
		
		# PN Correlations ####
		# Add these outputs to the server function:
		
		# Correlation analysis for technical replicates
		tr_data <- reactive({
			req(analysis_results())
			req(input$qc_column)
			
			results <- analysis_results()$original
			meta <- results$meta
			
			# Identify TR samples
			is_tr <- grepl("TR", meta[[input$qc_column]], ignore.case = TRUE)
			print(rownames(meta[is_tr,]))
			
			if (sum(is_tr) == 0) {
				return(NULL)
			}
			
			list(
				is_tr = is_tr,
				tr_samples = rownames(meta)[is_tr],
				meta = meta
			)
		})
		
		# Calculate correlations
		correlation_results <- reactive({ 
			req(tr_data())
			req(analysis_results())
			
			tr_info <- tr_data()
			tr_samples <- tr_info$tr_samples
			
			if (length(tr_samples) < 2) {
				return(NULL)
			}
			
			# Original data correlations
			expr_orig <- analysis_results()$original$expr[, tr_samples]
			dim(expr_orig)
			cor_orig <- cor(expr_orig, method = "pearson")
			
			results <- list(
				original = list(
					expr_orig = expr_orig,
					cor_matrix = cor_orig,
					mean = mean(cor_orig[upper.tri(cor_orig)], na.rm = TRUE),
					median = median(cor_orig[upper.tri(cor_orig)], na.rm = TRUE),
					min = min(cor_orig[upper.tri(cor_orig)], na.rm = TRUE)
				)
			)
			
			# Corrected data correlations (if available)
			if (!is.null(analysis_results()$corrected)) {
				expr_corr <- analysis_results()$corrected$expr[, tr_samples]
				cor_corr <- cor(expr_corr, method = "pearson")
				
				results$corrected <- list(
					expr_corr = expr_corr,
					cor_matrix = cor_corr,
					mean = mean(cor_corr[upper.tri(cor_corr)], na.rm = TRUE),
					median = median(cor_corr[upper.tri(cor_corr)], na.rm = TRUE),
					min = min(cor_corr[upper.tri(cor_corr)], na.rm = TRUE)
				)
			}
			
			results
		})
		
		# Correlation statistics display
		output$correlation_stats_original <- renderUI({
			req(correlation_results())
			
			stats <- correlation_results()$original
			threshold <- 0.9
			
			mean_color <- if (stats$mean < threshold) "red" else "black"
			median_color <- if (stats$median < threshold) "red" else "black"
			min_color <- if (stats$min < threshold) "red" else "black"
			
			HTML(paste0(
				"<strong>Correlation Statistics:</strong><br>",
				"Mean = <span style='color: ", mean_color, ";'>", round(stats$mean, 3), "</span><br>",
				"Median = <span style='color: ", median_color, ";'>", round(stats$median, 3), "</span><br>",
				"Minimum = <span style='color: ", min_color, ";'>", round(stats$min, 3), "</span>"
			))
		})
		
		output$correlation_stats_corrected <- renderUI({
			req(correlation_results())
			req(correlation_results()$corrected)
			
			stats <- correlation_results()$corrected
			threshold <- 0.9
			
			mean_color <- if (stats$mean < threshold) "red" else "green"
			median_color <- if (stats$median < threshold) "red" else "green"
			min_color <- if (stats$min < threshold) "red" else "green"
			
			HTML(paste0(
				"<strong>Correlation Statistics:</strong><br>",
				"Mean = <span style='color: ", mean_color, ";'>", round(stats$mean, 3), "</span><br>",
				"Median = <span style='color: ", median_color, ";'>", round(stats$median, 3), "</span><br>",
				"Minimum = <span style='color: ", min_color, ";'>", round(stats$min, 3), "</span>"
			))
		})
		
		# Correlation matrix plots
		output$correlation_matrix_original <- renderPlot({
			req(correlation_results())
			
			expr_orig <- correlation_results()$original$expr_orig
			plot_correlation_matrix(expr_orig, "Original Data")
		})
		
		output$correlation_matrix_corrected <- renderPlot({
			req(correlation_results())
			req(correlation_results()$corrected)
			
			expr_corr <- correlation_results()$corrected$expr_corr
			plot_correlation_matrix(expr_corr, "ComBat Corrected Data")
		})
		
		# Correlation histogram
		output$correlation_histogram <- renderPlot({
			req(correlation_results())
			
			plot_correlation_histogram(correlation_results())
		})
		
		# Correlation by batch
		output$correlation_by_batch <- renderPlot({
			req(correlation_results())  
			req(tr_data())
			req(input$color_by)
			
			#correlation_results, meta, batch_column, tr_samples
			
			plot_correlation_by_batch(
				correlation_results = correlation_results(),
				meta = tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		# Intra vs Inter batch correlation
		output$correlation_intra_inter <- renderPlot({
			req(correlation_results())
			req(tr_data())
			req(input$color_by)
			
			plot_correlation_intra_inter(
				correlation_results(),
				tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		output$correlation_summary_table <- DT::renderDataTable({
			req(correlation_results())
			req(tr_data())
			req(input$color_by)
			
			create_correlation_summary_table(
				correlation_results(),
				tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		
		# Sample correlation analysis (non-TR samples) ####
		sample_correlation_results <- reactive({
			req(analysis_results())
			req(tr_data())
			
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			
			# Get non-TR samples
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			if (length(non_tr_samples) < 2) {
				return(NULL)
			}
			
			# Original data correlations
			expr_orig <- analysis_results()$original$expr[, non_tr_samples]
			dim(expr_orig)
			cor_orig <- cor(expr_orig, method = "pearson")
			mean(cor_orig)
			
			# Extract upper triangle (to avoid duplicates and diagonal)
			upper_tri_orig <- cor_orig[upper.tri(cor_orig)]
			
			results <- list(
				n_samples = length(non_tr_samples),
				original = list(
					cor_matrix = cor_orig,
					mean = mean(upper_tri_orig, na.rm = TRUE),
					median = median(upper_tri_orig, na.rm = TRUE),
					min = min(upper_tri_orig, na.rm = TRUE),
					max = max(upper_tri_orig, na.rm = TRUE),
					sd = sd(upper_tri_orig, na.rm = TRUE),
					values = upper_tri_orig
				)
			)
			
			# Corrected data correlations (if available)
			if (!is.null(analysis_results()$corrected)) {
				expr_corr <- analysis_results()$corrected$expr[, non_tr_samples]
				cor_corr <- cor(expr_corr, method = "pearson")
				upper_tri_corr <- cor_corr[upper.tri(cor_corr)]
				
				results$corrected <- list(
					cor_matrix = cor_corr,
					mean = mean(upper_tri_corr, na.rm = TRUE),
					median = median(upper_tri_corr, na.rm = TRUE),
					min = min(upper_tri_corr, na.rm = TRUE),
					max = max(upper_tri_corr, na.rm = TRUE),
					sd = sd(upper_tri_corr, na.rm = TRUE),
					values = upper_tri_corr
				)
			}
			
			results
		})
		
		# Sample correlation statistics - Original
		output$sample_correlation_stats_original <- renderUI({
			req(sample_correlation_results())
			
			results <- sample_correlation_results()
			stats <- results$original
			n_samples <- results$n_samples
			n_comparisons <- length(stats$values)
			
			HTML(paste0(
				"<div style='padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>",
				"<strong>Number of Samples:</strong> ", n_samples, "<br>",
				"<strong>Number of Comparisons:</strong> ", format(n_comparisons, big.mark = ","), "<br><br>",
				"<strong>Correlation Statistics:</strong><br>",
				"<table style='margin-left: 20px;'>",
				"<tr><td>Mean:</td><td style='padding-left: 10px;'><strong>", round(stats$mean, 4), "</strong></td></tr>",
				"<tr><td>Median:</td><td style='padding-left: 10px;'><strong>", round(stats$median, 4), "</strong></td></tr>",
				"<tr><td>Minimum:</td><td style='padding-left: 10px;'><strong>", round(stats$min, 4), "</strong></td></tr>",
				"<tr><td>Maximum:</td><td style='padding-left: 10px;'><strong>", round(stats$max, 4), "</strong></td></tr>",
				"<tr><td>Std Dev:</td><td style='padding-left: 10px;'><strong>", round(stats$sd, 4), "</strong></td></tr>",
				"</table>",
				"</div>"
			))
		})
		
		# Sample correlation statistics - Corrected
		output$sample_correlation_stats_corrected <- renderUI({
			req(sample_correlation_results())
			req(sample_correlation_results()$corrected)
			
			results <- sample_correlation_results()
			stats <- results$corrected
			n_samples <- results$n_samples
			n_comparisons <- length(stats$values)
			
			# Calculate improvement
			orig_stats <- results$original
			mean_change <- stats$mean - orig_stats$mean
			median_change <- stats$median - orig_stats$median
			min_change <- stats$min - orig_stats$min
			
			mean_arrow <- if (mean_change > 0.01) "↑" else if (mean_change < -0.01) "↓" else "→"
			median_arrow <- if (median_change > 0.01) "↑" else if (median_change < -0.01) "↓" else "→"
			min_arrow <- if (min_change > 0.01) "↑" else if (min_change < -0.01) "↓" else "→"
			
			HTML(paste0(
				"<div style='padding: 15px; background-color: #d4edda; border-radius: 5px;'>",
				"<strong>Number of Samples:</strong> ", n_samples, "<br>",
				"<strong>Number of Comparisons:</strong> ", format(n_comparisons, big.mark = ","), "<br><br>",
				"<strong>Correlation Statistics:</strong><br>",
				"<table style='margin-left: 20px;'>",
				"<tr><td>Mean:</td><td style='padding-left: 10px;'><strong>", round(stats$mean, 4), 
				"</strong> <span style='color: ", if (mean_change > 0) "green" else if (mean_change < 0) "red" else "gray", ";'>", 
				mean_arrow, " ", sprintf("%+.4f", mean_change), "</span></td></tr>",
				"<tr><td>Median:</td><td style='padding-left: 10px;'><strong>", round(stats$median, 4), 
				"</strong> <span style='color: ", if (median_change > 0) "green" else if (median_change < 0) "red" else "gray", ";'>", 
				median_arrow, " ", sprintf("%+.4f", median_change), "</span></td></tr>",
				"<tr><td>Minimum:</td><td style='padding-left: 10px;'><strong>", round(stats$min, 4), 
				"</strong> <span style='color: ", if (min_change > 0) "green" else if (min_change < 0) "red" else "gray", ";'>", 
				min_arrow, " ", sprintf("%+.4f", min_change), "</span></td></tr>",
				"<tr><td>Maximum:</td><td style='padding-left: 10px;'><strong>", round(stats$max, 4), "</strong></td></tr>",
				"<tr><td>Std Dev:</td><td style='padding-left: 10px;'><strong>", round(stats$sd, 4), "</strong></td></tr>",
				"</table>",
				"</div>"
			))
		})
		
		# Comparison table
		output$sample_correlation_comparison <- DT::renderDataTable({
			req(sample_correlation_results())
			
			results <- sample_correlation_results()
			orig <- results$original
			
			comparison_df <- data.frame(
				Statistic = c("Mean", "Median", "Minimum", "Maximum", "Std Dev"),
				Original = c(orig$mean, orig$median, orig$min, orig$max, orig$sd)
			)
			
			if (! is.null(results$corrected)) {
				corr <- results$corrected
				comparison_df$Corrected <- c(corr$mean, corr$median, corr$min, corr$max, corr$sd)
				comparison_df$Change <- comparison_df$Corrected - comparison_df$Original
				comparison_df$Percent_Change <- (comparison_df$Change / comparison_df$Original) * 100
			}
			
			DT::datatable(
				comparison_df,
				options = list(
					dom = 't',
					pageLength = 10
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('Original', 'Corrected', 'Change'), digits = 4) %>%
				DT::formatRound(columns = 'Percent_Change', digits = 2) %>%
				{
					if ('Change' %in% colnames(comparison_df)) {
						DT::formatStyle(
							.,
							'Change',
							backgroundColor = DT::styleInterval(c(-0.01, 0.01), c('lightcoral', 'lightyellow', 'lightgreen'))
						)
					} else {
						. 
					}
				}
		})
		
		# Histograms
		output$sample_correlation_hist_original <- renderPlot({
			req(sample_correlation_results())
			
			stats <- sample_correlation_results()$original
			
			hist(
				stats$values,
				breaks = 50,
				main = "Original Data",
				xlab = "Pearson Correlation",
				ylab = "Frequency",
				col = "coral",
				border = "white",
				xlim = c(min(stats$values, 0), 1)
			)
			abline(v = stats$mean, col = "red", lwd = 2, lty = 2)
			abline(v = stats$median, col = "blue", lwd = 2, lty = 2)
			legend("topleft", 
						 legend = c("Mean", "Median"),
						 col = c("red", "blue"),
						 lty = 2,
						 lwd = 2,
						 bty = "n")
		})
		
		output$sample_correlation_hist_corrected <- renderPlot({
			req(sample_correlation_results())
			req(sample_correlation_results()$corrected)
			
			stats <- sample_correlation_results()$corrected
			
			hist(
				stats$values,
				breaks = 50,
				main = "Corrected Data",
				xlab = "Pearson Correlation",
				ylab = "Frequency",
				col = "steelblue",
				border = "white",
				xlim = c(min(stats$values, 0), 1)
			)
			abline(v = stats$mean, col = "red", lwd = 2, lty = 2)
			abline(v = stats$median, col = "blue", lwd = 2, lty = 2)
			legend("topleft", 
						 legend = c("Mean", "Median"),
						 col = c("red", "blue"),
						 lty = 2,
						 lwd = 2,
						 bty = "n")
		})
		
		##  Sample correlation by batch ####
		output$sample_correlation_by_batch <- renderPlot({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			plot_sample_correlation_by_batch(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		# Sample correlation intra vs inter batch
		output$sample_correlation_intra_inter <- renderPlot({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			plot_sample_correlation_intra_inter(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		output$sample_correlation_intra_inter_table <- DT::renderDataTable({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			create_sample_correlation_intra_inter_table(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		# Dendrogram - Original ####
		output$dendrogram_original <- renderPlot({
			req(analysis_results()) 
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$original
			
			
			
			plot_dendrogram(
				hclust_obj = results$hclust,
				meta = results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "Original Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Corrected ####
		output$dendrogram_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$corrected
			
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "ComBat Corrected Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Original ####
		output$dendrogram_original_2 <- renderPlot({
			req(analysis_results()) 
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$original
			
			
			
			plot_dendrogram(
				hclust_obj = results$hclust,
				meta = results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "Original Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Corrected ####
		output$dendrogram_corrected_2 <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$corrected
			
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "ComBat Corrected Data - Hierarchical Clustering"
			)
		})
		
		# t-SNE #####
		# t-SNE - Original
		output$tsne_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# t-SNE - Corrected
		output$tsne_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# t-SNE - Comparison
		output$tsne_comparison <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			orig <- analysis_results()$original
			corr <- analysis_results()$corrected
			
			plot_tsne_comparison(
				orig$tsne, corr$tsne,
				orig$meta,
				color_by = input$color_by,
				shape_by = input$shape_by
			)
		})
		
		# PCA - Original
		output$pca_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# PCA - Corrected
		output$pca_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# PCA Variance
		output$pca_variance <- renderPlot({
			req(analysis_results())
			
			orig_var <- analysis_results()$original$pca$sdev^2 / sum(analysis_results()$original$pca$sdev^2)
			
			if (! is.null(analysis_results()$corrected)) {
				corr_var <- analysis_results()$corrected$pca$sdev^2 / sum(analysis_results()$corrected$pca$sdev^2)
				plot_pca_variance_comparison(orig_var, corr_var)
			} else {
				plot_pca_variance_single(orig_var)
			}
		})
		
		# Heatmap - Original
		output$heatmap_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "Original Data"
			)
		})
		
		# Heatmap - Corrected
		output$heatmap_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "ComBat Corrected Data"
			)
		})
	})
}