#' pFC v2 Analysis Shiny Module - UI
#'
#' UI component for pFC v2 baseline-driven penetrance analysis.
#'
#' @param id Module namespace ID
#' @param use_box Logical; wrap in shinydashboard box? Default FALSE.
#'
#' @export
pFC_v2_UI <- function(id, use_box = FALSE) {
	ns <- NS(id)
	
	
	
	ui_content <- tagList(
		test_debug_module_UI(ns('test_pFC_v2')),
		conditionalPanel(
			condition = "true",
			uiOutput(ns("debug_ui"))
		),
		
		h3("pFC v2 Analysis"),
		
		fluidRow(
			column(
				width = 4,
				
				selectInput(
					ns("v2_var"),
					"Grouping Variable:",
					choices = NULL,
					selected = NULL
				),
				
				selectInput(
					ns("v2_baseline_source"),
					"Baseline Source:",
					choices = c("all", "group"),
					selected = "all"
				),
				
				conditionalPanel(
					condition = sprintf("input['%s'] == 'group'", ns("v2_baseline_source")),
					selectInput(
						ns("v2_baseline_group"),
						"Baseline Group:",
						choices = NULL,
						selected = NULL
					)
				),
				
				numericInput(
					ns("v2_fold_change"),
					"Fold Change Threshold:",
					value = 2,
					min = 1,
					max = 10,
					step = 0.1
				),
				
				selectInput(
					ns("v2_threshold_method"),
					"Threshold Method:",
					choices = c("fc", "2mad", "3mad"),
					selected = "3mad"
				)
			),
			
			column(
				width = 4,
				
				checkboxInput(
					ns("v2_trim_outliers"),
					"Trim Outliers Using Initial MAD Cutoff",
					value = TRUE
				),
				
				numericInput(
					ns("v2_outlier_mad_multiplier"),
					"Outlier MAD Multiplier:",
					value = 4,
					min = 1,
					max = 10,
					step = 0.5
				),
				
				numericInput(
					ns("v2_min_baseline_n"),
					"Minimum Baseline Samples:",
					value = 3,
					min = 1,
					max = 100,
					step = 1
				),
				
				textInput(
					ns("v2_descriptor"),
					"Output Descriptor:",
					value = "pFC_v2_analysis"
				)
			),
			
			column(
				width = 4,
				
				div(
					class = "alert alert-info",
					tags$strong("pFC v2 Summary"),
					tags$p(
						"This module builds per-protein baseline thresholds from either all samples ",
						"or a selected baseline group, then calculates penetrance across all groups."
					),
					tags$ul(
						tags$li(tags$strong("fc: "), "Signal above the per-protein fold-change threshold"),
						tags$li(tags$strong("2mad: "), "Signal above baseline median + 2 x MAD"),
						tags$li(tags$strong("3mad: "), "Signal above baseline median + 3 x MAD")
					)
				)
			)
		),
		
		fluidRow(
			column(
				width = 12,
				actionButton(
					ns("run_pfc_v2"),
					"Run pFC v2 Analysis",
					icon = icon("play"),
					class = "btn-success btn-lg",
					style = "width: 100%;"
				)
			)
		),
		
		hr(),
		
		fluidRow(
			column(
				width = 12,
				tabsetPanel(
					tabPanel('Tables',
			id = ns("v2_results_tabs"),
			tabsetPanel(
			tabPanel(
				"Guide",
				br(),
				uiOutput(ns("v2_results_guide"))
			),
			
			tabPanel(
				"Baseline",
				br(),
				DT::DTOutput(ns("v2_baseline_table"))
			),
			
			tabPanel(
				"Sample Flags",
				br(),
				DT::DTOutput(ns("v2_sample_flags_table"))
			),
			
			tabPanel(
				"Master Penetrance",
				br(),
				DT::DTOutput(ns("v2_master_penetrance_table"))
			),
			
			tabPanel(
				"Group Stats",
				br(),
				DT::DTOutput(ns("v2_master_group_stats_table"))
			),
			
			tabPanel(
				"Global Stats",
				br(),
				DT::DTOutput(ns("v2_master_global_stats_table"))
			),
			
			tabPanel(
				"Fisher",
				br(),
				DT::DTOutput(ns("v2_fisher_table"))
			),
			
			tabPanel(
				"ChiSq",
				br(),
				DT::DTOutput(ns("v2_chisq_table"))
			),
			
			tabPanel(
				"Logistic Group",
				br(),
				DT::DTOutput(ns("v2_logistic_group_table"))
			),
			
			tabPanel(
				"Logistic Global",
				br(),
				DT::DTOutput(ns("v2_logistic_global_table"))
			),
			
			tabPanel(
				"Firth",
				br(),
				DT::DTOutput(ns("v2_firth_table"))
			)
		)),
		tabPanel("Plots",
						 tabsetPanel(
						 	tabPanel('Violin Plots',
						 					 br(),
						 					 annotated_violin_UI(ns("pfc_v2_plot_test"))
						 
							 
							 )
						 )
		)
		)
			)
		)
	)
	
	
	if (use_box) {
		if (!requireNamespace("shinydashboard", quietly = TRUE)) {
			stop("shinydashboard package required when use_box = TRUE")
		}
		
		shinydashboard::box(
			title = "pFC v2 Analysis",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			collapsible = TRUE,
			ui_content
		)
	} else {
		ui_content
	}
}


#' pFC v2 Analysis Shiny Module - Server
#'
#' Server component for pFC v2 baseline-driven penetrance analysis.
#'
#' @param id Module namespace ID
#' @param eset_reactive Reactive expression returning an ExpressionSet object
#' @param assay_name Character string naming the assayData element to analyze
#' @param default_var_reactive Reactive expression returning the default grouping variable
#' @param debug Logical; enable debug tools
#'
#' @export
pFC_v2_Server <- function(id,
													eset_reactive,
													assay_name,
													default_var_reactive = reactive(NULL),
													debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		rv <- reactiveValues(
			baseline_stats = NULL,
			sample_flags = NULL,
			test_results = NULL,
			v2_plot_data = NULL,
			v2_violin_plots = NULL,
			var_initialized = FALSE
		)
		
		test_debug_module_Server('test_pFC_v2')
		output$debug_ui <- renderUI({
			if (debug == TRUE) {
				actionButton(session$ns("debug"), "pFC v2 Debug", class = "btn-warning btn-sm")
			}
		})
		
		observeEvent(input$debug, {
			message("\n========== pFC v2 DEBUG ==========")
			message("Available objects:")
			message("  eset_reactive()")
			message("  rv$baseline_stats")
			message("  rv$sample_flags")
			message("  rv$test_results")
			message("  input$v2_var")
			message("  input$v2_baseline_source")
			message("  input$v2_baseline_group")
			message("  input$v2_fold_change")
			message("  input$v2_threshold_method")
			message("  input$v2_trim_outliers")
			message("  input$v2_outlier_mad_multiplier")
			message("  input$v2_min_baseline_n")
			message("==================================\n")
			
			browser()
		})
		
		
		observe({
			req(eset_reactive())
			eset <- eset_reactive()
			metadata_cols <- colnames(Biobase::pData(eset))
			default_var <- default_var_reactive()
			
			selected_var <- if (!is.null(default_var) && default_var %in% metadata_cols) {
				default_var
			} else {
				metadata_cols[1]
			}
			
			updateSelectInput(
				session,
				"v2_var",
				choices = metadata_cols,
				selected = selected_var
			)
			
			rv$var_initialized <- TRUE
		})
		
		observe({
			req(eset_reactive(), input$v2_var)
			eset <- eset_reactive()
			
			var_values <- unique(as.character(Biobase::pData(eset)[[input$v2_var]]))
			var_values <- var_values[!is.na(var_values) & var_values != ""]
			
			updateSelectInput(
				session,
				"v2_baseline_group",
				choices = var_values,
				selected = if (length(var_values) > 0) var_values[1] else NULL
			)
		})
		
		output$v2_results_guide <- renderUI({
			tagList(
				div(
					class = "alert alert-info",
					tags$strong("How to interpret pFC v2"),
					tags$ul(
						tags$li("Baseline: per-protein baseline estimates and derived thresholds."),
						tags$li("Sample Flags: one row per protein per sample showing whether that sample exceeds the selected thresholds."),
						tags$li("Master Penetrance: penetrance for every group in the selected grouping variable."),
						tags$li("Group Stats: group-specific one-vs-rest test results."),
						tags$li("Global Stats: whole-dataset tests for whether penetrance differs across groups."),
						tags$li("Fisher: exact one-vs-rest tests; useful for sparse counts."),
						tags$li("ChiSq: overall multi-group difference test."),
						tags$li("Logistic: model-based one-vs-rest and global tests."),
						tags$li("Firth: bias-reduced logistic regression, useful when ordinary logistic is unstable.")
					)
				)
			)
		})
		
		observeEvent(input$run_pfc_v2, {
			req(eset_reactive(), input$v2_var)
			
			progress_id <- "pfc_v2_progress"
			
			showNotification(
				"pFC v2: starting analysis...",
				type = "message",
				duration = NULL,
				id = progress_id
			)
			
			tryCatch({
				baseline_group_value <- NULL
				if (identical(input$v2_baseline_source, "group")) {
					baseline_group_value <- input$v2_baseline_group
				}
				
				showNotification(
					"pFC v2: calculating baseline statistics...",
					type = "message",
					duration = NULL,
					id = progress_id
				)
				
				rv$baseline_stats <- pFC_baseline_stats_v2(
					eset = eset_reactive(),
					assay_name = assay_name,
					var = input$v2_var,
					baseline_source = input$v2_baseline_source,
					baseline_group = baseline_group_value,
					fold_change = input$v2_fold_change,
					trim_outliers = isTRUE(input$v2_trim_outliers),
					outlier_mad_multiplier = input$v2_outlier_mad_multiplier,
					min_baseline_n = input$v2_min_baseline_n
				)
				
				showNotification(
					"pFC v2: generating per-sample threshold flags...",
					type = "message",
					duration = NULL,
					id = progress_id
				)
				
				rv$sample_flags <- pFC_sample_flags_v2(
					eset = eset_reactive(),
					assay_name = assay_name,
					baseline_stats = rv$baseline_stats,
					var = input$v2_var
				)
				
				showNotification(
					"pFC v2: running penetrance test suite...",
					type = "message",
					duration = NULL,
					id = progress_id
				)
				
				rv$test_results <- pFC_penetrance_test_suite_v2(
					sample_flags = rv$sample_flags,
					var = input$v2_var,
					threshold_method = input$v2_threshold_method
				)
				
				showNotification(
					"pFC v2: preparing plotting data...",
					type = "message",
					duration = NULL,
					id = progress_id
				)
				
				# rv$v2_plot_data <- pFC_v2_violin_plot_data(
				# 	sample_flags = rv$sample_flags,
				# 	master_group_stats = rv$test_results$master_group_stats,
				# 	master_global_stats = rv$test_results$master_global_stats,
				# 	var = input$v2_var,
				# 	threshold_method = input$v2_threshold_method,
				# 	min_penetrance = 10,
				# 	top_n = 30
				# )
				
				showNotification(
					"pFC v2: finalising outputs...",
					type = "message",
					duration = NULL,
					id = progress_id
				)
				
				removeNotification(progress_id)
				
				showNotification(
					"pFC v2 analysis completed.",
					type = "message",
					duration = 3
				)
				
			}, error = function(e) {
				removeNotification(progress_id)
				showNotification(
					paste("pFC v2 error:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
			print("pFC v2 analysis completed")
		})
		
		
		# observeEvent(input$run_pfc_v2, {
		# 	req(eset_reactive(), input$v2_var)
		# 	#req(rv$sample_flags, rv$test_results, input$v2_var, input$v2_threshold_method)
		# 	
		# 	showNotification("Running pFC v2 analysis...", type = "message", duration = NULL, id = "pfc_v2_progress")
		# 	
		# 	tryCatch({
		# 		baseline_group_value <- NULL
		# 		if (identical(input$v2_baseline_source, "group")) {
		# 			baseline_group_value <- input$v2_baseline_group
		# 		}
		# 		
		# 		rv$baseline_stats <- pFC_baseline_stats_v2(
		# 			eset = eset_reactive(),
		# 			assay_name = assay_name,
		# 			var = input$v2_var,
		# 			baseline_source = input$v2_baseline_source,
		# 			baseline_group = baseline_group_value,
		# 			fold_change = input$v2_fold_change,
		# 			trim_outliers = isTRUE(input$v2_trim_outliers),
		# 			outlier_mad_multiplier = input$v2_outlier_mad_multiplier,
		# 			min_baseline_n = input$v2_min_baseline_n
		# 		)
		# 		
		# 		rv$sample_flags <- pFC_sample_flags_v2(
		# 			eset = eset_reactive(),
		# 			assay_name = assay_name,
		# 			baseline_stats = rv$baseline_stats,
		# 			var = input$v2_var
		# 		)
		# 		
		# 		rv$test_results <- pFC_penetrance_test_suite_v2(
		# 			sample_flags = rv$sample_flags,
		# 			var = input$v2_var,
		# 			threshold_method = input$v2_threshold_method
		# 		)
		# 		
		# 		rv$v2_plot_data <- pFC_v2_violin_plot_data(
		# 			sample_flags = rv$sample_flags,
		# 			master_group_stats = rv$test_results$master_group_stats,
		# 			master_global_stats = rv$test_results$master_global_stats,
		# 			var = input$v2_var,
		# 			threshold_method = input$v2_threshold_method,
		# 			min_penetrance = 10,
		# 			top_n = 30
		# 		)
		# 		
		# 		# rv$v2_violin_plots <- pFC_v2_plot_violins(
		# 		# 	v2_plot_data = rv$v2_plot_data,
		# 		# 	violin_ncol = 3,
		# 		# 	violin_nrow = 3
		# 		# )
		# 		
		# 		removeNotification("pfc_v2_progress")
		# 		showNotification("pFC v2 analysis completed.", type = "message", duration = 3)
		# 		
		# 	}, error = function(e) {
		# 		removeNotification("pfc_v2_progress")
		# 		showNotification(paste("pFC v2 error:", e$message), type = "error", duration = 10)
		# 		print(e)
		# 	})
		# })
		
		output$v2_baseline_table <- DT::renderDT({
			req(rv$baseline_stats)
			
			DT::datatable(
				rv$baseline_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_sample_flags_table <- DT::renderDT({
			req(rv$sample_flags)
			
			DT::datatable(
				rv$sample_flags,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_master_penetrance_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$master_penetrance)
			
			DT::datatable(
				rv$test_results$master_penetrance,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_master_group_stats_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$master_group_stats)
			
			DT::datatable(
				rv$test_results$master_group_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_master_global_stats_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$master_global_stats)
			
			DT::datatable(
				rv$test_results$master_global_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_fisher_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$fisher_stats)
			
			DT::datatable(
				rv$test_results$fisher_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_chisq_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$chisq_stats)
			
			DT::datatable(
				rv$test_results$chisq_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_logistic_group_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$logistic_group_stats)
			
			DT::datatable(
				rv$test_results$logistic_group_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_logistic_global_table <- DT::renderDT({
			req(rv$test_results, rv$test_results$logistic_global_stats)
			
			DT::datatable(
				rv$test_results$logistic_global_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		output$v2_firth_table <- DT::renderDT({
			req(rv$test_results)
			
			firth_df <- rv$test_results$firth_group_stats
			
			validate(
				need(!is.null(firth_df), "Firth results are not available."),
				need(is.data.frame(firth_df), "Firth results are not a data frame."),
				need(nrow(firth_df) > 0, "Firth results table is empty.")
			)
			
			DT::datatable(
				firth_df,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		

		plot_spec_reactive <- reactive({
			req(rv$sample_flags, rv$test_results)

			pFC_v2_build_plot_spec(
				sample_flags = rv$sample_flags,
				master_group_stats = rv$test_results$master_group_stats,
				master_global_stats = rv$test_results$master_global_stats,
				var = input$v2_var,
				threshold_method = input$v2_threshold_method,
				min_penetrance = 10,
				top_n = 30
			)
		})
		
		annotated_violin_Server("pfc_v2_plot_test",plot_spec_reactive = plot_spec_reactive, debug = debug)
		
		
		# output$v2_violin_ui <- renderUI({
		# 	req(rv$v2_violin_plots, rv$v2_violin_plots$log2_plots)
		# 	
		# 	n_plots <- length(rv$v2_violin_plots$log2_plots)
		# 	
		# 	tagList(
		# 		if (n_plots > 1) {
		# 			fluidRow(
		# 				column(
		# 					width = 12,
		# 					sliderInput(
		# 						session$ns("v2_violin_page"),
		# 						"Page:",
		# 						min = 1,
		# 						max = n_plots,
		# 						value = 1,
		# 						step = 1,
		# 						width = "100%"
		# 					)
		# 				)
		# 			)
		# 		},
		# 		plotOutput(session$ns("v2_violin_plot"), height = "900px")
		# 	)
		# })
		# 
		# output$v2_violin_plot <- renderPlot({
		# 	req(rv$v2_violin_plots, rv$v2_violin_plots$log2_plots)
		# 	
		# 	page_num <- if (!is.null(input$v2_violin_page)) input$v2_violin_page else 1
		# 	rv$v2_violin_plots$log2_plots[[page_num]]
		# })
		# 
		# 
		# output$v2_rfu_violin_ui <- renderUI({
		# 	req(rv$v2_violin_plots, rv$v2_violin_plots$rfu_plots)
		# 	
		# 	n_plots <- length(rv$v2_violin_plots$rfu_plots)
		# 	
		# 	tagList(
		# 		if (n_plots > 1) {
		# 			fluidRow(
		# 				column(
		# 					width = 12,
		# 					sliderInput(
		# 						session$ns("v2_rfu_violin_page"),
		# 						"Page:",
		# 						min = 1,
		# 						max = n_plots,
		# 						value = 1,
		# 						step = 1,
		# 						width = "100%"
		# 					)
		# 				)
		# 			)
		# 		},
		# 		plotOutput(session$ns("v2_rfu_violin_plot"), height = "900px")
		# 	)
		# })
		# 
		# 
		# output$v2_violin_plot <- renderPlot({
		# 	req(rv$v2_violin_plots, rv$v2_violin_plots$log2_plots)
		# 	
		# 	page_num <- if (!is.null(input$v2_violin_page)) input$v2_violin_page else 1
		# 	rv$v2_violin_plots$log2_plots[[page_num]]
		# })
		
		
		
		
	})
	

	
}

