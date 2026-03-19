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
		
		tabsetPanel(
			id = ns("v2_results_tabs"),
			
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
			var_initialized = FALSE
		)
		
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
			
			showNotification("Running pFC v2 analysis...", type = "message", duration = NULL, id = "pfc_v2_progress")
			
			tryCatch({
				baseline_group_value <- NULL
				if (identical(input$v2_baseline_source, "group")) {
					baseline_group_value <- input$v2_baseline_group
				}
				
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
				
				rv$sample_flags <- pFC_sample_flags_v2(
					eset = eset_reactive(),
					assay_name = assay_name,
					baseline_stats = rv$baseline_stats,
					var = input$v2_var
				)
				
				rv$test_results <- pFC_penetrance_test_suite_v2(
					sample_flags = rv$sample_flags,
					var = input$v2_var,
					threshold_method = input$v2_threshold_method
				)
				
				removeNotification("pfc_v2_progress")
				showNotification("pFC v2 analysis completed.", type = "message", duration = 3)
				
			}, error = function(e) {
				removeNotification("pfc_v2_progress")
				showNotification(paste("pFC v2 error:", e$message), type = "error", duration = 10)
				print(e)
			})
		})
		
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
		
	})
}

