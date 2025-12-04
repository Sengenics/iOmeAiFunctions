# UI : BatchCorrection #####

ui <- dashboardPage(
	dashboardHeader(title = "Batch Testing App"),
	dashboardSidebar(
		
		sidebarMenu(
			menuItem("Data Selection", tabName = "data_tab", icon = icon("database")),
			#menuItem("Annotation Analysis", tabName = "annotation_tab", icon = icon("table")),
			
			menuItem("Batch Analysis", tabName = "batch_tab", icon = icon("flask")),
			menuItem("ComBat Correction", tabName = "combat_tab", icon = icon("magic")),
			
			menuItem("Results", tabName = "results_tab", icon = icon("chart-bar"))
			
			
		),
		uiOutput("debug_ui")
	),
	dashboardBody(
		tabItems(
			tabItem(
				tabName = "data_tab",
				mod_app_data_selection_ui("data_select", debug = run_debug)
			),
			# tabItem(
			# 	tabName = "annotation_tab",
			# 	mod_annotation_analysis_ui("annotation_analysis", debug = run_debug)
			# ),
			tabItem(
				tabName = "batch_tab",
				mod_annotation_analysis_ui("annotation_analysis", debug = run_debug),
				column(6,
					mod_sample_group_selector_ui("sample_group", debug = run_debug)
				),
				column(6,
					mod_batch_column_selector_ui("column_selector", debug = run_debug)
				),
				column(12,
					tags$h3('Batch Testing Module'),
					mod_batch_testing_ui("batch_testing", debug = run_debug),
					mod_batch_distribution_test_ui("distribution_test", debug = run_debug),
					mod_batch_combined_analysis_ui("batch_combined", debug = run_debug)
				)
				
				#h2("Batch Analysis - Coming Soon")
			),
			tabItem(
				tabName = "combat_tab",
				mod_combat_correction_ui("combat", debug = run_debug)
			),
			tabItem(
				tabName = "results_tab",
				h2("Results - Coming Soon")
			)
		)
	)
)