# UI : BatchCorrection #####

ui <- dashboardPage(
	dashboardHeader(title = "Batch Testing App"),
	dashboardSidebar(
		
		sidebarMenu(
			menuItem("Upload", tabName = "data_tab", icon = icon("database")),
			#menuItem("Annotation Analysis", tabName = "annotation_tab", icon = icon("table")),
			
			menuItem("Batch Analysis", tabName = "batch_tab", icon = icon("flask")),
			menuItem("ComBat Correction", tabName = "combat_tab", icon = icon("magic")),
			menuItem("Batch Visualization", tabName = "viz_tab", icon = icon("chart-area")),
			menuItem("Export Data", tabName = "export_tab", icon = icon("download"))#,
			#menuItem("Results", tabName = "results_tab", icon = icon("chart-bar"))
			
			
		),
		uiOutput("debug_ui")
	),
	dashboardBody(
		tabItems(
			## Data ####
			tabItem(
				tabName = "data_tab",
				fluidRow(
					box(
						title = "Upload ExpSet_list. rds",
						width = 12,
						status = "success",
						solidHeader = TRUE,
						mod_expset_import_ui("expset_import", debug = run_debug)
					)
				),
				
				mod_expset_viewer_ui("expset_viewer")
				
				#mod_app_data_selection_ui("data_select", debug = run_debug),
				#mod_eset_subset_ui("subset", debug = run_debug),
				#mod_eset_transform_ui("transform", debug = run_debug)
			),
			# tabItem(
			# 	tabName = "annotation_tab",
			# 	mod_annotation_analysis_ui("annotation_analysis", debug = run_debug)
			# ),
			## Batch Tab #####
			tabItem(
				tabName = "batch_tab",
				mod_eset_selector_standalone_ui("initial_select",T,T,T,T),
				
				column(6,
					mod_sample_group_selector_ui("sample_group", debug = run_debug)
				),
				column(6,
					mod_batch_column_selector_ui("column_selector", debug = run_debug)
				),
				column(12,
					mod_batch_combined_analysis_ui("batch_combined", debug = run_debug),
					#tags$h3('Batch Testing Module'),
					mod_batch_testing_ui("batch_testing", debug = run_debug),
					mod_batch_distribution_test_ui("distribution_test", debug = run_debug),
					mod_annotation_analysis_ui("annotation_analysis", debug = run_debug),
					#mod_batch_combined_analysis_ui("batch_combined", debug = run_debug)
				)
				
				#h2("Batch Analysis - Coming Soon")
			),
			## ComBat ####
			tabItem(
				tabName = "combat_tab",
				mod_eset_selector_standalone_ui("combat_data",T,T,T,T),
				mod_combat_correction_ui("combat", debug = run_debug)
			),
			## Viz ####
			tabItem(
				tabName = "viz_tab",
				column(6,
							 mod_eset_selector_standalone_ui("vis_input",T,T,T,T)
				),
				column(6,
							 radioButtons('viz_output','Corrected ExpSet',c('_ComBat','exp corrected'),inline = T)
							 #mod_eset_selector_standalone_ui("combat_data",T,T,T,T)
							 ),
				column(12,
							 
					mod_batch_visualization_ui("batch_viz", debug = run_debug)
				)
			),
			## Export ####
			tabItem(
				tabName = "export_tab",
				h2("Export ExpressionSet List"),
				p("Save your ExpressionSet list including all ComBat corrections"),
				mod_expset_manager_ui("expset_manager", debug = TRUE),
				mod_expset_export_ui("expset_export")
			)
			# tabItem(
			# 	tabName = "results_tab",
			# 	h2("Results - Coming Soon")
			# )
		)
	)
)