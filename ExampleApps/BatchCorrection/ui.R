# UI : BatchCorrection #####

ui <- dashboardPage(
	dashboardHeader(title = "Batch Correction"),
	dashboardSidebar(
		tags$h6(version),
		sidebarMenu(
			menuItem("Upload", tabName = "data_tab", icon = icon("database")),
			menuItem("ComBat Correction", tabName = "combat_tab", icon = icon("magic")),
			#menuItem("Batch Visualization", tabName = "viz_tab", icon = icon("chart-area")),
			menuItem("Export Data", tabName = "export_tab", icon = icon("download"))#,
			
			
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

			),
			## ComBat ####
			tabItem(
				tabName = "combat_tab",
				fluidRow(
					column(12,
					column(6,mod_eset_selector_standalone_ui("initial_select",debug = run_debug)),
					column(6,
								 mod_eset_selector_standalone_ui("combat_data",debug = run_debug),
								 )
					),
								 #mod_eset_selector_standalone_ui("vis_input",T,T,T,T)),
					column(12,
						column(4,
								 #mod_sample_group_selector_ui("sample_group", debug = run_debug)
								 mod_column_selector_ui(
								 	"sample_group",
								 	label = "Sample Grouping Column",
								 	help_text = "Primary biological grouping variable",
								 	show_info = TRUE,
								 	debug = run_debug
								 )
					),
					column(4,
								 #mod_batch_column_selector_ui("column_selector", debug = run_debug)
								 mod_column_selector_ui(
								 	"batch_columns",
								 	label = "Batch Testing Columns",
								 	help_text = "Select columns to test for batch effects",
								 	show_info = TRUE,
								 	debug = TRUE
								 )
					),
					column(4,
								 mod_combat_correction_selector_ui("combat_selector", debug = run_debug)
								 )
					),
					column(12,
								 mod_combat_single_ui("combat_single", show_auto_run_toggle = TRUE, debug = run_debug)
								 ),
					#column(12,mod_combat_correction_selector_ui("selector", debug = debug)),
					column(12,
					tabsetPanel(
						tabPanel('Batch Analysis',
										 #mod_eset_selector_standalone_ui("initial_select",T,T,T,T),
										 
								
										 column(12,
										 			 mod_batch_combined_analysis_ui("batch_combined", debug = run_debug),
										 			 mod_batch_testing_ui("batch_testing", debug = run_debug),
										 			 mod_batch_distribution_test_ui("distribution_test", debug = run_debug),
										 			 mod_annotation_analysis_ui("annotation_analysis", debug = run_debug),
										 )
										 
										 ),
						# tabPanel("Run ComBat",
						# 	
						# 	mod_combat_correction_ui("combat", debug = run_debug)
						# 	
						# ),
						tabPanel('Visualisation',
						
										  
										 	
										 	mod_batch_visualization_ui("batch_viz", debug = run_debug)
										 	
										 )
					))
				)
			),
			## Viz ####
			# tabItem(
			# 	tabName = "viz_tab",
			# 	mod_eset_selector_standalone_ui("vis_input",T,T,T,T),
			# 
			# 	mod_batch_visualization_ui("batch_viz", debug = run_debug)
			# 	
			# ),
			## Export ####
			tabItem(
				tabName = "export_tab",
				mod_combat_multi_assay_ui("combat_multi",debug = run_debug),
				mod_expset_export_ui("expset_export")
			)
		)
	)
)