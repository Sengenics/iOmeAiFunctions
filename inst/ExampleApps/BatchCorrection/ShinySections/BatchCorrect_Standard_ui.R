

mode = 'standard'

ui_content = 	fluidRow(
	shinydashboard::box(
		title = paste("BatchCorrection Module ",mode),
		width = NULL,
		status = "primary",
		solidHeader = TRUE,
		collapsible = TRUE,
		collapsed = TRUE,
		p("Module build in iOmeAiFunctions"),
		p("Version: 1.0")
	),
	uiOutput('debug_bc_server_ui'),
	column(12,
				 column(6,mod_eset_selector_standalone_ui(mid("initial_select",mode),debug = run_debug)),
				 column(6,
				 			 mod_eset_selector_standalone_ui(mid("combat_data",mode),debug = run_debug),
				 )
	),
	#mod_eset_selector_standalone_ui("vis_input",T,T,T,T)),
	column(12,
				 column(4,
				 			 #mod_sample_group_selector_ui("sample_group", debug = run_debug)
				 			 mod_column_selector_ui(
				 			 	mid("sample_group",mode),
				 			 	label = "Sample Grouping Column",
				 			 	help_text = "Primary biological grouping variable",
				 			 	show_info = TRUE,
				 			 	debug = run_debug
				 			 )
				 ),
				 column(4,
				 			 #mod_batch_column_selector_ui("column_selector", debug = run_debug)
				 			 mod_column_selector_ui(
				 			 	mid("batch_columns",mode),
				 			 	label = "Batch Testing Columns",
				 			 	help_text = "Select columns to test for batch effects",
				 			 	show_info = TRUE,
				 			 	debug = TRUE
				 			 )
				 ),
				 column(4,
				 			 mod_combat_correction_selector_ui(mid("combat_selector",mode), debug = run_debug)
				 )
	),
	column(12,
				 mod_combat_single_ui(mid("combat_single",mode), show_auto_run_toggle = TRUE, debug = run_debug)
	),
	#column(12,mod_combat_correction_selector_ui("selector", debug = debug)),
	column(12,
				 #tabsetPanel(
				 	#tabPanel('Batch Analysis',
				 					 #mod_eset_selector_standalone_ui("initial_select",T,T,T,T),
				 					 
				 					 
				 					 #column(12,
				 mod_batch_combined_analysis_ui(mid("batch_combined",mode), debug = run_debug),
				 					 			 #mod_batch_testing_ui(mid("batch_testing",mode), debug = run_debug),
				 					 			 #mod_batch_distribution_test_ui(mid("distribution_test",mode), debug = run_debug),
				 					 			 #mod_annotation_analysis_ui(mid("annotation_analysis",mode), debug = run_debug),
				 					 #),
				 					 
				 	#),
				 	# tabPanel("Run ComBat",
				 	# 	
				 	# 	mod_combat_correction_ui("combat", debug = run_debug)
				 	# 	
				 	# ),
				 	#tabPanel('Visualisation',
				 					 
				 					 
				 					 
				 		mod_batch_visualization_ui(mid("batch_viz",mode), debug = run_debug)
				 					 
				 	)
				 #))
)

# ═══════════════════════════════════════════════════════════════
# Wrap in dashboardPage if running standalone
# ═══════════════════════════════════════════════════════════════

# Check if we're in a standalone context
# If STANDALONE_MODE exists and is TRUE, wrap in full dashboard
if (exists("STANDALONE_MODE") && isTRUE(STANDALONE_MODE)) {
	
	# ✅ Standalone mode - full dashboard
	dashboardPage(
		dashboardHeader(disable = TRUE),
		dashboardSidebar(disable = TRUE),
		dashboardBody(ui_content)
	)
	
} else {
	
	# ✅ Embedded mode - just the content
	ui_content
}
