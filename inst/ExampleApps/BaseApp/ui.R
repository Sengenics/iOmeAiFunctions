# Base APP ####

ui <- dashboardPage(
	dashboardHeader(title = "Base App"),
	dashboardSidebar(
		sidebarMenu(
			menuItem("Data Selection", tabName = "data_tab", icon = icon("database")),
			menuItem("Batch Analysis", tabName = "batch_tab", icon = icon("flask")),
			menuItem("Results", tabName = "results_tab", icon = icon("chart-bar"))
		),
		uiOutput("debug_ui")
	),
	dashboardBody(
		tabItems(
			
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
			
			tabItem(
				tabName = "data_tab",
				mod_app_data_selection_ui("data_select", debug = run_debug)
			),
			tabItem(
				tabName = "batch_tab",
				h2("Batch Analysis - Coming Soon")
			),
			tabItem(
				tabName = "results_tab",
				h2("Results - Coming Soon")
			)
		)
	)
)