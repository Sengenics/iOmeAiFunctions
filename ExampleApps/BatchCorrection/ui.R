# UI Batch Correction ####
ui <- dashboardPage(
	skin = "blue",
	
	dashboardHeader(
		title = " iOme AI - Batch Correction",
		titleWidth = 300
	),
	
	## sidebar ####
	dashboardSidebar(
		width = 300,
		sidebarMenu(
			id = "sidebar",
			menuItem("Data Selection", tabName = "data_select", icon = icon("database")),
			menuItem("Configure Heatmap", tabName = "configure", icon = icon("sliders")),
			menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
			menuItem("Help", tabName = "help", icon = icon("question-circle")),
			hr(),
			uiOutput("debug_ui")
		)
	),
	
	dashboardBody(
		tags$head(
			tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .debug-box {
          border: 2px solid #f39c12;
        }
        .upload-box {
          border: 2px solid #00a65a;
        }
      "))
		),
		
		## tabs ####
		tabItems(
			### Data selection tab ####
			tabItem(
				tabName = "data_select",
				
				# File Upload Box
				fluidRow(
					box(
						title = "Import New ExpSet Data",
						width = 12,
						status = "success",
						solidHeader = TRUE,
						class = "upload-box",
						collapsible = TRUE,
						collapsed = FALSE,
						
						p(icon("upload"), strong("Upload a new ExpSet. rds file"), "to import custom expression data"),
						
						mod_expset_import_ui("expset_import",debug = run_debug)
					)
				),
				
				# ExpressionSet Selection
				fluidRow(
					box(
						title = "ExpressionSet Data Selection",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						
						p("Select the expression data to visualize.  The app will auto-load from package data or use your uploaded file."),
						
						fluidRow(
							column(
								width = 6,
								box(
									title = "Select ExpressionSet",
									width = NULL,
									status = "info",
									solidHeader = TRUE,
									
									p("Select which ExpressionSet to visualize:"),
									mod_eset_selector_ui("eset_select"),
									
									hr(),
									
									p(strong("Selected Data Info:")),
									verbatimTextOutput("eset_info")
								)
							),
							column(
								width = 6,
								box(
									title = "Data Summary",
									width = NULL,
									status = "success",
									solidHeader = TRUE,
									
									verbatimTextOutput("eset_summary")
								)
							)
						)
					)
				),
				
				# Debug & Diagnostics
				fluidRow(
					box(
						title = "Debug & Diagnostics",
						width = 12,
						status = "warning",
						solidHeader = TRUE,
						class = "debug-box",
						collapsible = TRUE,
						collapsed = TRUE,
						
						p(icon("tools"), strong("Development Tools"), "- Inspect data structure and troubleshoot issues"),
						
						fluidRow(
							column(
								width = 3,
								actionButton(
									"debug_data",
									"🔍 Enter Debug Mode",
									icon = icon("bug"),
									class = "btn-warning btn-block"
								),
								helpText("Pause execution and inspect objects in browser mode")
							),
							column(
								width = 3,
								actionButton(
									"run_diagnostics",
									"📊 Run Full Diagnostics",
									icon = icon("stethoscope"),
									class = "btn-info btn-block"
								),
								helpText("Print detailed structure analysis")
							),
							column(
								width = 3,
								actionButton(
									"quick_check",
									"⚡ Quick Check",
									icon = icon("bolt"),
									class = "btn-secondary btn-block"
								),
								helpText("Quick validation of data structure")
							),
							column(
								width = 3,
								actionButton(
									"goto_configure",
									"Configure Heatmap →",
									icon = icon("arrow-right"),
									class = "btn-success btn-block"
								),
								helpText("Navigate to configuration")
							)
						)
					)
				),
				
				# Status indicators
				fluidRow(
					box(
						title = "Data Status",
						width = 12,
						status = "info",
						
						fluidRow(
							column(
								width = 3,
								valueBoxOutput("status_eset_list", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_selected", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_samples", width = NULL)
							),
							column(
								width = 3,
								valueBoxOutput("status_features", width = NULL)
							)
						)
					)
				)
			)
		)
	)
)