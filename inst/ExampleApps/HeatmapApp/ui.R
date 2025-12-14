# UI ####
ui <- dashboardPage(
	skin = "blue",
	
	dashboardHeader(
		title = "Heatmap Viewer - iOme AI",
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
			),
			

			
			### Configure tab ####
			tabItem(
				tabName = "configure",
				fluidRow(
					box(
						title = "Enhanced Heatmap Configuration",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						mod_heatmap_controls_enhanced_ui("heatmap_controls", debug = run_debug)
					)
				),
				fluidRow(
					box(
						title = "Ready to Visualize",
						width = 12,
						status = "success",
						actionButton(
							"goto_heatmap",
							"View Heatmap →",
							icon = icon("th"),
							class = "btn-success btn-lg"
						)
					)
				)
			),
			
			### Heatmap tab (ENHANCED) ####
			tabItem(
				tabName = "heatmap",
				fluidRow(
					box(
						title = "Heatmap Visualization",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						mod_heatmap_display_enhanced_ui("heatmap_display", debug = run_debug)
					)
				)
			),
			
			
			### Help tab ####
			tabItem(
				tabName = "help",
				
				fluidRow(
					box(
						title = "Heatmap Viewer Help",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						
						h3("Quick Start Guide"),
						
						h4("Recommended Workflow"),
						tags$ol(
							tags$li(strong("Data Selection:"), "Upload or select your ExpressionSet data"),
							tags$li(strong("Configure:"), "Choose assay, annotations, and apply filters"),
							tags$li(strong("Heatmap:"), "View and export your heatmap"),
							tags$li(strong("Export:"), "Download plots or filtered data")
						),
						
						hr(),
						
						h4("1. Import Data (Optional)"),
						p("You can either:"),
						tags$ul(
							tags$li(strong("Use Default Data:"), "The app will auto-load from package data"),
							tags$li(strong("Upload New Data:"), "Click 'Browse.. .' to select your own ExpSet.rds file")
						),
						
						hr(),
						
						h4("2. Data Selection"),
						p("Select your ExpressionSet from the dropdown.  The app shows:"),
						tags$ul(
							tags$li("Number of samples (columns)"),
							tags$li("Number of features (rows)"),
							tags$li("Available assays"),
							tags$li("Available metadata columns")
						),
						
						hr(),
						
						h4("3. Configure Heatmap"),
						
						h5("Assay Selection"),
						p("Choose which data matrix to visualize (e.g., 'exprs', 'log2_exprs', 'normalized')"),
						
						h5("Annotations"),
						tags$ul(
							tags$li(strong("Sample Annotations:"), "Select pData columns to annotate heatmap columns (e.g., Sample_Group, Disease_State)"),
							tags$li(strong("Feature Annotations:"), "Select fData columns to annotate heatmap rows (e.g., Protein_Family, Function)")
						),
						
						h5("Filters"),
						p(strong("Multi-Level Cascading Filters"), "allow you to progressively narrow down your data:"),
						
						p(strong("Sample Filters Example:")),
						tags$ol(
							tags$li("Add Filter Level 1: Sample_Group = 'Clinical'"),
							tags$li("Add Filter Level 2: Disease_State = 'Cancer' (only shows Cancer samples within Clinical)"),
							tags$li("Add Filter Level 3: Age > 50 (only shows Cancer patients over 50)")
						),
						
						p(strong("Feature Filters Example:")),
						tags$ol(
							tags$li("Add Filter Level 1: Protein_Family = 'Kinase'"),
							tags$li("Add Filter Level 2: Function = 'Signaling' (only shows signaling kinases)")
						),
						
						p(em("Each filter level narrows based on the previous level (AND logic).")),
						
						hr(),
						
						h4("4. View Heatmap"),
						p("Click", strong("'View Heatmap →'"), "to generate the visualization.  The heatmap shows:"),
						tags$ul(
							tags$li("Expression values as colors"),
							tags$li("Sample annotations as colored bars at the top"),
							tags$li("Feature annotations as colored bars on the left"),
							tags$li("Hierarchical clustering of samples and features")
						),
						
						hr(),
						
						h4("5. Export Results"),
						p("Download your results:"),
						tags$ul(
							tags$li(strong("Download Plot:"), "Save heatmap as PNG image"),
							tags$li(strong("Download Data:"), "Export filtered expression matrix as CSV"),
							tags$li(strong("Download ExpSet:"), "Save filtered ExpressionSet as RDS for further analysis")
						)
					)
				),
				
				fluidRow(
					box(
						title = "Understanding Annotations",
						width = 12,
						status = "success",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("Sample Annotations (Column Annotations)"),
						p("Displayed as colored bars above the heatmap.  Each sample metadata column becomes a row of colored bars:"),
						tags$ul(
							tags$li(strong("Sample_Group:"), "e.g., Clinical, Pooled Normal, Control"),
							tags$li(strong("Disease_State:"), "e.g., Cancer, Healthy, Pre-disease"),
							tags$li(strong("Treatment:"), "e.g., Drug A, Drug B, Placebo")
						),
						
						h4("Feature Annotations (Row Annotations)"),
						p("Displayed as colored bars to the left of the heatmap. Each feature metadata column becomes a column of colored bars:"),
						tags$ul(
							tags$li(strong("Protein_Family:"), "e.g., Kinase, Transcription Factor, Receptor"),
							tags$li(strong("Function:"), "e.g., Signaling, Metabolism, Cell Cycle"),
							tags$li(strong("Chromosome:"), "e.g., chr1, chr2, chrX")
						),
						
						p(strong("Colors are automatically assigned"), "using distinct palettes for easy visual separation.")
					)
				),
				
				fluidRow(
					box(
						title = "Debug Mode Usage",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("Using Debug Mode"),
						p("Debug mode allows you to pause execution and inspect data:"),
						
						h5("Available Debug Functions:"),
						tags$ul(
							tags$li(strong("🔍 Enter Debug Mode:"), "Pause and inspect all objects"),
							tags$li(strong("📊 Run Full Diagnostics:"), "Print detailed ExpSet structure"),
							tags$li(strong("⚡ Quick Check:"), "Validate data structure")
						),
						
						h5("Common Debug Commands:"),
						tags$pre(
							"# Check ExpressionSet structure\n",
							"str(eset_selected())\n",
							"Biobase::assayDataElementNames(eset_selected())\n",
							"\n",
							"# Check expression data\n",
							"expr <- Biobase::exprs(eset_selected())\n",
							"dim(expr)\n",
							"\n",
							"# Check metadata\n",
							"pdata <- Biobase::pData(eset_selected())\n",
							"colnames(pdata)\n",
							"\n",
							"# Check feature data\n",
							"fdata <- Biobase::fData(eset_selected())\n",
							"colnames(fdata)\n",
							"\n",
							"# Check filters\n",
							"sample_filter$filtered_indices()\n",
							"feature_filter$filtered_indices()\n",
							"\n",
							"# Exit debug mode\n",
							"c  # Continue\n",
							"Q  # Quit"
						)
					)
				)
			)
		)
	)
)