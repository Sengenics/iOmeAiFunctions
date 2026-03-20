# UI : Denoiser ####
ui <- dashboardPage(
	skin = "blue",
	
	dashboardHeader(
		title = "Denoiser - iOme AI",
		titleWidth = 300
	),
	
	## sidebar ####
	dashboardSidebar(
		width = 300,
		sidebarMenu(
			id = "sidebar",
			menuItem("ExpSet List Import", tabName = "data_select", icon = icon("database")),
			menuItem("Data Selection ", tabName = "data_select_2", icon = icon("database")),
			menuItem("pFC Analysis", tabName = "pfc", icon = icon("chart-line")),
			menuItem("Limma Analysis", tabName = "pn_limma", icon = icon("chart-bar")),
			menuItem("Denoiser", tabName = "denoise", icon = icon("filter")),
			
			#menuItem("PC Visualizer", tabName = "pc_viz", icon = icon("eye")),
			menuItem("Help", tabName = "help", icon = icon("question-circle")),
			actionButton('debug','Debug')
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
		
		## Main App ####
		tabItems(
			### Data selection tab ####
			tabItem(
				tabName = "data_select",
				
				# NEW: File Upload Box
				# fluidRow(
				# 	shinydashboard::box(
				# 		title = "Import New ExpSet Data",
				# 		width = 12,
				# 		status = "success",
				# 		solidHeader = TRUE,
				# 		class = "upload-box",
				# 		collapsible = TRUE,
				# 		collapsed = FALSE,
				# 		
				# 		p(icon("upload"), strong("Upload a new ExpSet.rds file"), "to import custom expression data"),
				# 		
				# 		fluidRow(
				# 			column(
				# 				width = 4,
				# 				fileInput(
				# 					"expset_file",
				# 					"Choose ExpSet.rds File",
				# 					accept = c(".rds", ".RDS"),
				# 					placeholder = "No file selected",
				# 					buttonLabel = "Browse...",
				# 					width = "100%"
				# 				)
				# 			),
				# 			column(
				# 				width = 3,
				# 				br(),
				# 				actionButton(
				# 					"load_expset",
				# 					"Load ExpSet File",
				# 					icon = icon("file-upload"),
				# 					class = "btn-success btn-lg",
				# 					style = "margin-top: 5px;"
				# 				)
				# 			),
				# 			column(
				# 				width = 5,
				# 				br(),
				# 				uiOutput("expset_status_ui")
				# 			)
				# 		),
				# 		
				# 		hr(),
				# 		
				# 		p(icon("info-circle"), strong("File Requirements:")),
				# 		tags$ul(
				# 			tags$li("File must be in .rds or .RDS format"),
				# 			tags$li("File should contain an ExpressionSet object or a named list of ExpressionSets"),
				# 			tags$li("ExpressionSets must have expression data accessible via Biobase::exprs()"),
				# 			tags$li("Sample metadata should be available via Biobase::pData()")
				# 		)
				# 	)
				# ),
				
				fluidRow(
					box(
						title = "Upload ExpSet_list.rds",
						width = 12,
						status = "success",
						solidHeader = TRUE,
						mod_expset_import_ui("expset_import", debug = run_debug, dataset_choices = dev_datasets)
					)
				),
				
				mod_expset_viewer_ui("expset_viewer",debug = TRUE),
				mod_expset_metadata_manager_ui("metadata_mgr"),
	
				
				
			),
				
			tabItem(
				tabName = "data_select_2",
				fluidRow(
					shinydashboard::box(
						title = "ExpressionSet Data Selection",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						
						p("Select the expression data to use for denoising. The app will auto-load from data/ExpSet_list.rds or use your uploaded file."),
						
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "Raw/NetI Data",
									width = NULL,
									status = "info",
									solidHeader = TRUE,
									
									p("Select the raw or NetI data for denoising:"),
									#mod_eset_selector_ui("eset_raw"),
									mod_eset_selector_standalone_ui(
										"eset_raw",
										show_summary = TRUE,
										show_subset = TRUE,      # ✅ Enable subsetting
										show_transform = FALSE,  # Probably don't need transform for raw data
										show_info = TRUE,
										debug = run_debug
									),
									
									
									hr(),
									
									p(strong("Selected Data Info:")),
									verbatimTextOutput("raw_info")
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Normalized Data (Optional)",
									width = NULL,
									status = "warning",
									solidHeader = TRUE,
									
									p("Select normalized data for background visualization:"),
									#mod_eset_selector_ui("eset_norm"),
									mod_eset_selector_standalone_ui(
										"eset_norm",
										show_summary = TRUE,
										show_subset = TRUE,      # ✅ Enable subsetting
										show_transform = FALSE,
										show_info = TRUE,
										debug = run_debug
									),
									
									hr(),
									
									p(strong("Selected Data Info:")),
									verbatimTextOutput("norm_info")
								)
							),
							
							fluidRow(
								shinydashboard::box(
									title = "Primary Analysis Variable",
									width = 12,
									status = "success",
									solidHeader = TRUE,
									collapsible = TRUE,
									
									p("Select the primary metadata variable for downstream analyses (limma, pFC, etc.):"),
									
									fluidRow(
										column(
											width = 4,
											selectInput(
												"primary_variable",
												"Primary Variable:",
												choices = NULL
											),
											
											p(class = "text-muted", 
												"This variable will be auto-selected in analysis modules. You can change it there if needed.")
										),
										column(
											width = 8,
											h4("Variable Summary:"),
											tableOutput("variable_summary")
										)
									)
								)
							)
						
							
						)
					)
				
				)
			),
				
			
			### pFC Analysis tab ####
			tabItem(
				tabName = "pfc",
				
				column(
					width = 6,
					shinydashboard::box(
						title = "Normalized Data (Optional)",
						width = NULL,
						status = "warning",
						solidHeader = TRUE,
						
						p("Select normalized data for background visualization:"),
						#mod_eset_selector_ui("eset_norm"),
						mod_eset_selector_standalone_ui(
							"eset_pFC",
							show_summary = TRUE,
							show_subset = TRUE,      # ✅ Enable subsetting
							show_transform = FALSE,
							show_info = TRUE,
							debug = run_debug
						),
						
						hr(),
						
						p(strong("Selected Data Info:")),
						verbatimTextOutput("norm_info")
					)
				),
				column(12,
				fluidRow(
					tabsetPanel(selected = 'pFC v2',
						tabPanel('pFC',
		
							test_debug_module_UI('test_main'),
							p("Penetrance Fold Change (pFC) analysis identifies features enriched in a case group compared to controls."),
							p("This analysis uses normalized data. Select your groups below."),
							
							hr(),
							
							pFC_UI("pfc_module"),
							fluidRow(
								box(
									title = "Help: pFC Analysis",
									width = 12,
									status = "info",
									solidHeader = TRUE,
									collapsible = TRUE,
									collapsed = TRUE,
									
									h4("What is pFC?"),
									p("Penetrance Fold Change (pFC) analysis calculates, per feature:"),
									tags$ul(
										tags$li("The percentage of case samples with FC > threshold vs. control mean"),
										tags$li("Fisher's exact test for enrichment in cases"),
										tags$li("Effect size and penetrance metrics")
									),
									
									h4("Input Requirements:"),
									tags$ul(
										tags$li(strong("Normalized data:"), "Uses your selected normalized ExpressionSet"),
										tags$li(strong("Variable:"), "The metadata column containing case/control labels"),
										tags$li(strong("Groups:"), "Select which groups to compare")
									),
									
									h4("Output:"),
									tags$ul(
										tags$li(strong("Summary:"), "Overall statistics and hit counts"),
										tags$li(strong("Significant Results:"), "Features passing p-value threshold"),
										tags$li(strong("Violin Plots:"), "Distribution of significant features across groups"),
										tags$li(strong("Heatmaps:"), "Visual representation of hits"),
										tags$li(strong("Downloads:"), "Export all results as CSV/PDF/ZIP")
									),
									
									h4("Typical Workflow:"),
									tags$ol(
										tags$li("Select normalized data in 'Data Selection' tab"),
										tags$li("Come to this tab"),
										tags$li("Select variable (e.g., 'Sample_Group')"),
										tags$li("Choose case group (e.g., 'Cancer')"),
										tags$li("Choose control group (e.g., 'Healthy')"),
										tags$li("Adjust FC threshold (default: 2)"),
										tags$li("Adjust p-value threshold (default: 0.2)"),
										tags$li("Click 'Run pFC Analysis'"),
										tags$li("Explore results in tabs")
									)
								)
							)
							
						),
						tabPanel('pFC v2',
										 pFC_v2_UI("pfc_v2",TRUE))
				)
				)
				)
				
			
			),	
	
			
			
			### PN LIMMA #####
			tabItem(
				tabName = "pn_limma",
				mod_pn_limma_ui("pn_limma")
			),
			
			# tabItem(
			# 	tabName = "pn_limma",
			# 	mod_pn_limma_ui("pn_limma"),
			# 	fluidRow(
			# 		box(
			# 			title = "Pooled Normal AAb Estimation",
			# 			width = 12,
			# 			status = "primary",
			# 			solidHeader = TRUE,
			# 
			# 			p("Estimate the expected number of autoantibodies (AAbs) in Pooled Normal samples using limma differential expression analysis."),
			# 
			# 			fluidRow(
			# 				column(
			# 					width = 4,
			# 					selectInput(
			# 						"limma_method",
			# 						"Analysis Method",
			# 						choices = c(
			# 							"Standard (PN vs Clinical)" = "standard",
			# 							"PSA Covariate (PN vs Clinical with PSA)" = "psa_cov"
			# 						),
			# 						selected = "psa_cov"
			# 					)
			# 				),
			# 				column(
			# 					width = 4,
			# 					numericInput(
			# 						"limma_fc_cutoff",
			# 						"Fold Change Cutoff",
			# 						value = 1.75,
			# 						min = 1.0,
			# 						max = 3.0,
			# 						step = 0.25
			# 					)
			# 				),
			# 				column(
			# 					width = 4,
			# 					br(),
			# 					actionButton(
			# 						"run_limma",
			# 						"Run Limma Analysis",
			# 						icon = icon("play"),
			# 						class = "btn-primary btn-lg",
			# 						style = "margin-top: 5px;"
			# 					)
			# 				)
			# 			),
			# 
			# 			hr(),
			# 
			# 			conditionalPanel(
			# 				condition = "output.limma_complete",
			# 
			# 				h4("Expected PN AAbs"),
			# 				verbatimTextOutput("limma_summary"),
			# 
			# 				hr(),
			# 
			# 				fluidRow(
			# 					column(
			# 						width = 6,
			# 						h4("Top Differential Antigens"),
			# 						DT::dataTableOutput("limma_top_table")
			# 					),
			# 					column(
			# 						width = 6,
			# 						h4("Volcano Plot"),
			# 						plotOutput("limma_volcano", height = "400px")
			# 					)
			# 				),
			# 
			# 				hr(),
			# 
			# 				downloadButton("download_limma", "Download Full Results", class = "btn-success")
			# 			)
			# 		)
			# 	),
			# 
			# 	fluidRow(
			# 		box(
			# 			title = "Help: PN Limma Analysis",
			# 			width = 12,
			# 			status = "info",
			# 			solidHeader = TRUE,
			# 			collapsible = TRUE,
			# 			collapsed = TRUE,
			# 
			# 			h4("What is this analysis?"),
			# 			p("This step estimates how many autoantibodies we expect to find in Pooled Normal (PN) samples. This is critical for optimizing the denoising parameters."),
			# 
			# 			h4("Two Methods:"),
			# 			tags$ul(
			# 				tags$li(strong("Standard:"), "Compares PN samples to clinical samples using a simple contrast: PN vs (Clinical groups)."),
			# 				tags$li(strong("PSA Covariate:"), "Same comparison but includes PSA score as a continuous covariate to account for PSA contamination.")
			# 			),
			# 
			# 			h4("Output:"),
			# 			tags$ul(
			# 				tags$li(strong("Expected PN AAb Count:"), "The range of AAbs expected (e.g., 6-12), calculated based on the FC cutoff."),
			# 				tags$li(strong("Specific PN AAbs:"), "The list of antigens that pass the FC threshold (e.g., PSIP1, MAPK9, MX1)."),
			# 				tags$li("These values are used by the", strong("Denoiser tab"), "to optimize cutpoint selection.")
			# 			),
			# 
			# 			h4("Usage:"),
			# 			p("Run this analysis", strong("before"), "running the denoiser. The results will automatically populate the", strong("Expected PN AAbs"), "field in the Denoiser tab.")
			# 		)
			# 	)
			# ),

			### Denoiser tab ####
			tabItem(
				tabName = "denoise",
				mod_denoiser_ui("denoiser")
			),
			
			### PC Vis ####
			# tabItem(
			# 	tabName = "pc_viz",
			# 	mod_pc_visualizer_ui("pc_viz")
			# ),
			
			# Help tab
			tabItem(
				tabName = "help",
				
				fluidRow(
					shinydashboard::box(
						title = "Denoiser Help",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						
						h3("Quick Start Guide"),
						
						h4("Recommended Workflow"),
						tags$ol(
							tags$li(strong("Data Selection:"), "Upload or select your ExpressionSet data"),
							tags$li(strong("PN Limma Analysis:"), "Run limma analysis to estimate expected PN AAbs"),
							tags$li(strong("Denoiser:"), "Configure parameters (auto-populated from limma) and run denoising"),
							tags$li(strong("Results:"), "Explore visualizations and export results")
						),
						
						h4("1. Import Data (Optional)"),
						p("You can either:"),
						tags$ul(
							tags$li(strong("Use Default Data:"), "The app will auto-load from data/ExpSet_list.rds"),
							tags$li(strong("Upload New Data:"), "Click 'Browse...' to select your own ExpSet.rds file, then click 'Load ExpSet File'")
						),
						
						hr(),
						
						h4("2. Data Selection"),
						p("Select your expression data from the dropdowns. You need:"),
						tags$ul(
							tags$li(strong("Raw/NetI Data:"), "The input data for PC removal and denoising"),
							tags$li(strong("Normalized Data:"), "(Optional) For background visualization in AAb borders plots")
						),
						
						hr(),
						
						h4("3. Configure Parameters"),
						p("In the Denoiser tab, set:"),
						tags$ul(
							tags$li(strong("Number of PCs:"), "How many principal components to remove (typically 1-7)"),
							tags$li(strong("Cutpoint Range:"), "Range of thresholds to test"),
							tags$li(strong("Expected PN AAbs:"), "List of antigens expected to be elevated in Pooled Normals"),
							tags$li(strong("Expected Count:"), "Range of AAb count expected in PNs (from limma analysis)")
						),
						
						hr(),
						
						h4("4. Run Denoising"),
						p("Click", strong("Run Denoising"), "to start the analysis. This will:"),
						tags$ol(
							tags$li("Remove principal components from the data"),
							tags$li("Test multiple cutpoints"),
							tags$li("Calculate quality metrics"),
							tags$li("Select optimal parameters"),
							tags$li("Generate AAb-called data")
						),
						
						hr(),
						
						h4("5. Explore Results"),
						p("Use the tabs to:"),
						tags$ul(
							tags$li(strong("PCA & Denoising:"), "View variance explained and denoised data"),
							tags$li(strong("Cutpoint Analysis:"), "Compare different cutpoint thresholds"),
							tags$li(strong("AAb-Called Data:"), "View final AAb calls and adjust cutpoints"),
							tags$li(strong("Visualization:"), "AAb borders and t-SNE plots"),
							tags$li(strong("Summary:"), "Statistics and export template")
						),
						
						hr(),
						
						h4("6. Export Results"),
						p("Download results as a ZIP file or generate an AAb_caller_template.R script for pipeline use.")
					)
				),
				
				fluidRow(
					shinydashboard::box(
						title = "Key Metrics Explained",
						width = 12,
						status = "warning",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("TP:FP Ratio"),
						p("True Positive to False Positive ratio. Higher is better. Measures how many legitimate PN AAbs are detected vs. noise."),
						
						h4("PN AAb Count"),
						p("Number of antigens detected in ≥67% of Pooled Normal samples. Should match expected range from limma."),
						
						h4("PN Hit Rate"),
						p("Percentage of expected PN AAbs (from limma) that are successfully detected by the denoiser."),
						
						h4("ZZ Control Rates"),
						p("Fraction of samples positive for ZZ_con2/4 controls. Should approach 0% with optimal cutpoint."),
						
						h4("PSA+ Rate"),
						p("Percentage of samples typed as PSA-positive. Should be ≤5% for good quality denoising.")
					)
				),
				
				fluidRow(
					shinydashboard::box(
						title = "Debug Mode Usage",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						h4("Using Debug Mode"),
						p("Debug mode allows you to pause execution and inspect data at any point:"),
						
						h5("In Data Selection Tab:"),
						tags$ul(
							tags$li("Click", strong("🔍 Enter Debug Mode"), "to inspect loaded ExpressionSets"),
							tags$li("Use", code("quick_inspect_eset(eset_raw())"), "to see data structure"),
							tags$li("Use", code("diagnose_ExpSet_list(ExpSet_list())"), "for full diagnostics")
						),
						
						h5("In Denoiser Tab:"),
						tags$ul(
							tags$li("Click", strong("🔍 Enter Debug Mode"), "in the controls panel"),
							tags$li("Debug mode will show context-aware information based on pipeline stage"),
							tags$li("Inspect reactive values with", code("rv$denoise_results"), "etc.")
						),
						
						h5("Common Debug Commands:"),
						tags$pre(
							"# Check ExpressionSet structure\n",
							"str(eset_raw())\n",
							"Biobase::assayDataElementNames(eset_raw())\n",
							"\n",
							"# Check expression data\n",
							"expr <- Biobase::exprs(eset_raw())\n",
							"class(expr)\n",
							"dim(expr)\n",
							"\n",
							"# Check metadata\n",
							"meta <- Biobase::pData(eset_raw())\n",
							"colnames(meta)\n",
							"table(meta$Sample_Group)\n",
							"\n",
							"# Exit debug mode\n",
							"c  # Continue execution\n",
							"Q  # Quit browser"
						)
					)
				)
			)
		)
	)
)
