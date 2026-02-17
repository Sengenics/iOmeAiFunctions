#' Denoiser Module UI
#'
#' @param id Character; module namespace ID
#'
#' @export
mod_denoiser_ui <- function(id) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			# Left panel - Controls ####
			
			column(
				width = 3,
				shinydashboard::box(
					title = "Denoiser Controls",
					width = NULL,
					status = "primary",
					solidHeader = TRUE,
					actionButton(ns('denoise_mod_debug'),'Mod Debug'),
					actionButton(
						ns("run_denoise"),
						"Run Denoising",
						icon = icon("play"),
						class = "btn-success btn-lg btn-block"
					),
					actionButton(
						ns("denoise_update_ExpSet"),
						"Update ExpSet list",
						icon = icon("plus-circle"),
						class = "btn-success btn-lg btn-block"
					),
					downloadButton(
						ns("download_expset"),
						"Download ExpressionSet",
						class = "btn-primary btn-block"
					),
					
					br(),
					
					# Data selection
					h4("Data Selection"),
					selectInput(
						ns("assay_name"),
						"Expression Data Assay:",
						choices = c("NetI" = "NetI", "exprs" = "exprs"),
						selected = "exprs"
					),
					
					selectInput(
						ns("norm_assay_name"),
						"Normalized Data Assay (for borders):",
						choices = c("loess_combat" = "loess_combat", 
												"median_combat" = "median_combat"),
						selected = "loess_combat"
					),
					
					hr(),
					
					# PC removal
					h4("PC Removal"),
					sliderInput(
						ns("n_PCs"),
						"Number of PCs to Remove:",
						min = 1,
						max = 10,
						value = 3,
						step = 1
					),
					
					checkboxInput(
						ns("scale_pca"),
						"Scale data for PCA",
						value = TRUE
					),
					checkboxInput(
						ns("center_pca"),
						"Center data for PCA",
						value = TRUE
					),
					radioButtons(
						ns("transpose_pca"),
						"Transpose",
						c('Sample vs Featrures' = T,
							'Features vs Samples' = F),
						F
					),
					
					
					hr(),
					
					# Cutpoint settings
					h4("Cutpoint Settings"),
					selectInput(
						ns("method"),
						"Cutting Method:",
						choices = c(
							"Singular (global threshold)" = "singular",
							"MAD (per-antigen)" = "MAD",
							"AAD (per-antigen)" = "AAD"
						),
						selected = "singular"
					),
					
					sliderInput(
						ns("cut_min"),
						"Cutpoint Range - Min:",
						min = 0,
						max = 5,
						value = 0.4,
						step = 0.1
					),
					
					sliderInput(
						ns("cut_max"),
						"Cutpoint Range - Max:",
						min = 0,
						max = 5,
						value = 3,
						step = 0.1
					),
					
					sliderInput(
						ns("cut_step"),
						"Cutpoint Step:",
						min = 0.05,
						max = 0.5,
						value = 0.1,
						step = 0.05
					),
					
					hr(),
					
					# PN identification
					h4("Pooled Normal Samples"),
					selectInput(
						ns("PN_column"),
						"PN Identifier Column:",
						choices = NULL  # Will be populated from eset
					),
					
					textInput(
						ns("PN_value"),
						"PN Identifier Value:",
						value = "Pooled Normal"
					),
					
					hr(),
					
					# Expected PN AAbs
					h4("Expected PN AAbs"),
					textAreaInput(
						ns("PN_AAbs"),
						"Expected PN AAbs (comma-separated):",
						value = "PSIP1, MAPK9, MX1, UBE2I, PTPN11, MLH1",
						rows = 3
					),
					
					numericInput(
						ns("exp_PN_min"),
						"Expected PN AAb Count - Min:",
						value = 6,
						min = 0,
						step = 1
					),
					
					numericInput(
						ns("exp_PN_max"),
						"Expected PN AAb Count - Max:",
						value = 12,
						min = 0,
						step = 1
					),
					
					hr(),
					
					# Annotation columns
					h4("Plot Annotations"),
					selectizeInput(
						ns("annotation_cols"),
						"Annotation Columns:",
						choices = NULL,  # Will be populated from eset
						multiple = TRUE,
						options = list(
							placeholder = 'Select annotation columns...',
							maxItems = 5
						)
					),
					
					hr(),
					
					# Action buttons
		
					
					downloadButton(
						ns("download_results"),
						"Download Results",
						class = "btn-primary btn-block"
					)
				)
			),
			
			# Right panel - Tabs #####
			column(
				width = 9,
				tabsetPanel(
					id = ns("main_tabs"),
					type = "tabs",
					
					
					# Add this at the top of the "Component Matrices" tab in mod_denoiser_ui:
					
					tabPanel(
						"Denoiser Overview",
						br(),
						
						## Description Section ####
						div(
							style = "background-color: #f8f9fa; padding: 20px; border-left: 4px solid #007bff; margin-bottom: 20px;",
							
							h3(icon("info-circle"), " Understanding PCA Denoising"),
							
							p(strong("What is PCA doing?")),
							p("Principal Component Analysis (PCA) decomposes your autoantibody data into independent components, 
		  each capturing different patterns of variation. The goal is to separate technical noise from biological signal."),
							
							hr(),
							
							div(
								style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
								
								# Left column
								div(
									h5(icon("flask"), strong(" The Problem")),
									tags$ul(
										tags$li(strong("Biological signal:"), " Real disease differences between patients"),
										tags$li(strong("Technical noise:"), " Batch effects, assay dates, plate effects"),
										tags$li("Both types of variation are mixed together in your raw data")
									)
								),
								
								# Right column
								div(
									h5(icon("magic"), strong(" The Solution")),
									tags$ul(
										tags$li(strong("Early PCs (PC1, PC2):"), " Usually capture large, systematic technical noise"),
										tags$li(strong("Later PCs (PC5+):"), " Usually capture subtle biological patterns"),
										tags$li(strong("Remove early PCs:"), " Eliminate technical noise while preserving biology")
									)
								)
							),
							
							hr(),
							
							h5(icon("chart-line"), strong(" How to Interpret Components:")),
							
							div(
								style = "background-color: white; padding: 15px; border-radius: 5px; margin-top: 10px;",
								
								div(
									style = "margin-bottom: 10px;",
									span(style = "background-color: #dc3545; color: white; padding: 3px 8px; border-radius: 3px; font-weight: bold;", "PC1"),
									span(style = "margin-left: 10px;", "Typically 30-40% variance | Often batch effects"),
									tags$ul(
										tags$li(strong("Loadings show:"), " Which antibodies are affected by batch"),
										tags$li(strong("If you see:"), " ZZ controls, random antibodies → This is technical noise"),
										tags$li(strong("Action:"), " ❌ Remove this component")
									)
								),
								
								div(
									style = "margin-bottom: 10px;",
									span(style = "background-color: #fd7e14; color: white; padding: 3px 8px; border-radius: 3px; font-weight: bold;", "PC2"),
									span(style = "margin-left: 10px;", "Typically 10-15% variance | Often assay date effects"),
									tags$ul(
										tags$li(strong("Loadings show:"), " Which antibodies are affected by time/storage"),
										tags$li(strong("Action:"), " ❌ Usually remove this component")
									)
								),
								
								div(
									style = "margin-bottom: 10px;",
									span(style = "background-color: #ffc107; color: black; padding: 3px 8px; border-radius: 3px; font-weight: bold;", "PC3-4"),
									span(style = "margin-left: 10px;", "Typically 5-10% variance | Transition zone"),
									tags$ul(
										tags$li(strong("Check:"), " Does it correlate with Sample_Group or Batch_ID?"),
										tags$li(strong("Action:"), " ⚠️ Test with and without removal")
									)
								),
								
								div(
									span(style = "background-color: #28a745; color: white; padding: 3px 8px; border-radius: 3px; font-weight: bold;", "PC5+"),
									span(style = "margin-left: 10px;", "Typically <5% variance | Biological signal"),
									tags$ul(
										tags$li(strong("Loadings show:"), " Known disease markers, patient subtypes"),
										tags$li(strong("Action:"), " ✅ Keep these components")
									)
								)
							),
							
							hr(),
							
							h5(icon("lightbulb"), strong(" What You'll See in the Tabs Below:")),
							
							div(
								style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-top: 10px;",
								
								div(
									style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px;",
									h6(strong("📊 Set 1: Component Loadings")),
									p("Each tab shows which antibodies contribute most to that component:"),
									tags$ul(
										tags$li(strong("Positive loadings:"), " Antibodies that increase with the component"),
										tags$li(strong("Negative loadings:"), " Antibodies that decrease with the component"),
										tags$li(strong("Large absolute values:"), " Most affected by this pattern")
									),
									p(style = "margin-bottom: 0;", em("Use this to understand what each PC captures."))
								),
								
								div(
									style = "background-color: #f3e5f5; padding: 15px; border-radius: 5px;",
									h6(strong("📈 Set 2: Reconstructed Data")),
									p("Each tab shows expression values after removing PCs:"),
									tags$ul(
										tags$li(strong("1 PC removed:"), " Data with PC1 subtracted out"),
										tags$li(strong("2 PCs removed:"), " Data with PC1+PC2 subtracted"),
										tags$li(strong("Compare tabs:"), " See how removal affects your data")
									),
									p(style = "margin-bottom: 0;", em("Use this to see the actual denoised values."))
								)
							),
							
							hr(),
							
							h5(icon("check-circle"), strong(" How to Decide How Many PCs to Remove:")),
							
							tags$ol(
								tags$li(strong("View PC1-2 loadings:"), " Check if ZZ controls and random antibodies are prominent → Technical noise"),
								tags$li(strong("Compare reconstructed data:"), " Look at distribution changes between removal levels"),
								tags$li(strong("Check metrics in other tabs:"), 
												tags$ul(
													tags$li("TP:FP ratio maximized?"),
													tags$li("ZZ control rate = 0%?"),
													tags$li("Expected PN antibodies still detected?")
												)
								),
								tags$li(strong("Select optimal level:"), " Usually 1-3 PCs removed achieves best balance")
							),
							
							div(
								style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 15px;",
								icon("exclamation-triangle"),
								strong(" Important:"),
								" Removing too many PCs can eliminate biological signal! Always validate that expected antibodies are still detected."
							)
						),
						
						hr(),
						
						# Add this section to your Component Matrices tab or create a new "Understanding PCA Components" section:
						
						div(
							style = "background-color: #f8f9fa; padding: 20px; border-left: 4px solid #17a2b8; margin-bottom: 20px;",
							
							h3(icon("cube"), " What's Actually in PCA Components?"),
							
							p("When you run PCA (", code("prcomp()"), "), you get three key pieces of information. 
	  Understanding these helps you interpret what each component represents and decide what to remove."),
							
							hr(),
							
							## Component 1: Scores ####
							div(
								style = "background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #007bff;",
								
								h4(
									icon("map-marker-alt"),
									strong(" 1. Scores (Sample Coordinates)"),
									tags$code(style = "margin-left: 10px; background-color: #e9ecef; padding: 3px 8px; border-radius: 3px;", 
														"pca_result$x")
								),
								
								fluidRow(
									column(4,
												 div(
												 	style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
												 	strong(icon("ruler-combined"), " Dimensions:"),
												 	br(),
												 	"Samples × Components",
												 	br(),
												 	tags$small(em("e.g., 150 samples × 150 PCs"))
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px;",
												 	strong(icon("question-circle"), " What it is:"),
												 	br(),
												 	"Where each ", strong("sample"), " sits along each PC axis"
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #fff3e0; padding: 10px; border-radius: 5px;",
												 	strong(icon("lightbulb"), " Use:"),
												 	br(),
												 	"See which samples are similar/different"
												 )
									)
								),
								
								div(
									style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
									strong("Example interpretation:"),
									tags$ul(
										tags$li(
											strong("High PC1 score (e.g., +12.45):"),
											" Sample is strongly affected by whatever PC1 captures (e.g., processed in Batch A)"
										),
										tags$li(
											strong("Low PC1 score (e.g., -10.23):"),
											" Sample affected in opposite direction (e.g., processed in Batch B)"
										),
										tags$li(
											strong("Near-zero score:"),
											" Sample not affected by this particular pattern"
										)
									)
								),
								
								div(
									style = "margin-top: 10px; padding: 8px; background-color: #d1ecf1; border-left: 3px solid #0c5460; border-radius: 3px;",
									icon("chart-scatter"),
									strong(" Visualize with:"),
									" PCA biplots, score heatmaps, violin plots by group"
								)
							),
							
							## Component 2: Loadings ####
							div(
								style = "background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #28a745;",
								
								h4(
									icon("weight-hanging"),
									strong(" 2. Loadings (Feature Contributions)"),
									tags$code(style = "margin-left: 10px; background-color: #e9ecef; padding: 3px 8px; border-radius: 3px;", 
														"pca_result$rotation")
								),
								
								fluidRow(
									column(4,
												 div(
												 	style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
												 	strong(icon("ruler-combined"), " Dimensions:"),
												 	br(),
												 	"Features × Components",
												 	br(),
												 	tags$small(em("e.g., 1000 antibodies × 150 PCs"))
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px;",
												 	strong(icon("question-circle"), " What it is:"),
												 	br(),
												 	"How much each ", strong("antibody"), " contributes to each PC"
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #fff3e0; padding: 10px; border-radius: 5px;",
												 	strong(icon("lightbulb"), " Use:"),
												 	br(),
												 	"Understand what pattern each PC captures"
												 )
									)
								),
								
								div(
									style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
									strong("Example interpretation:"),
									tags$ul(
										tags$li(
											strong("High positive loading (e.g., +0.245):"),
											" Antibody increases strongly when this PC increases"
										),
										tags$li(
											strong("High negative loading (e.g., -0.213):"),
											" Antibody decreases when this PC increases"
										),
										tags$li(
											strong("Near-zero loading (e.g., 0.012):"),
											" Antibody not related to this pattern"
										),
										tags$li(
											span(style = "color: #dc3545;", icon("exclamation-triangle"), strong(" Red flag:")),
											" If ZZ controls have high loadings → This PC captures technical noise!"
										)
									)
								),
								
								div(
									style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border-left: 3px solid #155724; border-radius: 3px;",
									icon("chart-bar"),
									strong(" Visualize with:"),
									" Loading bar plots, loading heatmaps, combined biplots"
								)
							),
							
							## Component 3: Standard Deviations ####
							div(
								style = "background-color: white; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #ffc107;",
								
								h4(
									icon("chart-line"),
									strong(" 3. Standard Deviations (Variance Explained)"),
									tags$code(style = "margin-left: 10px; background-color: #e9ecef; padding: 3px 8px; border-radius: 3px;", 
														"pca_result$sdev")
								),
								
								fluidRow(
									column(4,
												 div(
												 	style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
												 	strong(icon("ruler-combined"), " Dimensions:"),
												 	br(),
												 	"Vector",
												 	br(),
												 	tags$small(em("e.g., 150 values (one per PC)"))
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #e8f5e9; padding: 10px; border-radius: 5px;",
												 	strong(icon("question-circle"), " What it is:"),
												 	br(),
												 	"Magnitude of variation in each PC"
												 )
									),
									column(4,
												 div(
												 	style = "background-color: #fff3e0; padding: 10px; border-radius: 5px;",
												 	strong(icon("lightbulb"), " Use:"),
												 	br(),
												 	"Decide which PCs are important"
												 )
									)
								),
								
								div(
									style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
									strong("Example interpretation:"),
									tags$ul(
										tags$li(
											strong("PC1: 35% variance"),
											" → Captures the largest pattern in your data (usually technical noise)"
										),
										tags$li(
											strong("PC2: 12% variance"),
											" → Second largest pattern (more technical noise)"
										),
										tags$li(
											strong("PC5: 4% variance"),
											" → Smaller pattern (may be biological signal)"
										),
										tags$li(
											strong("PC20: 0.5% variance"),
											" → Very small pattern (likely noise/individual variation)"
										)
									)
								),
								
								div(
									style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border-left: 3px solid #856404; border-radius: 3px;",
									icon("chart-area"),
									strong(" Visualize with:"),
									" Scree plots, cumulative variance plots"
								)
							),
							
							hr(),
							
							## How They Work Together ####
							div(
								style = "background-color: #e0f7fa; padding: 15px; border-radius: 5px;",
								
								h4(icon("link"), strong(" How These Three Pieces Work Together")),
								
								p("Think of PCA like decomposing a song into different instruments:"),
								
								tags$ol(
									tags$li(
										strong("Loadings"), " tell you ", em("which instruments (antibodies) play in each part")
									),
									tags$li(
										strong("Scores"), " tell you ", em("how loud each part is in each sample")
									),
									tags$li(
										strong("Variance"), " tells you ", em("how important each part is overall")
									)
								),
								
								div(
									style = "margin-top: 15px; padding: 12px; background-color: white; border-radius: 5px;",
									strong(icon("flask"), " In your denoising workflow:"),
									tags$ul(
										tags$li("Check ", strong("loadings"), " → Are ZZ controls affected? → PC captures technical noise"),
										tags$li("Check ", strong("scores"), " → Do samples cluster by batch? → PC captures batch effects"),
										tags$li("Check ", strong("variance"), " → Is this PC important? → Decide if worth removing")
									)
								)
							),
							
							hr(),
							
							## Navigation Guide ####
							div(
								style = "background-color: #fff9e6; padding: 15px; border-radius: 5px;",
								
								h4(icon("compass"), strong(" How to Navigate the Tabs Below")),
								
								fluidRow(
									column(6,
												 div(
												 	style = "background-color: white; padding: 12px; border-radius: 5px; height: 100%;",
												 	h5(icon("layer-group"), strong(" Set 1: Component Loadings")),
												 	p("Each tab (PC1, PC2, PC3...) shows the ", strong("loadings"), " table:"),
												 	tags$ul(
												 		tags$li("Sorted by absolute value (most important features first)"),
												 		tags$li("Shows which antibodies contribute to this pattern"),
												 		tags$li("Look for ZZ controls, known markers, random antibodies")
												 	),
												 	div(
												 		style = "background-color: #e3f2fd; padding: 8px; border-radius: 3px; margin-top: 10px;",
												 		icon("search"),
												 		strong(" What to look for:"),
												 		br(),
												 		tags$small("High loadings on ZZ controls = technical noise")
												 	)
												 )
									),
									column(6,
												 div(
												 	style = "background-color: white; padding: 12px; border-radius: 5px; height: 100%;",
												 	h5(icon("table"), strong(" Set 2: Reconstructed Data")),
												 	p("Each tab (1 PC Removed, 2 PCs...) shows the ", strong("denoised values"), ":"),
												 	tags$ul(
												 		tags$li("Full expression matrix after removing PCs"),
												 		tags$li("Compare distributions across tabs"),
												 		tags$li("See how values change with more PCs removed")
												 	),
												 	div(
												 		style = "background-color: #f3e5f5; padding: 8px; border-radius: 3px; margin-top: 10px;",
												 		icon("exchange-alt"),
												 		strong(" What to compare:"),
												 		br(),
												 		tags$small("Check if distributions become cleaner/more normalized")
												 	)
												 )
									)
								)
							)
						),
						# Collapsible version - less intrusive
						div(
							style = "margin: 20px 0;",
							
							tags$button(
								class = "btn btn-warning btn-block",
								`data-toggle` = "collapse",
								`data-target` = "#pca-implementation-note",
								style = "text-align: left;",
								icon("exclamation-triangle"),
								strong(" Technical Note: PCA Implementation Details"),
								tags$span(
									style = "float: right;",
									icon("chevron-down")
								)
							),
							
							div(
								id = "pca-implementation-note",
								class = "collapse",
								style = "background-color: #fff3cd; padding: 20px; border: 1px solid #ffc107; border-top: none;",
								
								
									
									h4(
										icon("exclamation-triangle", style = "color: #856404;"),
										strong(" Important Note: PCA Implementation Details")
									),
									
									hr(style = "border-color: #ffc107;"),
									
									h5(icon("cogs"), strong(" Current Implementation")),
									
									p("In this denoising workflow, PCA is performed on the ", 
										strong("Features × Samples"), " matrix (without transposing). This means:"),
									
									tags$ul(
										tags$li(
											strong("Input orientation:"), 
											" Rows = Antigens (features), Columns = Samples"
										),
										tags$li(
											strong("PCA operates on:"), 
											" Feature space (identifies patterns across antigens)"
										),
										tags$li(
											code("pca_result$x"), " contains: ", 
											strong("Feature scores"), " (Features × PCs)"
										),
										tags$li(
											code("pca_result$rotation"), " contains: ", 
											strong("Sample loadings"), " (Samples × PCs)"
										)
									),
									
									div(
										style = "background-color: white; padding: 15px; border-left: 4px solid #ffc107; margin: 15px 0;",
										
										h5(icon("balance-scale"), strong(" Comparison with Standard Approach")),
										
										fluidRow(
											column(6,
														 div(
														 	style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
														 	h6(strong("Standard PCA Denoising")),
														 	p(style = "font-size: 0.9em; margin-bottom: 5px;", 
														 		"Used in: SVA, RUV, PEER, ComBat"),
														 	tags$ul(
														 		style = "font-size: 0.85em;",
														 		tags$li("Transpose: ", strong("Samples × Features")),
														 		tags$li("PC1 captures: ", em("Sample-level variation")),
														 		tags$li("Example: Batch A vs Batch B samples"),
														 		tags$li("Removing PC1: Corrects batch effects across all features")
														 	)
														 )
											),
											column(6,
														 div(
														 	style = "background-color: #fff3e0; padding: 10px; border-radius: 5px;",
														 	h6(strong("Current Implementation")),
														 	p(style = "font-size: 0.9em; margin-bottom: 5px;", 
														 		"Used in: This workflow"),
														 	tags$ul(
														 		style = "font-size: 0.85em;",
														 		tags$li("No transpose: ", strong("Features × Samples")),
														 		tags$li("PC1 captures: ", em("Feature co-variation patterns")),
														 		tags$li("Example: Antibodies that correlate together"),
														 		tags$li("Removing PC1: Removes systematic co-variation patterns")
														 	)
														 )
											)
										)
									),
									
									h5(icon("question-circle"), strong(" What Does This Mean?")),
									
									p("The choice of orientation determines ", strong("what type of variation"), 
										" the principal components capture:"),
									
									div(
										style = "background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;",
										
										strong("Standard approach (transposed):"),
										tags$ul(
											tags$li("PC1 typically captures: ", 
															tags$span(style = "color: #dc3545;", "Batch effects, assay date, plate effects")),
											tags$li("Removing PC1: Directly addresses sample-level technical variation"),
											tags$li("Interpretation: \"Samples from Batch A vs Batch B\"")
										),
										
										hr(style = "margin: 10px 0;"),
										
										strong("Current approach (not transposed):"),
										tags$ul(
											tags$li("PC1 captures: ", 
															tags$span(style = "color: #17a2b8;", "Systematic patterns of antibody co-expression")),
											tags$li("Removing PC1: Eliminates dominant feature correlation structure"),
											tags$li("Interpretation: \"Groups of antibodies that behave similarly\""),
											tags$li(
												style = "color: #856404;",
												icon("info-circle"),
												" Note: May ", em("indirectly"), " reduce batch effects if they create feature co-variation patterns"
											)
										)
									),
									
									h5(icon("check-square"), strong(" Validation Approach")),
									
									p("The effectiveness of this denoising approach is validated through multiple quality metrics:"),
									
									div(
										style = "background-color: #d4edda; padding: 12px; border-radius: 5px; border-left: 4px solid #28a745;",
										tags$ol(
											tags$li(
												strong("ZZ Control Rate:"), 
												" Measures if empty wells (technical noise) are eliminated (target: 0%)"
											),
											tags$li(
												strong("PN AAb Detection:"), 
												" Ensures expected biological antibodies in Pooled Normals are preserved"
											),
											tags$li(
												strong("TP:FP Ratio:"), 
												" Quantifies signal-to-noise improvement"
											),
											tags$li(
												strong("Downstream Cutpoint Optimization:"), 
												" Further refines the denoised data through empirical thresholding"
											)
										),
										
										p(
											style = "margin-top: 10px; margin-bottom: 5px;",
											icon("thumbs-up"),
											strong(" Key point:"), 
											" If these validation metrics pass (ZZ = 0%, PN AAbs detected, high TP:FP), ",
											"the denoising is effective regardless of the PCA orientation."
										)
									),
									
									h5(icon("flask"), strong(" How to Verify What PC1 Captures")),
									
									p("To understand what variation PC1 is removing in your specific dataset:"),
									
									div(
										style = "background-color: #e3f2fd; padding: 12px; border-radius: 5px;",
										
										tags$ol(
											tags$li(
												strong("Check sample loadings by batch:"),
												br(),
												tags$code("In the visualizations below, look at the PCA biplot colored by Batch_ID"),
												br(),
												tags$small("If batches separate along PC1 → Capturing batch effects")
											),
											tags$li(
												strong("Check sample loadings by sample group:"),
												br(),
												tags$code("Color the PCA biplot by Sample_Group (disease vs control)"),
												br(),
												tags$small("If disease groups separate → May be removing biological signal! (⚠️ Check carefully)")
											),
											tags$li(
												strong("Check feature loadings:"),
												br(),
												tags$code("View the 'Component Loadings' tabs below"),
												br(),
												tags$small("Look for patterns: Are ZZ controls high? Are known disease markers high?")
											),
											tags$li(
												strong("Compare before/after:"),
												br(),
												tags$code("Use PERMANOVA or batch effect metrics on raw vs denoised data"),
												br(),
												tags$small("Quantify reduction in batch variance")
											)
										)
									),
									
									hr(style = "border-color: #ffc107;"),
									
									div(
										style = "background-color: #d1ecf1; padding: 12px; border-radius: 5px; border-left: 4px solid #0c5460;",
										
										h5(icon("lightbulb"), strong(" Interpretation Guide for Your Data")),
										
										p(strong("When viewing the visualizations below:")),
										
										fluidRow(
											column(6,
														 div(
														 	style = "background-color: white; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
														 	h6(icon("map-marked-alt"), strong(" Sample Positions (Biplot)")),
														 	p(style = "font-size: 0.9em;", 
														 		"Shows where samples sit in PC space. Use ", code("pca_result$rotation"), "."),
														 	tags$ul(
														 		style = "font-size: 0.85em;",
														 		tags$li("Color by Batch_ID: See if batches cluster"),
														 		tags$li("Color by Sample_Group: Check for biological patterns"),
														 		tags$li("If PC1 separates batches → Removing PC1 reduces batch effects")
														 	)
														 )
											),
											column(6,
														 div(
														 	style = "background-color: white; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
														 	h6(icon("weight-hanging"), strong(" Feature Loadings (Bar Plot)")),
														 	p(style = "font-size: 0.9em;", 
														 		"Shows which antibodies contribute to each PC. Use ", code("pca_result$x"), "."),
														 	tags$ul(
														 		style = "font-size: 0.85em;",
														 		tags$li("High loadings on ZZ controls → PC captures technical noise ✓"),
														 		tags$li("High loadings on known markers → PC may capture biology ⚠️"),
														 		tags$li("Random antibodies → Likely noise or co-expression ✓")
														 	)
														 )
											)
										)
									),
									
									div(
										style = "background-color: #f8d7da; padding: 12px; border-radius: 5px; border-left: 4px solid #dc3545; margin-top: 15px;",
										icon("exclamation-circle"),
										strong(" Caution:"),
										" If you observe that PC1 strongly separates disease groups (e.g., SLE vs Healthy), ",
										"removing PC1 may eliminate biological signal. Always validate with the metrics in the ",
										strong("Cutpoint Analysis"), " tab (PN AAb hit rate, expected markers preserved)."
									),
									
									hr(style = "border-color: #ffc107;"),
									
									div(
										style = "text-align: center; padding: 10px;",
										tags$small(
											style = "color: #6c757d;",
											icon("book"),
											" For questions about the PCA orientation or to compare with standard approaches, ",
											"consult with the bioinformatics team or see references: Leek (SVA), Gagnon-Bartsch (RUV), Stegle (PEER)."
										)
									)
								)
						)
					
						
						
					),
					
					# Add to your "Component Matrices" or new "PCA Visualization" tab:
					## PCA Visualisation ####
				
					tabPanel(
						"PCA Visualization",
						br(),
						
						# In mod_denoiser_ui, in the PCA Visualization tab:
						
						fluidRow(
							column(3,
										 selectInput(
										 	ns("biplot_pc_x"),
										 	"X-axis PC:",
										 	choices = NULL,  # Will be populated by observer
										 	selected = NULL
										 )
							),
							column(3,
										 selectInput(
										 	ns("biplot_pc_y"),
										 	"Y-axis PC:",
										 	choices = NULL,  # Will be populated by observer
										 	selected = NULL
										 )
							)
						),
						
						tabsetPanel(
							
							tabPanel('Random',
											 uiOutput(ns('random_pca_plots'))
							),
							tabPanel("Feature Scores",
											 column(3,
											 			 selectInput(
											 			 	ns("features_pca_color_by"),
											 			 	"Color by:",
											 			 	choices = NULL,  # Will be populated by observer
											 			 	selected = NULL
											 			 )
											 ),
											 column(3,
											 			 selectInput(
											 			 	ns("features_pca_shape_by"),
											 			 	"Shape by:",
											 			 	choices = NULL,  # Will be populated by observer
											 			 	selected = NULL
											 			 )
											 ),
											 column(12,
												 plotOutput(ns("pca_biplot"), height = "600px"),
												 hr(),
												 plotOutput(ns("pca_feature_cont"), height = "600px"),
												 hr(),
												 plotOutput(ns("pc_scores_violin"), height = "400px")
											 )
							),
							
							tabPanel("Sample Loadings",
											 
											 column(3,
											 			 selectInput(
											 			 	ns("pca_color_by"),
											 			 	"Color by:",
											 			 	choices = NULL,  # Will be populated by observer
											 			 	selected = NULL
											 			 )
											 ),
											 column(3,
											 			 selectInput(
											 			 	ns("pca_shape_by"),
											 			 	"Shape by:",
											 			 	choices = NULL,  # Will be populated by observer
											 			 	selected = NULL
											 			 )
											 ),
											 column(12,
												 plotOutput(ns("pca_sample_biplot"), height = "600px"),
												 hr(),
												 plotOutput(ns("pca_sample_cont"), height = "600px"),
												 hr(),
												 plotOutput(ns("pc_loadings_barplot"), height = "600px"),
												 hr(),
												 plotOutput(ns("loadings_heatmap"), height = "600px")
											 )
							),
							
							tabPanel("Combined Biplot",
											 plotOutput(ns("pca_biplot_with_loadings"), height = "700px")
							),
							
							tabPanel("Variance",
											 plotOutput(ns("scree_plot"), height = "500px"),
											 hr(),
											 plotOutput(ns("pca_scores_heatmap"), height = "600px")
							)
						)
					),
					
					tabPanel(
						"Component Data",
						br(),
						h4("Denoised Expression Matrix"),
						p("View the actual denoised data for the selected PC removal level."),
						DT::dataTableOutput(ns("denoised_data_table")),
						
						hr(),
						
						h4("Variance Explained by Components"),
						p("Shows how much variance each principal component explains. Components marked 'Removed' were excluded during reconstruction."),
						tableOutput(ns("variance_table"))
					),
					
					tabPanel(
						"Component Matrices",
						br(),
						
						h3("PCA Component Analysis"),
						p("View the individual principal components and the reconstructed data after removing them."),
						
						hr(),
						
						## Set 1: PCA Components ####
						h4("Set 1: Principal Component Loadings"),
						p("Each tab shows the loadings (contributions) for that principal component."),
						
						uiOutput(ns("pca_components_tabs_ui")),
						
						hr(),
						
						## Set 2: Reconstructed Data ####
						h4("Set 2: Reconstructed Expression Matrices"),
						p("Each tab shows the denoised expression data after removing the specified number of PCs."),
						
						uiOutput(ns("reconstructed_data_tabs_ui"))
					),
					
					## Components #####
					tabPanel('Components',
									 mod_pc_visualizer_ui("pc_viz")),
						
					## Tab 1: PCA & Denoising ####
					
					
					tabPanel(
						"PCA & Denoising",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "PCA Variance Explained",
									width = NULL,
									plotOutput(ns("pca_variance_plot"), height = "300px")
								)
							)
						),
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PC Selection",
									width = NULL,
									sliderInput(
										ns("pc_view_slider"),
										"PCs Removed:",
										min = 0,
										max = 3,
										value = 1,
										step = 1,
										animate = TRUE
									)
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Denoised Data Info",
									width = NULL,
									tableOutput(ns("denoised_info_table"))
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Denoised Data Heatmap",
									width = NULL,
									plotOutput(ns("denoised_heatmap"), height = "600px")
								)
							),
							column(
								width = 12,
								shinydashboard::box(
									title = "Denoised Data Density",
									width = NULL,
									plotOutput(ns("denoised_density"), height = "600px")
								)
							)
						)
					),
					
					## Tab 2: Cutpoint Analysis ####
					tabPanel(
						"Cutpoint Analysis",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Cutpoint Metrics",
									width = NULL,
									plotOutput(ns("cutpoint_summary_plot"), height = "600px")
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Cutpoint Results Table",
									width = NULL,
									DTOutput(ns("cutpoint_table"))
								)
							)
						)
					),
					## Pooled Normal Analysis ####
					tabPanel(
						"Pooled Normal Analysis",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Pooled Normal Sample Selection",
									width = NULL,
									status = "info",
									collapsible = TRUE,
									collapsed = FALSE,
									
									fluidRow(
										column(
											width = 4,
											numericInput(
												ns("pn_pc_level"),
												"PCs Removed:",
												value = 3,
												min = 0,
												max = 10,
												step = 1
											)
										),
										column(
											width = 4,
											checkboxInput(
												ns("pn_show_expected_aabs"),
												"Highlight Expected PN AAbs",
												value = TRUE
											)
										),
										column(
											width = 4,
											checkboxInput(
												ns("pn_cluster_samples"),
												"Cluster Samples",
												value = FALSE
											)
										)
									)
								)
							)
						),
						
						# Statistics row
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PN Sample Statistics",
									width = NULL,
									tableOutput(ns("pn_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Expected PN AAb Detection",
									width = NULL,
									status = "warning",
									tableOutput(ns("pn_expected_aabs_table"))
								)
							)
						),
						
						# Three visualization modules side by side (plots stacked vertically within each)
						fluidRow(
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_neti"), title = "NetI Data", status = "primary", show_density = TRUE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_norm"), title = "Normalized Data", status = "info", show_density = TRUE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_denoised"), title = "Denoised Data", status = "success", show_density = TRUE)
							)
						),
						
						# Comparison plot
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Comparison: PN vs Non-PN Samples",
									width = NULL,
									plotOutput(ns("pn_comparison_plot"), height = "500px")
								)
							)
						)
					),
					
					## Tab 3: AAb-Called Data ####
					tabPanel(
						"AAb-Called Data",
						br(),
						
						# Cutpoint selection row
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "Cutpoint Selection",
									width = NULL,
									sliderInput(
										ns("cutpoint_slider"),
										"Select Cutpoint:",
										min = 0.4,
										max = 3,
										value = 1.0,
										step = 0.1
									),
									tableOutput(ns("selected_cutpoint_info"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Optimal Cutpoint",
									width = NULL,
									status = "success",
									tableOutput(ns("optimal_cutpoint_table"))
								)
							)
						),
						
						# Full AAb-Called Data Heatmap (all samples)
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb-Called Data Heatmap (All Samples)",
									width = NULL,
									status = "info",
									plotOutput(ns("aab_called_heatmap"), height = "600px")
								)
							)
						),
						
						# AAb Call Rate Distribution
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Call Rate Distribution",
									width = NULL,
									plotOutput(ns("aab_call_rate_hist"), height = "400px")
								)
							)
						),
						
						# Pooled Normal Sample Options
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Pooled Normal Sample Analysis Options",
									width = NULL,
									status = "warning",
									collapsible = TRUE,
									collapsed = FALSE,
									
									p(icon("info-circle"), 
										strong("Note:"), 
										"The visualizations below show only Pooled Normal samples from the AAb-called data."),
									
									fluidRow(
										column(
											width = 6,
											checkboxInput(
												ns("aab_show_expected_aabs"),
												"Highlight Expected PN AAbs",
												value = TRUE
											)
										),
										column(
											width = 6,
											checkboxInput(
												ns("aab_cluster_samples"),
												"Cluster Samples",
												value = FALSE
											)
										)
									)
								)
							)
						),
						
						# PN Sample Statistics for AAb-called data
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PN AAb Call Statistics",
									width = NULL,
									tableOutput(ns("aab_pn_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "PN Expected AAb Detection (AAb-Called)",
									width = NULL,
									status = "warning",
									tableOutput(ns("aab_pn_expected_aabs_table"))
								)
							)
						),
						
						# Three visualization modules for PN samples: NetI, Norm, AAb-Called
						# No density plots for binary AAb-called data
						fluidRow(
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_neti"), title = "PN - NetI Data", status = "primary", show_density = FALSE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_norm"), title = "PN - Normalized Data", status = "info", show_density = FALSE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_called"), title = "PN - AAb-Called Data", status = "success", show_density = FALSE)
							)
						),
						
						# AAb Calls per PN Sample
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Calls per Pooled Normal Sample",
									width = NULL,
									plotOutput(ns("aab_calls_per_pn_sample"), height = "400px")
								)
							)
						)
					),

					## Tab 4: Visualization ####
					tabPanel(
						"Visualization",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Borders (on Normalized Data)",
									width = NULL,
									plotOutput(ns("aab_borders_plot"), height = "800px")
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "t-SNE Plot",
									width = NULL,
									fluidRow(
										column(
											width = 4,
											selectInput(
												ns("tsne_color"),
												"Color by:",
												choices = NULL
											)
										),
										column(
											width = 4,
											selectInput(
												ns("tsne_shape"),
												"Shape by:",
												choices = c("None" = "none")
											)
										),
										column(
											width = 4,
											numericInput(
												ns("tsne_perplexity"),
												"Perplexity:",
												value = 30,
												min = 5,
												max = 50
											)
										)
									),
									plotOutput(ns("tsne_plot"), height = "600px")
								)
							)
						)
					),
					
					# Tab 5: Summary & Export
					tabPanel(
						"Summary",
						br(),
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "AAb Summary Statistics",
									width = NULL,
									status = "info",
									tableOutput(ns("summary_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Parameter Comparison",
									width = NULL,
									status = "warning",
									DTOutput(ns("parameter_comparison_table"))
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Export Template",
									width = NULL,
									textInput(
										ns("template_name"),
										"Template Name:",
										value = "AAb_caller_template"
									),
									textInput(
										ns("project_name"),
										"Project Name:",
										value = "MyProject"
									),
									downloadButton(
										ns("download_template"),
										"Download AAb_caller_template.R",
										class = "btn-primary"
									)
								)
							)
						),
						fluidRow(
							hr(),
							
							h4("PC Component Comparison"),
							p("Compare the best parameters across all PC removal levels. Rank 1 = best overall."),
							DT::dataTableOutput(ns("pc_comparison_table")),
							
							hr(),
							
							h4("Detailed Results: All Parameters"),
							p("Complete table showing metrics for every PC level × cutpoint combination tested."),
							DT::dataTableOutput(ns("all_cutpoints_table"))
						)
					)
				)
			)
		)
	)
}


#' Denoiser Module Server
#'
#' @param id Character; module namespace ID
#' @param eset_raw Reactive; ExpressionSet with raw/NetI data
#' @param eset_norm Reactive; ExpressionSet with normalized data (optional)
#'
#' @export
mod_denoiser_server <- function(id, ExpSet_list, eset_raw, eset_norm = NULL,pn_limma_results = NULL) {
	moduleServer(id, function(input, output, session) {
		
		
		
		ns <- session$ns
		
		observeEvent(input$denoise_mod_debug, {
			browser()
		})
		
		# Auto-populate expected PN AAbs from limma results
		# observe({
		# 	req(pn_limma_results())
		# 	
		# 	limma_res <- pn_limma_results()
		# 	
		# 	if (!is.null(limma_res$exp_PN_AAbs)) {
		# 		# Update expected AAbs range input
		# 		updateTextInput(
		# 			session,
		# 			"expected_pn_count",
		# 			value = paste(limma_res$exp_PN_AAbs, collapse = ",")
		# 		)
		# 		
		# 		# Update expected AAbs list
		# 		updateTextAreaInput(
		# 			session,
		# 			"expected_pn_aabs",
		# 			value = paste(limma_res$PN_AAbs, collapse = "\n")
		# 		)
		# 		
		# 		showNotification(
		# 			"✅ Expected PN AAbs auto-populated from limma analysis!",
		# 			type = "message",
		# 			duration = 5
		# 		)
		# 	}
		# })
		
		# Reactive values ####
		rv <- reactiveValues(
			denoise_results = NULL,
			cutpoint_results = NULL,
			optimal_cutpoint = NULL,
			aab_called_data = NULL,
			all_results = NULL,
			ExpSet_list = NULL
		)
		
		# PCA visualisation #####
		
		# Add to your server:
		# Replace the problematic pca_biplot output with this fixed version:
		
		# Add this observer BEFORE the output$pca_biplot code:
		
		## Update PCA plot controls ####
		observe({
			req(rv$denoise_results, eset_raw())
			
			# Get number of available PCs
			n_available_pcs <- ncol(rv$denoise_results$pca_result$x)
			n_show <- min(n_available_pcs, 10)  # Show max 10 PCs in dropdown
			
			# Update PC selectors
			updateSelectInput(
				session,
				"biplot_pc_x",
				choices = setNames(1:n_show, paste0("PC", 1:n_show)),
				selected = 1
			)
			
			updateSelectInput(
				session,
				"biplot_pc_y",
				choices = setNames(1:n_show, paste0("PC", 1:n_show)),
				selected = 2
			)
			
			# Update color/shape selectors
			metadata <- Biobase::pData(eset_raw())
			col_names <- colnames(metadata)
			
			updateSelectInput(
				session,
				"pca_color_by",
				choices = col_names,
				selected = if ("Sample_Group" %in% col_names) "Sample_Group" else col_names[1]
			)
			
			updateSelectInput(
				session,
				"pca_shape_by",
				choices = c("None" = "none", col_names),
				selected = if ("Batch_ID" %in% col_names) "Batch_ID" else "none"
			)
			
			featureData <- Biobase::fData(eset_raw())  
			feature_col_names <- colnames(featureData)
			updateSelectInput(
				session,
				"features_pca_color_by",
				choices = feature_col_names,
				selected = if ("PSA" %in% feature_col_names) "PSA" else col_names[1]
			)
			
			updateSelectInput(
				session,
				"features_pca_shape_by",
				choices = c("None" = "none", feature_col_names),
				selected = if ("ncf" %in% feature_col_names) "ncf" else "none"
			)
		})
		
		# Add this helper function to your server (at the top, before observers):
		
		## Helper: Get PCA data in visualization-friendly format ####
		get_pca_for_viz <- function(pca_result, metadata) {
			# Your PCA structure:
			# - pca_result$x = Features × PCs (feature scores)
			# - pca_result$rotation = Samples × PCs (sample loadings)
			
			# For visualization, we want sample positions, which are in $rotation
			sample_scores <- pca_result$rotation  # Samples × PCs
			feature_loadings <- pca_result$x      # Features × PCs
			
			# Validate sample names match
			if (!all(rownames(sample_scores) %in% rownames(metadata))) {
				warning("Sample name mismatch between PCA and metadata")
			}
			
			return(list(
				sample_scores = sample_scores,      # Use for sample biplots
				feature_loadings = feature_loadings # Use for feature contribution plots
			))
		}
		
		output$pca_biplot <- renderPlot({    
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			featureData = Biobase::fData(eset_raw())
			
			# Get scores for selected PCs
			pc_x <- as.numeric(input$biplot_pc_x)  # e.g., 1
			pc_y <- as.numeric(input$biplot_pc_y)  # e.g., 2
			
			# ❌ This assumed pca_result$x contains Samples × PCs
			scores_df <- as.data.frame(pca_result$x[, c(pc_x, pc_y)])
			scores_df$Protein <- rownames(scores_df)
			
	
			
			
			
			# Merge with metadata
			plot_df <- merge(scores_df, featureData, by.x = "Protein", by.y = "row.names")
			
			
			# ✅ FIX: Get the input values and validate they exist in data
			color_var <- input$features_pca_color_by
			shape_var <- input$features_pca_shape_by
			
			# Check if variables exist in plot_df
			if (!is.null(color_var) && !color_var %in% colnames(plot_df)) {
				color_var <- NULL
			}
			if (!is.null(shape_var) && shape_var == "none") {
				shape_var <- NULL
			}
			if (!is.null(shape_var) && !shape_var %in% colnames(plot_df)) {
				shape_var <- NULL
			}
			# Plot
			ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[paste0("PC", pc_x)]], 
																						y = .data[[paste0("PC", pc_y)]])) +
				ggplot2::geom_point(ggplot2::aes(color = .data[[input$features_pca_color_by]], 
																				 shape = .data[[input$features_pca_shape_by]]), 
														size = 3, alpha = 0.7) +
				ggplot2::stat_ellipse(ggplot2::aes(color = .data[[input$features_pca_color_by]]), level = 0.95) +
				ggplot2::labs(
					title = paste0("PCA: PC", pc_x, " vs PC", pc_y),
					x = paste0("PC", pc_x, " (", round(rv$denoise_results$variance_explained[pc_x], 1), "%)"),
					y = paste0("PC", pc_y, " (", round(rv$denoise_results$variance_explained[pc_y], 1), "%)")
				) +
				ggplot2::theme_minimal(base_size = 14) +
				ggplot2::theme(legend.position = "right")
			
			

		})
	
		output$pca_feature_cont = renderPlot({ 
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			featureData = Biobase::fData(eset_raw())
			
			pc_x <- as.numeric(input$biplot_pc_x)  # e.g., 1
			pc_y <- as.numeric(input$biplot_pc_y)

			
			x_l = pca_result$x %>% 
				as.data.frame() %>% 
				rownames_to_column('Protein') %>% 
				gather(key = PC,value = value,-1) %>% 
				left_join(featureData)
			
			(PCx = paste0("PC", pc_x))
			(PCy = paste0("PC", pc_y))

			
			# Get top and bottom 20 proteins for this PC
			proteins_to_plot_x <- x_l %>% 
				filter(PC %in% c(PCx)) %>% 
				arrange(value) %>% 
				slice(c(1:30, (n()-29):n())) %>%  # First 20 and last 20
				pull(Protein)
			proteins_to_plot_y <- x_l %>% 
				filter(PC %in% c(PCy)) %>% 
				arrange(value) %>% 
				slice(c(1:30, (n()-29):n())) %>%  # First 20 and last 20
				pull(Protein)
			proteins_to_plot = x_l %>% 
				filter(PC %in% c(PCx,PCy)) %>% 
				filter(Protein %in% c(proteins_to_plot_x,proteins_to_plot_y)) %>% 
				arrange(value) %>% 
				pull(Protein)
			# Filter and plot
			x_l %>% 
				filter(Protein %in% proteins_to_plot, PC %in% c(PCx,PCy)) %>% 
				mutate(Protein = factor(Protein, levels = unique(proteins_to_plot))) %>% 
				ggplot(aes(x = Protein, y = value, fill = .data[[input$features_pca_color_by]])) + 
				geom_col() + 
				#scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
				theme_minimal() +
				theme(
					axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
				) +
				facet_grid(PC ~ .)
		})
		
		
		output$pca_sample_biplot <- renderPlot({     
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			featureData = Biobase::fData(eset_raw())
			
			# Get scores for selected PCs
			pc_x <- as.numeric(input$biplot_pc_x)  # e.g., 1
			pc_y <- as.numeric(input$biplot_pc_y)  # e.g., 2
			
			# ❌ This assumed pca_result$x contains Samples × PCs
			scores_df <- as.data.frame(pca_result$rotation[, c(pc_x, pc_y)])
			scores_df$Sample <- rownames(scores_df)
			
			
			
			
			
			# Merge with metadata
			plot_df <- merge(scores_df, metadata, by.x = "Sample", by.y = "row.names")
			
			
			# ✅ FIX: Get the input values and validate they exist in data
			color_var <- input$features_pca_color_by
			shape_var <- input$features_pca_shape_by
			

			# Plot
			ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[paste0("PC", pc_x)]], 
																						y = .data[[paste0("PC", pc_y)]])) +
				ggplot2::geom_point(ggplot2::aes(color = .data[[input$pca_color_by]], 
																				 shape = .data[[input$pca_shape_by]]), 
														size = 3, alpha = 0.7) +
				ggplot2::stat_ellipse(ggplot2::aes(color = .data[[input$pca_color_by]]), level = 0.95) +
				ggplot2::labs(
					title = paste0("PCA: PC", pc_x, " vs PC", pc_y),
					x = paste0("PC", pc_x, " (", round(rv$denoise_results$variance_explained[pc_x], 1), "%)"),
					y = paste0("PC", pc_y, " (", round(rv$denoise_results$variance_explained[pc_y], 1), "%)")
				) +
				ggplot2::theme_minimal(base_size = 14) +
				ggplot2::theme(legend.position = "right")
			
			
			
		})
		
		output$pca_sample_cont = renderPlot({ 
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			featureData = Biobase::fData(eset_raw())
			
			pc_x <- as.numeric(input$biplot_pc_x)  # e.g., 1
			pc_y <- as.numeric(input$biplot_pc_y)
			
			
			x_l = pca_result$rotation %>% 
				as.data.frame() %>% 
				rownames_to_column('Sample') %>% 
				gather(key = PC,value = value,-1) %>% 
				left_join(metadata)
			
			(PCx = paste0("PC", pc_x))
			(PCy = paste0("PC", pc_y))
			
			
			# Get top and bottom 20 proteins for this PC
			proteins_to_plot_x <- x_l %>% 
				filter(PC %in% c(PCx)) %>% 
				arrange(value) %>% 
				slice(c(1:30, (n()-29):n())) %>%  # First 20 and last 20
				pull(Sample)
			proteins_to_plot_y <- x_l %>% 
				filter(PC %in% c(PCy)) %>% 
				arrange(value) %>% 
				slice(c(1:30, (n()-29):n())) %>%  # First 20 and last 20
				pull(Sample)
			proteins_to_plot = x_l %>% 
				filter(PC %in% c(PCx,PCy)) %>% 
				filter(Sample %in% c(proteins_to_plot_x,proteins_to_plot_y)) %>% 
				arrange(value) %>% 
				pull(Sample)
			# Filter and plot
			x_l %>% 
				filter(Sample %in% proteins_to_plot, PC %in% c(PCx,PCy)) %>% 
				mutate(Sample = factor(Sample, levels = unique(proteins_to_plot))) %>% 
				ggplot(aes(x = Sample, y = value, fill = .data[[input$pca_color_by]])) + 
				geom_col() + 
				#scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
				theme_minimal() +
				theme(
					axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
				) +
				facet_grid(PC ~ .)
		})
		
		# output$pca_biplot <- renderPlot({  
		# 	req(rv$denoise_results, eset_raw())
		# 	
		# 	pca_result <- rv$denoise_results$pca_result
		# 	
		# 	metadata <- Biobase::pData(eset_raw())
		# 	featureData = Biobase::fData(eset_raw())
		# 	
		# 	# Validate inputs
		# 	req(input$biplot_pc_x, input$biplot_pc_y)
		# 	
		# 	pc_x <- as.integer(input$biplot_pc_x)
		# 	pc_y <- as.integer(input$biplot_pc_y)
		# 	
		# 	# ✅ Get sample positions from rotation (not x)
		# 	pca_viz <- get_pca_for_viz(pca_result, metadata)
		# 	sample_scores <- pca_viz$sample_scores  # Samples × PCs
		# 	
		# 	# Validate PC indices
		# 	max_pcs <- ncol(sample_scores)
		# 	if (pc_x > max_pcs || pc_y > max_pcs || pc_x < 1 || pc_y < 1) {
		# 		return(NULL)
		# 	}
		# 	
		# 	# Extract selected PCs
		# 	scores_df <- as.data.frame(sample_scores[, c(pc_x, pc_y), drop = FALSE])
		# 	scores_df$Sample <- rownames(scores_df)
		# 	
		# 	# Rename columns
		# 	colnames(scores_df)[1:2] <- c("PC_X", "PC_Y")
		# 	
		# 	# Merge with metadata
		# 	plot_df <- merge(scores_df, metadata, by.x = "Sample", by.y = "row.names", all.x = TRUE)
		# 	
		# 	if (nrow(plot_df) == 0) {
		# 		showNotification("Error: Unable to merge PCA scores with metadata", type = "error")
		# 		return(NULL)
		# 	}
		# 	
		# 	# Get color/shape variables
		# 	color_var <- if (!is.null(input$pca_color_by) && input$pca_color_by %in% colnames(plot_df)) {
		# 		input$pca_color_by
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	shape_var <- if (!is.null(input$pca_shape_by) && input$pca_shape_by != "none" && input$pca_shape_by %in% colnames(plot_df)) {
		# 		input$pca_shape_by
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	# Build plot
		# 	p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = PC_X, y = PC_Y))
		# 	
		# 	if (!is.null(color_var) && !is.null(shape_var)) {
		# 		p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_var]], shape = .data[[shape_var]]), 
		# 																 size = 3, alpha = 0.7)
		# 		if (is.factor(plot_df[[color_var]]) || is.character(plot_df[[color_var]])) {
		# 			p <- p + ggplot2::stat_ellipse(ggplot2::aes(color = .data[[color_var]]), level = 0.95)
		# 		}
		# 	} else if (!is.null(color_var)) {
		# 		p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_var]]), size = 3, alpha = 0.7)
		# 		if (is.factor(plot_df[[color_var]]) || is.character(plot_df[[color_var]])) {
		# 			p <- p + ggplot2::stat_ellipse(ggplot2::aes(color = .data[[color_var]]), level = 0.95)
		# 		}
		# 	} else if (!is.null(shape_var)) {
		# 		p <- p + ggplot2::geom_point(ggplot2::aes(shape = .data[[shape_var]]), size = 3, alpha = 0.7)
		# 	} else {
		# 		p <- p + ggplot2::geom_point(size = 3, alpha = 0.7, color = "steelblue")
		# 	}
		# 	
		# 	p <- p + ggplot2::labs(
		# 		title = paste0("PCA Sample Biplot: PC", pc_x, " vs PC", pc_y),
		# 		x = paste0("PC", pc_x, " (", round(rv$denoise_results$variance_explained[pc_x], 1), "%)"),
		# 		y = paste0("PC", pc_y, " (", round(rv$denoise_results$variance_explained[pc_y], 1), "%)")
		# 	) +
		# 		ggplot2::theme_minimal(base_size = 14) +
		# 		ggplot2::theme(legend.position = "right")
		# 	
		# 	p
		# })
		
		output$pca_scores_heatmap <- renderPlot({
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			
			# Get first N PCs
			n_pcs <- min(10, ncol(pca_result$x))
			scores_matrix <- t(pca_result$x[, 1:n_pcs])
			
			# Annotation
			anno_df <- metadata[colnames(scores_matrix), input$annotation_cols, drop = FALSE]
			anno_col <- color_distinct(anno_df, 1:ncol(anno_df))
			
			pheatmap::pheatmap(
				scores_matrix,
				annotation_col = anno_df,
				annotation_colors = anno_col,
				cluster_rows = FALSE,
				cluster_cols = TRUE,
				show_colnames = FALSE,
				main = "PCA Scores Heatmap (First 10 PCs)",
				labels_row = paste0("PC", 1:n_pcs, " (", 
														round(rv$denoise_results$variance_explained[1:n_pcs], 1), "%)")
			)
		})
		
		output$pc_scores_violin <- renderPlot({
			req(rv$denoise_results, eset_raw()) 
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			featureData = Biobase::fData(eset_raw())
			
			# Select PC to view
			#pc_num <- input$pc_view_slider
			pc_num = 1
			#pc_num = input$biplot_pc_x
		
			
			scores_df <- data.frame(
				Protein = rownames(pca_result$x),
				PC_score = pca_result$x[, pc_num]
			)
			
			plot_df <- merge(scores_df, featureData, by.x = "Protein", by.y = "row.names")
			
			ggplot2::ggplot(plot_df, ggplot2::aes(x = PSA, y = PC_score, fill = PSA)) +
				ggplot2::geom_violin(alpha = 0.6) +
				ggplot2::geom_boxplot(width = 0.2, alpha = 0.8) +
				ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
				ggplot2::labs(
					title = paste0("PC", pc_num, " Scores by Group (", 
												 round(rv$denoise_results$variance_explained[pc_num], 1), "% variance)"),
					x = "Sample Group",
					y = paste0("PC", pc_num, " Score")
				) +
				ggplot2::theme_minimal(base_size = 14) +
				ggplot2::theme(legend.position = "none")
		})
		
		output$pc_loadings_barplot <- renderPlot({
			req(rv$denoise_results)
			
			pca_result <- rv$denoise_results$pca_result
			pc_num <- input$pc_view_slider
			
			# Get loadings for this PC
			loadings <- pca_result$rotation[, pc_num]
			
			# Top 20 by absolute value
			top_loadings <- sort(abs(loadings), decreasing = TRUE)[1:20]
			top_features <- names(top_loadings)
			loading_df <- data.frame(
				Feature = factor(top_features, levels = top_features),
				Loading = loadings[top_features]
			)
			
			ggplot2::ggplot(loading_df, ggplot2::aes(x = Feature, y = Loading, fill = Loading > 0)) +
				ggplot2::geom_col() +
				ggplot2::coord_flip() +
				ggplot2::scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral"),
																	 labels = c("Positive", "Negative")) +
				ggplot2::labs(
					title = paste0("Top 20 Features Contributing to PC", pc_num),
					x = "Antibody",
					y = "Loading (Contribution)",
					fill = "Direction"
				) +
				ggplot2::theme_minimal(base_size = 12)
		})
		
		output$loadings_heatmap <- renderPlot({
			req(rv$denoise_results)
			
			pca_result <- rv$denoise_results$pca_result
			
			# Get first N PCs
			n_pcs <- min(10, ncol(pca_result$rotation))
			loadings_matrix <- pca_result$rotation[, 1:n_pcs]
			
			# Optional: Filter to top varying features
			feature_variance <- apply(loadings_matrix, 1, var)
			top_features <- names(sort(feature_variance, decreasing = TRUE)[1:100])
			loadings_subset <- loadings_matrix[top_features, ]
			
			pheatmap::pheatmap(
				loadings_subset,
				cluster_rows = TRUE,
				cluster_cols = FALSE,
				show_rownames = TRUE,
				fontsize_row = 6,
				main = "Feature Loadings Across First 10 PCs",
				color = colorRampPalette(c("blue", "white", "red"))(100),
				breaks = seq(-max(abs(loadings_subset)), max(abs(loadings_subset)), length.out = 101)
			)
		})
		
		output$pca_biplot_with_loadings <- renderPlot({
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			
			# Scores
			scores <- as.data.frame(pca_result$x[, 1:2])
			scores$Sample_Group <- metadata[rownames(scores), "Sample_Group"]
			
			# Loadings (top 10 features)
			loadings <- pca_result$rotation[, 1:2]
			loading_importance <- sqrt(loadings[,1]^2 + loadings[,2]^2)
			top_features <- names(sort(loading_importance, decreasing = TRUE)[1:10])
			loadings_plot <- loadings[top_features, ] * 10  # Scale for visibility
			loadings_df <- as.data.frame(loadings_plot)
			loadings_df$Feature <- rownames(loadings_df)
			
			ggplot2::ggplot() +
				# Samples
				ggplot2::geom_point(data = scores, 
														ggplot2::aes(x = PC1, y = PC2, color = Sample_Group), 
														size = 3, alpha = 0.6) +
				# Loading arrows
				ggplot2::geom_segment(data = loadings_df,
															ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
															arrow = arrow(length = unit(0.2, "cm")),
															color = "red", alpha = 0.7) +
				ggplot2::geom_text(data = loadings_df,
													 ggplot2::aes(x = PC1, y = PC2, label = Feature),
													 hjust = 0, vjust = 0, size = 3, color = "darkred") +
				ggplot2::labs(title = "PCA Biplot: Samples and Feature Loadings") +
				ggplot2::theme_minimal()
		})
		
		output$scree_plot <- renderPlot({
			req(rv$denoise_results)
			
			variance_df <- data.frame(
				PC = 1:length(rv$denoise_results$variance_explained),
				Variance = rv$denoise_results$variance_explained,
				Cumulative = cumsum(rv$denoise_results$variance_explained)
			)
			
			# Only plot first 20 PCs
			variance_df <- variance_df[1:min(20, nrow(variance_df)), ]
			
			# Add removal indicator
			variance_df$Removed <- variance_df$PC <= input$n_PCs
			
			ggplot2::ggplot(variance_df, ggplot2::aes(x = factor(PC), y = Variance, fill = Removed)) +
				ggplot2::geom_col() +
				ggplot2::geom_line(ggplot2::aes(x = PC, y = Variance, group = 1), color = "black", linetype = "dashed") +
				ggplot2::scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "coral"),
																	 labels = c("Retained", "Removed")) +
				ggplot2::labs(
					title = "Scree Plot: Variance Explained by Each PC",
					x = "Principal Component",
					y = "Variance Explained (%)",
					fill = "Status"
				) +
				ggplot2::theme_minimal(base_size = 14)
		})
		
		
		output$random_pca_plots = renderUI({
			req(rv$denoise_results, eset_raw())
			
			pca_result <- rv$denoise_results$pca_result
			metadata <- Biobase::pData(eset_raw())
			feature_data = Biobase::fData(eset_raw())
			
			names(pca_result)
			sdev = pca_result$sdev
			pca_result$center
			pca_result$scale
			pca_result$x
			dim(pca_result$rotation)
			
			rotation_long = pca_result$rotation %>% 
				as.data.frame() %>% 
				rownames_to_column('Sample') %>% 
				gather(key = PC, value = value, -1) %>% 
				left_join(metadata)
			# pheatmap(
			# 	as.matrix(pca_result$rotation),
			# 	cluster_rows = TRUE,                      # ✅ Boolean to enable clustering
			# 	cluster_cols = TRUE,                      # ✅ Boolean to enable clustering
			# 	clustering_distance_rows = "euclidean",   # ✅ Separate argument for distance
			# 	clustering_distance_cols = "euclidean"    # ✅ Separate argument for distance
			# )
			
			ggplot(rotation_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = Sample,y = value, col = PC)) +
				theme(axis.text.x = element_text(angle = 90))
			
			sample_pca_psa = ggplot(rotation_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = PSA_score,y = value, col = PC))
			plot(density(sdev))
			
			x_long = pca_result$x %>% 
				as.data.frame() %>% 
				rownames_to_column('Protein') %>% 
				gather(key = PC, value = value, -1) %>% 
				left_join(feature_data)
			
			x_long$Protein = factor(x_long$Protein, rownames(pca_result$x %>% as.data.frame() %>% arrange(PC1)))
			ggplot(x_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = Protein,y = value, col = PC)) +
				geom_point(aes(x = Protein,y = PSA_logFC)) +
				theme(axis.text.x = element_text(angle = 90))
			
			x_long$Protein = factor(x_long$Protein, rownames(pca_result$x %>% as.data.frame() %>% arrange(PC3)))
			ggplot(x_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = Protein,y = value, col = PC)) +
				geom_point(aes(x = Protein,y = PSA_logFC)) +
				theme(axis.text.x = element_text(angle = 90))
			protein_pca_psa = ggplot(x_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = PSA_logFC,y = value, col = PC))
			
			protein_pca_correlation = ggplot(x_long %>% filter(PC %in% c('PC1','PC2','PC3','PC4','PC5'))) + 
				geom_point(aes(x = correlation,y = value, col = PC))
			
			output$sample_pca_psa = renderPlot({
				sample_pca_psa
			})
			
			output$protein_pca_psa = renderPlot({
				protein_pca_psa
			})
			
			output$protein_pca_correlation = renderPlot({
				protein_pca_correlation
			})
			
			lst = list(column(12,
											plotOutput(ns('sample_pca_psa')),
											plotOutput(ns('protein_pca_psa')),
											plotOutput(ns('protein_pca_correlation'))))
			
			do.call(tagList,lst)
		})
		
		
		# Component Data Tables ####
		
		## Denoised Data Table for Selected PC Level ####
		output$denoised_data_table <- renderDT({
			req(rv$denoise_results)
			pc_level <- input$pc_view_slider
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Convert to data frame with feature names
			denoised_df <- as.data.frame(denoised_data)
			denoised_df <- tibble::rownames_to_column(denoised_df, "Feature")
			
			DT::datatable(
				denoised_df,
				options = list(
					pageLength = 25,
					scrollX = TRUE,
					scrollY = "400px",
					dom = 'Bfrtip',
					buttons = list(
						list(
							extend = 'csv',
							text = 'Download CSV',
							filename = paste0('denoised_', pc_level, 'PCs'),
							title = paste0('Denoised Data - ', pc_level, ' PC(s) Removed')
						),
						'copy'
					)
				),
				extensions = 'Buttons',
				caption = htmltools::tags$caption(
					style = 'caption-side: top; text-align: center; font-weight: bold;',
					paste0('Denoised Expression Matrix - ', pc_level, ' PC(s) Removed')
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = 2:ncol(denoised_df), digits = 3)
		})
		
		## PC Component Comparison Table ####
		output$pc_comparison_table <- renderDT({
			req(rv$cutpoint_results)
			
			# Aggregate metrics by PC level (optimal cutpoint for each)
			pc_summary <- rv$cutpoint_results %>%
				group_by(PCs_removed) %>%
				filter(TP_FP_ratio == max(TP_FP_ratio, na.rm = TRUE)) %>%
				slice(1) %>%
				ungroup() %>%
				select(
					PCs_removed,
					cutpoint,
					PN_Aab_count_67_perc,
					PN_AAb_hit_rate,
					TP_FP_ratio,
					zz_2_frac,
					zz_4_frac,
					N_unique_AAbs,
					sample_AAb_median
				) %>%
				arrange(PCs_removed)
			
			# Add rank column
			pc_summary <- pc_summary %>%
				mutate(
					Rank = rank(-TP_FP_ratio, ties.method = "first"),
					.after = PCs_removed
				)
			
			DT::datatable(
				pc_summary,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 't',  # Just table, no pagination needed
					columnDefs = list(
						list(className = 'dt-center', targets = '_all')
					)
				),
				caption = htmltools::tags$caption(
					style = 'caption-side: top; text-align: center; font-weight: bold;',
					'Best Parameters for Each PC Removal Level'
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 3) %>%
				DT::formatRound(columns = 'PN_AAb_hit_rate', digits = 1) %>%
				DT::formatStyle(
					'TP_FP_ratio',
					background = DT::styleColorBar(pc_summary$TP_FP_ratio, 'lightgreen'),
					backgroundSize = '100% 90%',
					backgroundRepeat = 'no-repeat',
					backgroundPosition = 'center'
				) %>%
				DT::formatStyle(
					'Rank',
					backgroundColor = DT::styleEqual(
						c(1, 2, 3),
						c('#d4edda', '#fff3cd', '#f8d7da')
					)
				) %>%
				DT::formatStyle(
					'zz_2_frac',
					backgroundColor = DT::styleInterval(
						c(0.001, 0.01),
						c('#d4edda', '#fff3cd', '#f8d7da')
					)
				)
		})
		
		## Detailed Metrics by PC and Cutpoint ####
		output$all_cutpoints_table <- renderDT({
			req(rv$cutpoint_results)
			
			# Show ALL cutpoints for ALL PC levels
			detailed_results <- rv$cutpoint_results %>%
				select(
					PCs_removed,
					cutpoint,
					PN_Aab_count_67_perc,
					PN_AAb_hit_rate,
					TP_FP_ratio,
					zz_2_frac,
					zz_4_frac,
					N_unique_AAbs,
					sample_AAb_median
				) %>%
				arrange(PCs_removed, cutpoint)
			
			DT::datatable(
				detailed_results,
				filter = 'top',
				options = list(
					pageLength = 25,
					scrollX = TRUE,
					scrollY = "500px",
					dom = 'Bfrtip',
					buttons = list(
						list(
							extend = 'csv',
							text = 'Download All Results',
							filename = 'all_cutpoint_results',
							title = 'Complete Cutpoint Analysis Results'
						),
						'copy'
					)
				),
				extensions = 'Buttons',
				caption = htmltools::tags$caption(
					style = 'caption-side: top; text-align: center; font-weight: bold;',
					'Complete Results: All PC Levels × All Cutpoints'
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 3) %>%
				DT::formatRound(columns = 'PN_AAb_hit_rate', digits = 1) %>%
				DT::formatStyle(
					'TP_FP_ratio',
					background = DT::styleColorBar(detailed_results$TP_FP_ratio, 'lightblue'),
					backgroundSize = '100% 90%',
					backgroundRepeat = 'no-repeat',
					backgroundPosition = 'center'
				)
		})
		
		## Variance Explained Table ####
		output$variance_table <- renderTable({
			req(rv$denoise_results)
			
			variance_df <- data.frame(
				PC = paste0("PC", seq_along(rv$denoise_results$variance_explained)),
				Variance_Percent = rv$denoise_results$variance_explained,
				Cumulative_Percent = cumsum(rv$denoise_results$variance_explained)
			)
			
			# Highlight removed PCs
			variance_df$Status <- ifelse(
				seq_along(rv$denoise_results$variance_explained) <= input$n_PCs,
				"Removed ❌",
				"Retained ✓"
			)
			
			# Show first 20 PCs
			head(variance_df, 20)
		}, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
		
		
		# PCA Components Tables (Set 1) ####
		
		## Generate tabs for each PC component ####
		output$pca_components_tabs_ui <- renderUI({
			req(rv$denoise_results)
			
			pca_result <- rv$denoise_results$pca_result
			n_pcs_to_show <- min(input$n_PCs + 2, 10)  # Show removed PCs + 2 more, max 10
			
			# Create tab panels for each PC
			tab_list <- lapply(1:n_pcs_to_show, function(i) {
				tabPanel(
					title = paste0("PC", i),
					br(),
					
					# PC info
					div(
						style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
						strong(paste0("Principal Component ", i)),
						br(),
						paste0("Variance Explained: ", rv$denoise_results$variance_explained[i], "%"),
						br(),
						if (i <= input$n_PCs) {
							tags$span(style = "color: red;", "❌ This component is REMOVED during denoising")
						} else {
							tags$span(style = "color: green;", "✓ This component is RETAINED")
						}
					),
					
					# PC loadings table
					DT::dataTableOutput(ns(paste0("pc_loadings_", i)))
				)
			})
			
			do.call(tabsetPanel, c(list(id = ns("pca_tabs")), tab_list))
		})
		
		## Render each PC loadings table ####
		observe({
			req(rv$denoise_results)
			
			pca_result <- rv$denoise_results$pca_result
			n_pcs_to_show <- min(input$n_PCs + 2, 10)
			
			lapply(1:n_pcs_to_show, function(i) {
				output[[paste0("pc_loadings_", i)]] <- DT::renderDataTable({
					
					# Get loadings (rotation matrix) for this PC
					loadings <- pca_result$rotation[, i, drop = FALSE]
					loadings_df <- as.data.frame(loadings)
					loadings_df$Feature <- rownames(loadings_df)
					colnames(loadings_df)[1] <- paste0("PC", i, "_Loading")
					
					# Reorder columns
					loadings_df <- loadings_df[, c("Feature", paste0("PC", i, "_Loading"))]
					
					# Sort by absolute loading (most important features first)
					loadings_df <- loadings_df[order(abs(loadings_df[[2]]), decreasing = TRUE), ]
					
					DT::datatable(
						loadings_df,
						options = list(
							pageLength = 25,
							scrollX = TRUE,
							scrollY = "500px",
							dom = 'Bfrtip',
							buttons = list(
								list(
									extend = 'csv',
									text = 'Download CSV',
									filename = paste0('PC', i, '_loadings'),
									title = paste0('Principal Component ', i, ' Loadings')
								)
							)
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
							style = 'caption-side: top; text-align: center;',
							paste0('PC', i, ' Loadings (sorted by absolute value)')
						),
						rownames = FALSE
					) %>%
						DT::formatRound(columns = 2, digits = 4) %>%
						DT::formatStyle(
							paste0("PC", i, "_Loading"),
							background = DT::styleColorBar(
								range(loadings_df[[2]]),
								'lightblue'
							),
							backgroundSize = '100% 90%',
							backgroundRepeat = 'no-repeat',
							backgroundPosition = 'center'
						)
				})
			})
		})
		
		# Reconstructed Data Tables (Set 2) ####
		
		## Generate tabs for each reconstruction level ####
		output$reconstructed_data_tabs_ui <- renderUI({
			req(rv$denoise_results)
			
			n_levels <- length(rv$denoise_results$denoised_data)
			
			# Create tab panels for each reconstruction level
			tab_list <- lapply(1:n_levels, function(i) {
				tabPanel(
					title = paste0(i, " PC", ifelse(i > 1, "s", ""), " Removed"),
					br(),
					
					# Reconstruction info
					div(
						style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
						strong(paste0("Reconstructed Data: ", i, " PC", ifelse(i > 1, "s", ""), " Removed")),
						br(),
						paste0("Removed Components: ", paste0("PC", 1:i, collapse = ", ")),
						br(),
						paste0("Total Variance Removed: ", 
									 round(sum(rv$denoise_results$variance_explained[1:i]), 1), "%")
					),
					
					# Summary statistics
					fluidRow(
						column(4, tableOutput(ns(paste0("recon_stats_", i)))),
						column(8, plotOutput(ns(paste0("recon_density_", i)), height = "250px"))
					),
					
					hr(),
					
					# Reconstructed data table
					DT::dataTableOutput(ns(paste0("reconstructed_table_", i)))
				)
			})
			
			do.call(tabsetPanel, c(list(id = ns("reconstructed_tabs")), tab_list))
		})
		
		## Render each reconstructed data table ####
		observe({
			req(rv$denoise_results)
			
			n_levels <- length(rv$denoise_results$denoised_data)
			
			lapply(1:n_levels, function(i) {
				
				# Statistics table
				output[[paste0("recon_stats_", i)]] <- renderTable({
					denoised_data <- rv$denoise_results$denoised_data[[i]]
					
					data.frame(
						Metric = c("Features", "Samples", "Min Value", "Max Value", "Mean", "Median", "SD"),
						Value = c(
							nrow(denoised_data),
							ncol(denoised_data),
							round(min(denoised_data), 3),
							round(max(denoised_data), 3),
							round(mean(as.matrix(denoised_data)), 3),
							round(median(as.matrix(denoised_data)), 3),
							round(sd(as.matrix(denoised_data)), 3)
						)
					)
				}, striped = TRUE, bordered = TRUE, spacing = "xs")
				
				# Density plot
				output[[paste0("recon_density_", i)]] <- renderPlot({
					denoised_data <- rv$denoise_results$denoised_data[[i]]
					
					data_vector <- as.vector(as.matrix(denoised_data))
					
					ggplot2::ggplot(data.frame(value = data_vector), ggplot2::aes(x = value)) +
						ggplot2::geom_density(fill = "steelblue", alpha = 0.6) +
						ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
						ggplot2::labs(
							title = paste0("Distribution: ", i, " PC", ifelse(i > 1, "s", ""), " Removed"),
							x = "Expression Value",
							y = "Density"
						) +
						ggplot2::theme_minimal()
				})
				
				# Data table
				output[[paste0("reconstructed_table_", i)]] <- DT::renderDataTable({
					denoised_data <- rv$denoise_results$denoised_data[[i]]
					
					# Convert to data frame with feature names
					denoised_df <- as.data.frame(denoised_data)
					denoised_df$Feature <- rownames(denoised_df)
					
					# Reorder to put Feature first
					denoised_df <- denoised_df[, c("Feature", setdiff(names(denoised_df), "Feature"))]
					
					DT::datatable(
						denoised_df,
						options = list(
							pageLength = 25,
							scrollX = TRUE,
							scrollY = "400px",
							dom = 'Bfrtip',
							buttons = list(
								list(
									extend = 'csv',
									text = 'Download CSV',
									filename = paste0('denoised_', i, 'PCs_removed'),
									title = paste0('Denoised Data - ', i, ' PC(s) Removed')
								),
								'copy'
							)
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
							style = 'caption-side: top; text-align: center; font-weight: bold;',
							paste0('Reconstructed Expression Matrix (', i, ' PC', ifelse(i > 1, 's', ''), ' Removed)')
						),
						rownames = FALSE
					) %>%
						DT::formatRound(columns = 2:ncol(denoised_df), digits = 3)
				})
			})
		})
		
		# Update ExpressionSet with denoised and AAb data ####
		observeEvent(input$denoise_update_ExpSet, {   
			req(rv$denoise_results, rv$aab_called_data, ExpSet_list())
			
			denoise_matrix <- rv$denoise_results$denoised_data[[length(rv$denoise_results$denoised_data)]]
			aab_called_data <- rv$aab_called_data
			ExpSet_list_copy <- ExpSet_list()
			sample_ExpSet <- ExpSet_list_copy$sample_ExpSet
			
			denoise_matrix_l = denoise_matrix %>% 
				as.data.frame() %>% 
				rownames_to_column("Feautres") %>% 
				gather(key = Sample, value = denoise, -1)
			
			aab_called_data_l = aab_called_data %>% 
				as.data.frame() %>% 
				rownames_to_column("Feautres") %>% 
				gather(key = Sample, value = aab, -1)
			
			df_l = denoise_matrix_l %>% 
				left_join(aab_called_data_l)
			colnames(df_l)
			aab_m = df_l %>% 
				dplyr::select(Feautres,Sample,aab) %>% 
				spread(key = Sample, value = aab) %>% 
				column_to_rownames("Feautres") %>% 
				as.matrix()
			
			# Shift denoised data to make all values positive
			denoise_matrix_median <- denoise_matrix + abs(min(denoise_matrix)) + 1
			
			# Add matrices to ExpressionSet
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, denoise_matrix, 'denoised')
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, denoise_matrix_median, 'denoised_median')
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, aab_m, 'aab_called')
			
			ExpSet_list_copy$sample_ExpSet <- sample_ExpSet
			rv$ExpSet_list <- ExpSet_list_copy
			
			showNotification("✓ ExpressionSet updated with denoised and AAb-called data", type = "message", duration = 3)
		})
		
		# Download handler for ExpressionSet ####
		output$download_expset <- downloadHandler(
			filename = function() {
				paste0("ExpressionSet_with_reconstruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
			},
			content = function(file) {
				req(rv$ExpSet_list)
				withProgress(message = "Preparing download...", value = 0.5, {
					tryCatch({
						saveRDS(rv$ExpSet_list, file)
						showNotification("✓ ExpressionSet downloaded successfully", type = "message", duration = 3)
					}, error = function(e) {
						showNotification(paste("Error saving ExpressionSet:", e$message), type = "error", duration = 10)
					})
				})
			}
		)
		
		# Update UI choices based on eset ####
		observe({
			req(eset_raw()) 
			metadata <- Biobase::pData(eset_raw())
			
			updateSelectInput(session, "PN_column", choices = colnames(metadata),
												selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else colnames(metadata)[1])
			
			updateSelectizeInput(session, "annotation_cols", choices = colnames(metadata),
													 selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else NULL)
			
			updateSelectInput(session, "tsne_color", choices = colnames(metadata))
			updateSelectInput(session, "tsne_shape", choices = c("None" = "none", colnames(metadata)))
		})
		
		# Update PC slider max based on n_PCs input ####
		observe({
			updateSliderInput(session, "pc_view_slider", max = input$n_PCs, value = min(input$n_PCs))
		})
		
		# Parse PN AAbs input ####
		PN_AAbs_parsed <- reactive({
			req(input$PN_AAbs)
			aabs <- unlist(strsplit(input$PN_AAbs, ","))
			aabs <- trimws(aabs)
			aabs[aabs != ""]
		})
		
		# Run denoising pipeline ####
		observeEvent(input$run_denoise, {
			req(eset_raw())
			
			withProgress(message = 'Running denoising pipeline...', value = 0, {
				
				# Step 1: Validate inputs
				incProgress(0.1, detail = "Validating inputs...")
				tryCatch({
					validate_denoise_inputs(eset = eset_raw(), assay_name = input$assay_name,
																	PN_column = input$PN_column, PN_value = input$PN_value)
				}, error = function(e) {
					showNotification(paste("Validation error:", e$message), type = "error", duration = 10)
					return(NULL)
				})
				
				# Step 2: Remove PCs
				incProgress(0.2, detail = "Removing principal components...")
				rv$denoise_results <- denoise_remove_PCs(eset = eset_raw(), assay_name = input$assay_name,
																								 n_PCs = input$n_PCs, 
																								 scale = input$scale_pca, 
																								 center = input$center_pca,
																								 transpose = input$transpose_pca)
				
				# Step 3: Find cutpoints for each PC level
				incProgress(0.3, detail = "Testing cutpoints...")
				cut_seq <- seq(from = input$cut_min, to = input$cut_max, by = input$cut_step)
				all_cutpoint_results <- list()
				
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					incProgress(0.3 / length(rv$denoise_results$denoised_data), detail = paste("Testing PC level", i, "..."))
					
					cutpoint_results <- denoise_find_cutpoints(
						denoised_data = rv$denoise_results$denoised_data[[i]], eset = eset_raw(),
						PN_column = input$PN_column, PN_value = input$PN_value,
						PN_AAbs = PN_AAbs_parsed(), cut_seq = cut_seq, method = input$method
					)
					cutpoint_results$PCs_removed <- i
					all_cutpoint_results[[i]] <- cutpoint_results
				}
				rv$cutpoint_results <- do.call(rbind, all_cutpoint_results)
				
				# Step 4: Select optimal cutpoint
				incProgress(0.2, detail = "Selecting optimal cutpoint...")
				rv$optimal_cutpoint <- denoise_select_optimal_cutpoint(
					cutpoint_results = rv$cutpoint_results,
					exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max)
				)
				
				# Step 5: Generate final AAb-called data
				incProgress(0.2, detail = "Generating AAb-called data...")
				rv$aab_called_data <- denoise_apply_cutpoint(
					denoised_data = rv$denoise_results$denoised_data[[rv$optimal_cutpoint$PCs_removed]],
					cutpoint = rv$optimal_cutpoint$cutpoint, method = input$method
				)
				
				# Update cutpoint slider
				updateSliderInput(session, "cutpoint_slider", min = input$cut_min, max = input$cut_max,
													value = rv$optimal_cutpoint$cutpoint, step = input$cut_step)
				
				incProgress(0.1, detail = "Complete!")
				showNotification("Denoising complete!", type = "message", duration = 5)
			})
		})
		
		# PCA & Denoising Tab Outputs ####
		output$pca_variance_plot <- renderPlot({
			req(rv$denoise_results)
			variance_df <- data.frame(
				PC = paste0("PC", seq_along(rv$denoise_results$variance_explained)),
				Variance = rv$denoise_results$variance_explained
			)
			ggplot2::ggplot(variance_df[1:min(20, nrow(variance_df)), ], ggplot2::aes(x = PC, y = Variance)) +
				ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
				ggplot2::geom_vline(xintercept = input$n_PCs + 0.5, linetype = "dashed", color = "red", size = 1) +
				ggplot2::labs(title = "Variance Explained by Principal Components",
											subtitle = paste("Red line indicates", input$n_PCs, "PC(s) to be removed"),
											x = "Principal Component", y = "Variance Explained (%)") +
				ggplot2::theme_minimal() +
				ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
		})
		
		output$denoised_info_table <- renderTable({
			req(rv$denoise_results)
			pc_level <- input$pc_view_slider
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			data.frame(
				Metric = c("PCs Removed", "Features", "Samples", "Max Value", "Min Value"),
				Value = c(pc_level, nrow(denoised_data), ncol(denoised_data),
									round(max(denoised_data), 2), round(min(denoised_data), 2))
			)
		})
		
		output$denoised_heatmap <- renderPlot({
			req(rv$denoise_results, eset_raw())
			plot_denoise_heatmap(
				denoised_data = rv$denoise_results$denoised_data[[input$pc_view_slider]],
				eset = eset_raw(), annotation_cols = input$annotation_cols,
				title = paste("Denoised Data -", input$pc_view_slider, "PC(s) Removed"),
				show_rownames = FALSE, show_colnames = FALSE
			)
		})
		
		output$denoised_density <- renderPlot({
			req(rv$denoise_results, eset_raw())
			plot_density_data(rv$denoise_results$denoised_data[[input$pc_view_slider]], eset_raw())
		})
		
		# Cutpoint Analysis Tab Outputs ####
		output$cutpoint_summary_plot <- renderPlot({
			req(rv$cutpoint_results, rv$optimal_cutpoint)
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == input$pc_view_slider, ]
			plot_cutpoint_summary(cutpoint_results = cutpoint_subset, optimal_cutpoint = rv$optimal_cutpoint$cutpoint)
		})
		
		output$cutpoint_table <- renderDT({
			req(rv$cutpoint_results)
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == input$pc_view_slider, ]
			DT::datatable(cutpoint_subset, options = list(pageLength = 10, scrollX = TRUE, order = list(list(3, 'desc'))), rownames = FALSE) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2) %>%
				DT::formatStyle('TP_FP_ratio', background = DT::styleColorBar(cutpoint_subset$TP_FP_ratio, 'lightblue'),
												backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
		})
		
		# AAb-Called Data Tab Outputs ####
		current_aab_data <- reactive({
			req(rv$denoise_results, input$cutpoint_slider)
			denoise_apply_cutpoint(
				denoised_data = rv$denoise_results$denoised_data[[input$pc_view_slider]],
				cutpoint = input$cutpoint_slider, method = input$method
			)
		})
		
		output$selected_cutpoint_info <- renderTable({
			req(rv$cutpoint_results)
			cutpoint_subset <- rv$cutpoint_results[
				rv$cutpoint_results$PCs_removed == input$pc_view_slider &
					abs(rv$cutpoint_results$cutpoint - input$cutpoint_slider) < 0.01,
			]
			if (nrow(cutpoint_subset) > 0) {
				data.frame(
					Metric = c("Cutpoint", "PN AAb Count", "TP:FP Ratio", "ZZ_con2 Rate"),
					Value = c(cutpoint_subset$cutpoint[1], cutpoint_subset$PN_Aab_count_67_perc[1],
										round(cutpoint_subset$TP_FP_ratio[1], 2), round(cutpoint_subset$zz_2_frac[1], 3))
				)
			} else {
				data.frame(Metric = "No data", Value = "-")
			}
		})
		
		output$optimal_cutpoint_table <- renderTable({
			req(rv$optimal_cutpoint)
			data.frame(
				Parameter = c("PCs Removed", "Cutpoint", "PN AAb Count", "PN Hit Rate (%)", "TP:FP Ratio", "Unique AAbs", "Median AAbs/Sample"),
				Value = c(rv$optimal_cutpoint$PCs_removed, rv$optimal_cutpoint$cutpoint, rv$optimal_cutpoint$PN_Aab_count_67_perc,
									round(rv$optimal_cutpoint$PN_AAb_hit_rate, 1), round(rv$optimal_cutpoint$TP_FP_ratio, 2),
									rv$optimal_cutpoint$N_unique_AAbs, rv$optimal_cutpoint$sample_AAb_median)
			)
		})
		
		output$aab_called_heatmap <- renderPlot({
			req(current_aab_data(), eset_raw())
			plot_denoise_heatmap(
				denoised_data = current_aab_data(), eset = eset_raw(), annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE, show_colnames = FALSE
			)
		})
		
		output$aab_call_rate_hist <- renderPlot({
			req(current_aab_data())
			call_rates <- apply(current_aab_data(), 1, function(x) length(which(x > 0)) / length(x) * 100)
			graphics::hist(call_rates, breaks = 30, main = "Distribution of AAb Call Rates",
										 xlab = "Call Rate (%)", ylab = "Number of Antigens", col = "skyblue", border = "white")
			graphics::abline(v = median(call_rates), col = "red", lwd = 2, lty = 2)
			graphics::legend("topright", legend = paste("Median:", round(median(call_rates), 1), "%"),
											 col = "red", lwd = 2, lty = 2, bty = "n")
		})
		
		# Visualization Tab Outputs ####
		output$aab_borders_plot <- renderPlot({
			req(current_aab_data(), eset_raw(), eset_norm())
			background_data <- Biobase::exprs(eset_norm())
			if (!is.matrix(background_data)) background_data <- as.matrix(background_data)
			background_RC <- t(scale(t(background_data), scale = FALSE, center = TRUE))
			plot_denoise_borders(
				background_data = background_RC, aab_called_data = current_aab_data(),
				eset = eset_raw(), annotation_cols = input$annotation_cols,
				variable = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL,
				title = "AAb Borders on Row-Centered Data"
			)
		})
		
		output$tsne_plot <- renderPlot({
			req(current_aab_data(), eset_raw())
			shape_by <- if(input$tsne_shape == "none") NULL else input$tsne_shape
			plot_denoise_tsne(
				aab_called_data = current_aab_data(), eset = eset_raw(),
				color_by = input$tsne_color, shape_by = shape_by,
				perplexity = input$tsne_perplexity, seed = 42
			)
		})
		
		# Summary Tab Outputs ####
		output$summary_stats_table <- renderTable({
			req(rv$aab_called_data, eset_raw())
			calculate_aab_summary(
				aab_called_data = rv$aab_called_data, eset = eset_raw(),
				group_by = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL
			)
		})
		
		output$parameter_comparison_table <- renderDT({
			req(rv$cutpoint_results)
			comparison <- compare_denoise_parameters(
				cutpoint_results = rv$cutpoint_results, top_n = 20, sort_by = "TP_FP_ratio"
			)
			DT::datatable(comparison, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2)
		})
		
		# Download Handlers ####
		output$download_results <- downloadHandler(
			filename = function() paste0("denoiser_results_", Sys.Date(), ".zip"),
			content = function(file) {
				temp_dir <- tempdir()
				results_dir <- file.path(temp_dir, "denoiser_results")
				dir.create(results_dir, showWarnings = FALSE)
				
				write.csv(rv$aab_called_data, file.path(results_dir, "AAb_called_data.csv"))
				write.csv(rv$cutpoint_results, file.path(results_dir, "cutpoint_analysis.csv"), row.names = FALSE)
				write.csv(rv$optimal_cutpoint, file.path(results_dir, "optimal_cutpoint.csv"), row.names = FALSE)
				
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					write.csv(rv$denoise_results$denoised_data[[i]], file.path(results_dir, paste0("denoised_", i, "PCs.csv")))
				}
				zip(file, results_dir, flags = "-r9Xj")
			}
		)
		
		output$download_template <- downloadHandler(
			filename = function() paste0(input$template_name, ".R"),
			content = function(file) {
				generate_aab_caller_template(
					output_file = file, project_name = input$project_name, eset_file = "eset.rds",
					assay_name = input$assay_name, n_PCs = input$n_PCs, PN_AAbs = PN_AAbs_parsed(),
					exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max), annotation_cols = input$annotation_cols
				)
			}
		)
		
		# Pooled Normal Analysis - Data Preparation ####
		observe({
			req(rv$denoise_results)
			updateNumericInput(session, "pn_pc_level", value = input$pc_view_slider,
												 max = length(rv$denoise_results$denoised_data))
		})
		
		pn_data <- reactive({
			req(rv$denoise_results, eset_raw(), eset_norm())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			if (length(PN_samples) == 0) {
				showNotification("No Pooled Normal samples found!", type = "warning", duration = 5)
				return(NULL)
			}
			
			neti_data <- Biobase::exprs(eset_raw())
			norm_data <- Biobase::exprs(eset_norm())
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			pn_neti <- neti_data[, colnames(neti_data) %in% PN_samples, drop = FALSE]
			pn_norm <- norm_data[, colnames(norm_data) %in% PN_samples, drop = FALSE]
			pn_denoised <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			
			pn_neti <- pn_neti[rowSums(pn_neti) != 0, , drop = FALSE]
			pn_norm <- pn_norm[rowSums(pn_norm) != 0, , drop = FALSE]
			pn_denoised <- pn_denoised[rowSums(pn_denoised) != 0, , drop = FALSE]
			
			pn_metadata <- metadata[colnames(pn_denoised), , drop = FALSE]
			
			list(pn_neti = pn_neti, pn_norm = pn_norm, pn_denoised = pn_denoised,
					 pn_metadata = pn_metadata, pc_level = pc_level)
		})
		
		aab_pn_data <- reactive({ 
			req(current_aab_data(), eset_raw(), eset_norm())
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			if (length(PN_samples) == 0) return(NULL)
			
			neti_data <- Biobase::exprs(eset_raw())
			norm_data <- Biobase::exprs(eset_norm())
			aab_data <- current_aab_data()
			
			pn_aab <- aab_data[, colnames(aab_data) %in% PN_samples, drop = FALSE]
			pn_aab <- pn_aab[rowSums(pn_aab) != 0, , drop = FALSE]
			
			pn_neti <- neti_data[rownames(pn_aab), colnames(neti_data) %in% PN_samples, drop = FALSE]
			pn_norm <- norm_data[rownames(pn_aab), colnames(norm_data) %in% PN_samples, drop = FALSE]

			
			pn_metadata <- metadata[colnames(pn_aab), , drop = FALSE]
			
			list(pn_neti = pn_neti, pn_norm = pn_norm, pn_denoised = pn_aab,
					 pn_metadata = pn_metadata, pc_level = input$pn_pc_level)
		})
		
		# Pooled Normal Analysis - Statistics ####
		output$pn_stats_table <- renderTable({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			if (ncol(pn_denoised) == 0) return(data.frame(Metric = "No PN samples", Value = NA))
			
			data.frame(
				Metric = c("Number of PN Samples", "Number of Features", "Mean Expression", "Median Expression",
									 "SD Expression", "Min Expression", "Max Expression", "Features > 0", "% Features > 0"),
				Value = c(ncol(pn_denoised), nrow(pn_denoised), round(mean(pn_denoised), 3), round(median(pn_denoised), 3),
									round(sd(as.vector(pn_denoised)), 3), round(min(pn_denoised), 3), round(max(pn_denoised), 3),
									sum(rowMeans(pn_denoised) > 0), round(sum(rowMeans(pn_denoised) > 0) / nrow(pn_denoised) * 100, 1))
			)
		})
		
		output$pn_expected_aabs_table <- renderTable({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			if (ncol(pn_denoised) == 0) return(data.frame(AAb = "No data", Status = NA))
			
			expected_aabs <- PN_AAbs_parsed()
			expected_in_data <- expected_aabs[expected_aabs %in% rownames(pn_denoised)]
			
			if (length(expected_in_data) == 0) {
				return(data.frame(AAb = "None found", Status = "Not detected in data"))
			}
			
			aab_results <- data.frame(
				AAb = expected_in_data,
				Mean_Expression = sapply(expected_in_data, function(aab) round(mean(pn_denoised[aab, ]), 3)),
				Positive_Samples = sapply(expected_in_data, function(aab) sum(pn_denoised[aab, ] > 0)),
				Percent_Positive = sapply(expected_in_data, function(aab) round(sum(pn_denoised[aab, ] > 0) / ncol(pn_denoised) * 100, 1)),
				stringsAsFactors = FALSE
			)
			aab_results$Status <- ifelse(aab_results$Percent_Positive >= 50, "✓ Detected", "⚠ Weak/Absent")
			aab_results
		})
		
		output$aab_pn_stats_table <- renderTable({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			data.frame(
				Metric = c("Number of PN Samples", "Total AAb Calls", "Unique AAbs Called",
									 "Mean AAbs per Sample", "Median AAbs per Sample", "Max AAbs in a Sample"),
				Value = c(ncol(pn_aab), sum(pn_aab > 0), sum(rowSums(pn_aab) > 0),
									round(mean(colSums(pn_aab)), 1), median(colSums(pn_aab)), max(colSums(pn_aab)))
			)
		})
		
		output$aab_pn_expected_aabs_table <- renderTable({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			expected_aabs <- PN_AAbs_parsed()
			expected_in_data <- expected_aabs[expected_aabs %in% rownames(pn_aab)]
			
			if (length(expected_in_data) == 0) {
				return(data.frame(AAb = "None found", Status = "Not detected"))
			}
			
			aab_results <- data.frame(
				AAb = expected_in_data,
				Calls = sapply(expected_in_data, function(aab) sum(pn_aab[aab, ] > 0)),
				Percent = sapply(expected_in_data, function(aab) round(sum(pn_aab[aab, ] > 0) / ncol(pn_aab) * 100, 1)),
				stringsAsFactors = FALSE
			)
			aab_results$Status <- ifelse(aab_results$Percent >= 50, "✓ Called", "⚠ Not Called")
			aab_results
		})
		
		output$aab_calls_per_pn_sample <- renderPlot({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			aab_counts <- colSums(pn_aab)
			barplot(aab_counts, main = "AAb Calls per PN Sample", xlab = "Sample", ylab = "Number of AAb Calls",
							col = "steelblue", las = 2, cex.names = 0.8)
			abline(h = median(aab_counts), col = "red", lwd = 2, lty = 2)
			legend("topright", legend = paste("Median:", median(aab_counts)), col = "red", lwd = 2, lty = 2, bty = "n")
		})
		
		# Pooled Normal Comparison Plot ####
		output$pn_comparison_plot <- renderPlot({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			all_denoised <- rv$denoise_results$denoised_data[[pn_data()$pc_level]]
			non_pn_data <- all_denoised[, !(colnames(all_denoised) %in% PN_samples), drop = FALSE]
			
			if (ncol(pn_denoised) == 0 || ncol(non_pn_data) == 0) return(NULL)
			
			comparison_df <- data.frame(
				Expression = c(as.vector(pn_denoised), as.vector(non_pn_data)),
				Group = rep(c("Pooled Normal", "Other Samples"), c(length(as.vector(pn_denoised)), length(as.vector(non_pn_data))))
			)
			
			p1 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Expression, fill = Group)) +
				ggplot2::geom_density(alpha = 0.5) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(title = "Expression Distribution: PN vs Other Samples", x = "Expression Value", y = "Density") +
				ggplot2::theme_minimal(base_size = 12)
			
			p2 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
				ggplot2::geom_violin(alpha = 0.7) +
				ggplot2::geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(title = "Expression Distribution Comparison", x = "", y = "Expression Value") +
				ggplot2::theme_minimal(base_size = 12) +
				ggplot2::theme(legend.position = "none")
			
			gridExtra::grid.arrange(p1, p2, ncol = 2)
		})
		
		# Call PN Visualization Modules ####
		mod_pn_viz_server(
			id = "pn_neti",
			data = reactive({ req(pn_data()); pn_data()$pn_neti }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = "NetI Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		mod_pn_viz_server(
			id = "pn_norm",
			data = reactive({ req(pn_data()); pn_data()$pn_norm }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = "Normalized Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		mod_pn_viz_server(
			id = "pn_denoised",
			data = reactive({ req(pn_data()); pn_data()$pn_denoised }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = reactive({
				req(pn_data())
				pc_level <- pn_data()$pc_level
				paste0("Denoised Data (", pc_level, " PC", ifelse(pc_level > 1, "s", ""), " Removed)")
			}),
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		# # Call AAb PN Visualization Modules (no density for binary data) ####
		# mod_pn_viz_server(
		# 	id = "aab_pn_neti",
		# 	data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_neti }),
		# 	metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
		# 	title = "PN - NetI Data",
		# 	annotation_cols = reactive({ input$annotation_cols }),
		# 	highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
		# 	cluster_rows = FALSE,
		# 	cluster_cols = reactive({ input$aab_cluster_samples }),
		# 	show_density = FALSE
		# )
		# 
		# mod_pn_viz_server(
		# 	id = "aab_pn_norm",
		# 	data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_norm }),
		# 	metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
		# 	title = "PN - Normalized Data",
		# 	annotation_cols = reactive({ input$annotation_cols }),
		# 	highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
		# 	cluster_rows = FALSE,
		# 	cluster_cols = reactive({ input$aab_cluster_samples }),
		# 	show_density = FALSE
		# )
		# 
		# mod_pn_viz_server(
		# 	id = "aab_pn_called",
		# 	data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_denoised }),
		# 	metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
		# 	title = reactive({
		# 		paste0("PN - AAb-Called Data (Cutpoint: ", input$cutpoint_slider, ")")
		# 	}),
		# 	annotation_cols = reactive({ input$annotation_cols }),
		# 	highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
		# 	cluster_rows = FALSE,
		# 	cluster_cols = FALSE,
		# 	show_density = FALSE
		# )
		# 
		
		# Call AAb PN Visualization Modules (no density for binary data) ####
		mod_pn_viz_server(
			id = "aab_pn_neti",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_neti }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = "PN - NetI Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$aab_cluster_samples }),
			show_density = FALSE,
			show_rownames = TRUE  # Add this
		)
		
		mod_pn_viz_server(
			id = "aab_pn_norm",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_norm }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = "PN - Normalized Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$aab_cluster_samples }),
			show_density = FALSE,
			show_rownames = TRUE  # Add this
		)
		
		mod_pn_viz_server(
			id = "aab_pn_called",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_denoised }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = reactive({
				paste0("PN - AAb-Called Data (Cutpoint: ", input$cutpoint_slider, ")")
			}),
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = FALSE,
			show_density = FALSE,
			show_rownames = TRUE  # Show rownames for AAb-called data!
		)
		
		
		# Return results ####
		return(reactive({
			list(
				denoise_results = rv$denoise_results,
				cutpoint_results = rv$cutpoint_results,
				optimal_cutpoint = rv$optimal_cutpoint,
				aab_called_data = rv$aab_called_data,
				updated_expset = rv$ExpSet_list
			)
		}))
	})
}

# Replace the debug observer with this enhanced version:

observeEvent(input$debug, {
	if (!interactive()) {
		showNotification(
			"Debug mode only works in interactive R sessions",
			type = "warning",
			duration = 5
		)
		return(NULL)
	}
	
	# Determine current state
	has_eset <- !is.null(eset_raw())
	has_denoise <- !is.null(rv$denoise_results)
	has_cutpoints <- !is.null(rv$cutpoint_results)
	has_aab_called <- !is.null(rv$aab_called_data)
	
	message("\n╔═══════════════════════════════════════════════════════════╗")
	message("║              🔍 DEBUG MODE ACTIVATED                      ║")
	message("╚═══════════════════════════════════════════════════════════╝")
	
	# Show current state
	message("\n📊 Current State:")
	message("   ExpressionSet loaded: ", ifelse(has_eset, "✅", "❌"))
	message("   Denoising completed: ", ifelse(has_denoise, "✅", "❌"))
	message("   Cutpoints analyzed: ", ifelse(has_cutpoints, "✅", "❌"))
	message("   AAb-called data: ", ifelse(has_aab_called, "✅", "❌"))
	
	# Show available objects
	message("\n📍 Available objects:")
	if (has_eset) {
		message("   ✓ eset_raw() - Raw/NetI ExpressionSet")
		if (!is.null(eset_norm) && !is.null(eset_norm())) {
			message("   ✓ eset_norm() - Normalized ExpressionSet")
		}
	}
	if (has_denoise) {
		message("   ✓ rv$denoise_results - Denoised data and PCA results")
	}
	if (has_cutpoints) {
		message("   ✓ rv$cutpoint_results - Cutpoint analysis table")
		message("   ✓ rv$optimal_cutpoint - Optimal parameters")
	}
	if (has_aab_called) {
		message("   ✓ rv$aab_called_data - Final AAb-called matrix")
	}
	message("   ✓ input - All UI input values")
	
	# Context-specific commands
	message("\n💡 Suggested commands for current state:")
	
	if (has_eset && !has_denoise) {
		message("\n   === BEFORE DENOISING ===")
		message("   # Quick inspection")
		message("   quick_inspect_eset(eset_raw(), input$assay_name)")
		message("")
		message("   # Check expression data")
		message("   expr <- Biobase::exprs(eset_raw())")
		message("   class(expr)")
		message("   dim(expr)")
		message("   summary(as.vector(expr))")
		message("")
		message("   # Check metadata")
		message("   meta <- Biobase::pData(eset_raw())")
		message("   table(meta$Sample_Group)")
		message("")
		message("   # Check PN samples")
		message("   PN_samples <- get_PN_samples(eset_raw())")
		message("   length(PN_samples)")
	}
	
	if (has_denoise && !has_cutpoints) {
		message("\n   === AFTER DENOISING, BEFORE CUTPOINTS ===")
		message("   # Check denoised data")
		message("   rv$denoise_results$variance_explained")
		message("   denoised_1PC <- rv$denoise_results$denoised_data[[1]]")
		message("   dim(denoised_1PC)")
		message("   head(denoised_1PC[, 1:5])")
	}
	
	if (has_cutpoints && !has_aab_called) {
		message("\n   === CUTPOINT ANALYSIS COMPLETE ===")
		message("   # View cutpoint results")
		message("   head(rv$cutpoint_results)")
		message("   rv$optimal_cutpoint")
		message("")
		message("   # Check specific cutpoint")
		message("   rv$cutpoint_results[rv$cutpoint_results$cutpoint == 1.0, ]")
	}
	
	if (has_aab_called) {
		message("\n   === FULL PIPELINE COMPLETE ===")
		message("   # Check AAb-called data")
		message("   dim(rv$aab_called_data)")
		message("   sum(rv$aab_called_data > 0)  # Total AAb calls")
		message("   colSums(rv$aab_called_data > 0)  # AAbs per sample")
		message("   rowSums(rv$aab_called_data > 0)  # Samples per antigen")
		message("")
		message("   # Summary statistics")
		message("   calculate_aab_summary(rv$aab_called_data, eset_raw())")
	}
	
	# Always show general commands
	message("\n   === GENERAL COMMANDS ===")
	message("   ls()                    # List all objects")
	message("   str(rv)                 # Structure of reactive values")
	message("   names(input)            # All input names")
	message("   input$n_PCs             # Check specific input")
	
	message("\n⌨️  Type 'c' to continue, 'Q' to quit, 'n' to step")
	message("═══════════════════════════════════════════════════════════\n")
	
	browser()
})