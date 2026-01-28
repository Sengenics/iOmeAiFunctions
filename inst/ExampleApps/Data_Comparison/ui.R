# UI for File Comparison App

ui <- dashboardPage(
	
	dashboardHeader(title = "File Comparison Tool"),
	
	dashboardSidebar(
		sidebarMenu(
			menuItem("Upload Files", tabName = "upload", icon = icon("upload")),
			menuItem("Data Overview", tabName = "overview", icon = icon("table")),
			menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
			menuItem("Visualization", tabName = "visualization", icon = icon("chart-line"))
		),
		
		# Debug button (only visible in RStudio)
		uiOutput("debug_ui")
	),
	
	dashboardBody(
		tabItems(
			
			# Upload Tab
			tabItem(
				tabName = "upload",
				fluidRow(
					box(
						title = "File 1",
						status = "primary",
						solidHeader = TRUE,
						width = 6,
						fileInput("file1", "Choose File 1",
											accept = c("text/csv", "text/comma-separated-values",
																 "text/tab-separated-values", "text/plain",
																 ".csv", ".tsv", ".txt")),
						textOutput("file1_status"),
						hr(),
						h4("File 1 Summary:"),
						verbatimTextOutput("file1_summary")
					),
					
					box(
						title = "File 2",
						status = "primary",
						solidHeader = TRUE,
						width = 6,
						fileInput("file2", "Choose File 2",
											accept = c("text/csv", "text/comma-separated-values",
																 "text/tab-separated-values", "text/plain",
																 ".csv", ".tsv", ".txt")),
						textOutput("file2_status"),
						hr(),
						h4("File 2 Summary:"),
						verbatimTextOutput("file2_summary")
					)
				),
				
				fluidRow(
					box(
						title = "Instructions",
						status = "info",
						width = 12,
						solidHeader = TRUE,
						HTML("
              <h4>How to use this app:</h4>
              <ol>
                <li><strong>Upload Files:</strong> Select two files to compare (CSV, TSV, or TXT format)</li>
                <li><strong>Expected Format:</strong> First column = protein names, remaining columns = sample data (numeric)</li>
                <li><strong>View Data:</strong> Check the 'Data Overview' tab to preview loaded files</li>
                <li><strong>Compare:</strong> Go to 'Comparison' tab to see overlap and differences</li>
                <li><strong>Visualize:</strong> Explore visualizations in the 'Visualization' tab</li>
              </ol>
              <p><strong>Max file size:</strong> 100 MB</p>
            ")
					)
				)
			),
			
			# Data Overview Tab
			tabItem(
				tabName = "overview",
				fluidRow(
					box(
						title = "File 1 Data",
						status = "primary",
						solidHeader = TRUE,
						width = 12,
						DTOutput("file1_table")
					)
				),
				fluidRow(
					box(
						title = "File 2 Data",
						status = "primary",
						solidHeader = TRUE,
						width = 12,
						DTOutput("file2_table")
					)
				)
			),
			
			# Comparison Tab
			tabItem(
				tabName = "comparison",
				fluidRow(
					valueBoxOutput("common_proteins_box"),
					valueBoxOutput("unique_file1_box"),
					valueBoxOutput("unique_file2_box")
				),
				
				fluidRow(
					box(
						title = "Protein Overlap",
						status = "primary",
						solidHeader = TRUE,
						width = 6,
						plotOutput("venn_diagram", height = "400px")
					),
					
					box(
						title = "Comparison Summary",
						status = "info",
						solidHeader = TRUE,
						width = 6,
						verbatimTextOutput("comparison_summary")
					)
				),
				
				# NEW: Identity check section
				fluidRow(
					box(
						title = "Data Identity Check",
						status = "primary",
						solidHeader = TRUE,
						width = 6,
						collapsible = TRUE,
						collapsed = TRUE,
						verbatimTextOutput("identity_check")
					),
					
					box(
						title = "Identity Explanations",
						status = "info",
						solidHeader = TRUE,
						width = 6,
						collapsible = TRUE,
						collapsed = TRUE,
						htmlOutput("identity_explanations")
					)
				),
				
				fluidRow(
					box(
						title = "Common Proteins",
						status = "success",
						solidHeader = TRUE,
						width = 12,
						collapsible = TRUE,
						collapsed = TRUE,
						DTOutput("common_proteins_table")
					)
				),
				
				fluidRow(
					box(
						title = "Unique to File 1",
						status = "warning",
						solidHeader = TRUE,
						width = 6,
						collapsible = TRUE,
						collapsed = TRUE,
						DTOutput("unique_file1_table")
					),
					
					box(
						title = "Unique to File 2",
						status = "warning",
						solidHeader = TRUE,
						width = 6,
						collapsible = TRUE,
						collapsed = TRUE,
						DTOutput("unique_file2_table")
					)
				),
				
		
			),
			
			# Visualization Tab
			tabItem(
				tabName = "visualization",
				tabsetPanel(
					tabPanel("Correlation Heatmap",
				
						fluidRow(
							box(
								title = "Correlation Heatmap",
								status = "primary",
								solidHeader = TRUE,
								width = 12,
								plotlyOutput("correlation_heatmap", height = "600px")
							)
						)
					),
					tabPanel("Sample Scatter Plot",
				
						fluidRow(
							box(
								title = "Sample Scatter Plot",
								status = "primary",
								solidHeader = TRUE,
								width = 12,
								fluidRow(
									column(6, selectInput("scatter_x", "X-axis (File 1):", choices = NULL)),
									column(6, selectInput("scatter_y", "Y-axis (File 2):", choices = NULL))
								),
								plotlyOutput("scatter_plot", height = "500px")
							)
						)
					)
				)
			)
		)
	)
)