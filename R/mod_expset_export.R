#' ExpressionSet List Export Module - UI
#'
#' Export/save the entire ExpressionSet list
#'
#' @param id Module namespace ID
#' @export
mod_expset_export_ui <- function(id) {
	ns <- NS(id)
	
	fluidRow(
		box(
			title = "Export ExpressionSet List",
			width = 12,
			status = "primary",
			solidHeader = TRUE,
			
			p("Save the entire ExpressionSet list (including ComBat-corrected data) as an RDS file."),
			
			fluidRow(
				column(
					width = 6,
					textInput(
						ns("export_filename"),
						"Filename:",
						value = paste0("ExpSet_list_", Sys.Date(), ".rds"),
						placeholder = "ExpSet_list. rds"
					)
				),
				column(
					width = 6,
					br(),
					downloadButton(
						ns("download_expset_list"),
						"Download ExpressionSet List",
						class = "btn-success btn-lg",
						style = "width: 100%;"
					)
				)
			),
			
			hr(),
			
			h4("Current ExpressionSet List Contents:"),
			verbatimTextOutput(ns("expset_list_summary"))
		)
	)
}

#' ExpressionSet List Export Module - Server
#'
#' @param id Module namespace ID
#' @param ExpSet_list Reactive returning the ExpressionSet list
#' @export
mod_expset_export_server <- function(id, ExpSet_list) {
	moduleServer(id, function(input, output, session) {
		
		# Show summary of ExpSet list
		output$expset_list_summary <- renderPrint({
			req(ExpSet_list())
			
			expset_list <- ExpSet_list()
			
			cat("═══════════════════════════════════════════════\n")
			cat("EXPRESSIONSET LIST SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Number of ExpressionSets:", length(expset_list), "\n\n")
			
			for (name in names(expset_list)) {
				ExpSet <- expset_list[[name]]
				
				cat("─────────────────────────────────────────────\n")
				cat("Name:", name, "\n")
				cat("Dimensions:", nrow(ExpSet), "features ×", ncol(ExpSet), "samples\n")
				cat("assayData elements:\n")
				assay_names <- Biobase::assayDataElementNames(ExpSet)
				for (assay in assay_names) {
					cat("  •", assay, "\n")
				}
				cat("\n")
			}
			
			cat("═══════════════════════════════════════════════\n")
		})
		
		# Download handler
		output$download_expset_list <- downloadHandler(
			filename = function() {
				req(input$export_filename)
				filename <- input$export_filename
				if (! grepl("\\.rds$", filename, ignore.case = TRUE)) {
					filename <- paste0(filename, ".rds")
				}
				filename
			},
			content = function(file) {
				req(ExpSet_list())
				
				showNotification("Saving ExpressionSet list...", id = "save_progress", duration = NULL)
				
				tryCatch({
					saveRDS(ExpSet_list(), file)
					
					removeNotification("save_progress")
					showNotification(
						"✅ ExpressionSet list saved successfully!",
						type = "message",
						duration = 5
					)
				}, error = function(e) {
					removeNotification("save_progress")
					showNotification(
						paste("❌ Save failed:", e$message),
						type = "error",
						duration = 10
					)
				})
			}
		)
	})
}