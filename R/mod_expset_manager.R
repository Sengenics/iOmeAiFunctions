#' ExpressionSet Manager Module - UI
#'
#' Manage and export ExpressionSet list with ComBat corrections
#'
#' @param id Module namespace ID
#' @export
mod_expset_manager_ui <- function(id) {
	ns <- NS(id)
	
	tagList(
		# Update ExpressionSet with ComBat Data ####
		fluidRow(
			box(
				title = "Add ComBat-Corrected Data to ExpressionSet",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Add ComBat-corrected data from the Batch Correction tab to your ExpressionSet list as a new assayData element."),
				
				fluidRow(
					column(
						width = 4,
						selectInput(
							ns("source_expset"),
							"Source ExpressionSet:",
							choices = NULL
						),
						helpText("The ExpressionSet to add corrected data to")
					),
					column(
						width = 4,
						textInput(
							ns("combat_assay_name"),
							"New assayData Name:",
							value = "",
							placeholder = "e.g., exprs_combat"
						),
						helpText("Name for the ComBat-corrected matrix")
					),
					column(
						width = 4,
						br(),
						checkboxInput(
							ns("add_rc_version"),
							"Also add row-centered version (_RC)",
							value = TRUE
						)
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 12,
						actionButton(
							ns("update_expset"),
							"Add ComBat Data to ExpressionSet",
							icon = icon("plus-circle"),
							class = "btn-success btn-lg",
							style = "width: 100%;"
						)
					)
				),
				
				br(),
				
				uiOutput(ns("update_status"))
			)
		),
		
		# View ExpressionSet List Contents ####
		fluidRow(
			box(
				title = "ExpressionSet List Contents",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("View the contents of your ExpressionSet list, including all assayData elements."),
				
				verbatimTextOutput(ns("expset_list_summary"))
			)
		),
		
		# Export ExpressionSet List ####
		fluidRow(
			box(
				title = "Export ExpressionSet List",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Save the entire ExpressionSet list (including all ComBat-corrected data) as an RDS file."),
				
				fluidRow(
					column(
						width = 6,
						textInput(
							ns("export_filename"),
							"Filename:",
							value = paste0("ExpSet_list_", Sys.Date(), ". rds"),
							placeholder = "ExpSet_list. rds"
						)
					),
					column(
						width = 6,
						br(),
						downloadButton(
							ns("download_expset_list"),
							"Download ExpressionSet List",
							class = "btn-primary btn-lg",
							style = "width: 100%;"
						)
					)
				)
			)
		),
		
		# Export Individual ExpressionSet ####
		fluidRow(
			box(
				title = "Export Individual ExpressionSet",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Export a single ExpressionSet from the list."),
				
				fluidRow(
					column(
						width = 6,
						selectInput(
							ns("export_individual_expset"),
							"Select ExpressionSet:",
							choices = NULL
						)
					),
					column(
						width = 6,
						br(),
						downloadButton(
							ns("download_individual_expset"),
							"Download Selected ExpressionSet",
							class = "btn-default",
							style = "width: 100%;"
						)
					)
				)
			)
		)
	)
}


#' ExpressionSet Manager Module - Server
#'
#' @param id Module namespace ID
#' @param ExpSet_list Reactive returning the ExpressionSet list
#' @param corrected_eset Reactive returning the ComBat-corrected ExpressionSet
#' @param debug Enable debug mode
#' @export
mod_expset_manager_server <- function(id,
																			ExpSet_list,
																			corrected_eset = reactive(NULL),
																			debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Debug UI
		if (isTRUE(debug)) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ExpressionSet Manager Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		# Update ExpressionSet choices ####
		observe({
			req(ExpSet_list())
			
			expset_names <- names(ExpSet_list())
			
			updateSelectInput(session, "source_expset", choices = expset_names)
			updateSelectInput(session, "export_individual_expset", choices = expset_names)
		})
		
		# Auto-generate assay name based on corrected data ####
		observe({
			req(corrected_eset())
			req(input$source_expset)
			
			# Get the batch factors that were corrected from the notes
			notes <- Biobase::notes(corrected_eset())
			
			if (! is.null(notes$combat_correction)) {
				batch_factors <- notes$combat_correction$batch_factors
				
				# Get current assay name from source ExpSet
				source_expset <- ExpSet_list()[[input$source_expset]]
				current_assay <- Biobase::assayDataElementNames(source_expset)[1]
				
				# Generate name
				suggested_name <- paste0(current_assay, "_combat")
				
				updateTextInput(session, "combat_assay_name", value = suggested_name)
			}
		})
		
		# Add ComBat data to ExpressionSet ####
		update_success <- reactiveVal(FALSE)
		
		observeEvent(input$update_expset, {
			req(corrected_eset())
			req(input$combat_assay_name)
			req(input$source_expset)
			req(ExpSet_list())
			
			assay_name <- input$combat_assay_name
			
			# Validate name
			if (assay_name == "" || grepl("^\\s*$", assay_name)) {
				showNotification("⚠️ Please provide a valid assay name", type = "warning", duration = 5)
				return()
			}
			
			showNotification("Adding ComBat data to ExpressionSet...", id = "update_progress", duration = NULL, type = "message")
			
			tryCatch({
				# Get the corrected data
				corrected_ExpSet <- corrected_eset()
				corrected_data <- Biobase::exprs(corrected_ExpSet)
				
				# Get the source ExpressionSet from the list
				expset_list <- ExpSet_list()
				selected_name <- input$source_expset
				
				if (! selected_name %in% names(expset_list)) {
					stop("Selected ExpressionSet not found in list")
				}
				
				original_ExpSet <- expset_list[[selected_name]]
				
				# Add ComBat-corrected data as new assayData element
				Biobase::assayDataElement(original_ExpSet, assay_name) <- corrected_data
				
				added_names <- assay_name
				
				# Optionally add row-centered version
				if (isTRUE(input$add_rc_version)) {
					rc_name <- paste0(assay_name, "_RC")
					corrected_data_rc <- row_scale_function(corrected_data)
					Biobase::assayDataElement(original_ExpSet, rc_name) <- corrected_data_rc
					added_names <- c(added_names, rc_name)
				}
				
				# Update pData with any new columns from correction
				corrected_meta <- Biobase::pData(corrected_ExpSet)
				original_meta <- Biobase::pData(original_ExpSet)
				
				new_cols <- setdiff(colnames(corrected_meta), colnames(original_meta))
				if (length(new_cols) > 0) {
					for (col in new_cols) {
						original_meta[[col]] <- corrected_meta[[col]]
					}
					Biobase::pData(original_ExpSet) <- original_meta
				}
				
				# Update experimentData with correction info
				exp_data <- Biobase::experimentData(original_ExpSet)
				if (is.null(exp_data@other$combat_corrections)) {
					exp_data@other$combat_corrections <- list()
				}
				
				# Get combat info from notes
				combat_notes <- Biobase::notes(corrected_ExpSet)$combat_correction
				
				exp_data@other$combat_corrections[[assay_name]] <- list(
					batch_factors = combat_notes$batch_factors,
					model = combat_notes$model,
					sample_group_preserved = combat_notes$sample_group_preserved,
					par_prior = combat_notes$par_prior,
					row_centered = combat_notes$row_centered,
					correction_date = combat_notes$correction_date,
					assay_names = added_names
				)
				
				Biobase::experimentData(original_ExpSet) <- exp_data
				
				# Update the list
				expset_list[[selected_name]] <- original_ExpSet
				
				# Update the reactive ExpSet_list
				if (is.function(ExpSet_list)) {
					# It's a reactiveVal
					ExpSet_list(expset_list)
				} else {
					warning("ExpSet_list is not a reactiveVal - cannot update")
				}
				
				removeNotification("update_progress")
				
				update_success(TRUE)
				
				showNotification(
					HTML(paste0(
						"✅ ExpressionSet updated successfully! <br>",
						"<strong>ExpressionSet:</strong> ", selected_name, "<br>",
						"<strong>Added assayData elements:</strong><br>",
						paste("  • ", added_names, collapse = "<br>")
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				removeNotification("update_progress")
				update_success(FALSE)
				showNotification(
					paste("❌ Failed to update ExpressionSet:", e$message),
					type = "error",
					duration = 10
				)
				message("Update error: ", e$message)
			})
		})
		
		# Show update status ####
		output$update_status <- renderUI({
			if (update_success()) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" ExpressionSet successfully updated! "),
					p("The ComBat-corrected data has been added as a new assayData element. "),
					p("Next steps:"),
					tags$ul(
						tags$li("View the updated list in the 'ExpressionSet List Contents' section below"),
						tags$li("Export the entire list using the 'Export ExpressionSet List' section"),
						tags$li("Use the new assayData in downstream analyses")
					)
				)
			}
		})
		
		# ExpressionSet list summary ####
		output$expset_list_summary <- renderPrint({
			req(ExpSet_list())
			
			expset_list <- ExpSet_list()
			
			cat("═══════════════════════════════════════════════\n")
			cat("EXPRESSIONSET LIST SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Number of ExpressionSets:", length(expset_list), "\n\n")
			
			for (name in names(expset_list)) {
				ExpSet <- expset_list[[name]]
				
				cat("───────────────────────────────────────────────\n")
				cat("Name:", name, "\n")
				cat("Dimensions:", nrow(ExpSet), "features ×", ncol(ExpSet), "samples\n\n")
				
				cat("assayData elements:\n")
				assay_names <- Biobase::assayDataElementNames(ExpSet)
				for (assay in assay_names) {
					cat("  • ", assay, "\n")
				}
				
				# Show ComBat corrections if any
				exp_data <- Biobase::experimentData(ExpSet)
				if (!is.null(exp_data@other$combat_corrections)) {
					cat("\nComBat corrections applied:\n")
					for (correction_name in names(exp_data@other$combat_corrections)) {
						correction_info <- exp_data@other$combat_corrections[[correction_name]]
						cat("  • ", correction_name, "\n")
						cat("    - Batch factors: ", paste(correction_info$batch_factors, collapse = ", "), "\n")
						cat("    - Model: ", correction_info$model, "\n")
						if (!is.na(correction_info$sample_group_preserved)) {
							cat("    - Preserved group: ", correction_info$sample_group_preserved, "\n")
						}
						cat("    - Date: ", as.character(correction_info$correction_date), "\n")
					}
				}
				
				cat("\n")
			}
			
			cat("═══════════════════════════════════════════════\n")
		})
		
		# Download entire ExpressionSet list ####
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
		
		# Download individual ExpressionSet ####
		output$download_individual_expset <- downloadHandler(
			filename = function() {
				req(input$export_individual_expset)
				paste0(input$export_individual_expset, "_", Sys.Date(), ".rds")
			},
			content = function(file) {
				req(input$export_individual_expset)
				req(ExpSet_list())
				
				selected_expset <- ExpSet_list()[[input$export_individual_expset]]
				saveRDS(selected_expset, file)
				
				showNotification(
					paste("✅ Exported:", input$export_individual_expset),
					type = "message",
					duration = 5
				)
			}
		)
		
		# Return nothing (this module manages state internally)
		return(list())
	})
}