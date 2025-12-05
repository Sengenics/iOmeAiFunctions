#' ExpressionSet Manager Module - UI
#'
#' Manage and export ExpressionSet list with ComBat corrections
#'
#' @param id Module namespace ID
#' @export
mod_expset_manager_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# Debug UI (conditional) ####
		if (isTRUE(debug)) {
			fluidRow(
				box(
					width = 12,
					actionButton(
						ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					)
				)
			)
		},
		# Update ExpressionSet with ComBat Data ####
		fluidRow(
			box(
				title = "Add ComBat-Corrected Data to ExpressionSet(s)",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Add ComBat-corrected data from the Batch Correction tab to your ExpressionSet list as new assayData elements."),
				
				fluidRow(
					column(
						width = 6,
						pickerInput(
							ns("target_assays"),
							"Target ExpressionSet(s):",
							choices = NULL,
							selected = NULL,
							multiple = TRUE,
							options = pickerOptions(
								actionsBox = TRUE,
								selectedTextFormat = "count > 2",
								liveSearch = TRUE,
								title = "Select one or more ExpressionSets to update"
							)
						),
						helpText("Select which ExpressionSet(s) to add the corrected data to.  You can select multiple.")
					),
					column(
						width = 6,
						textInput(
							ns("combat_assay_name"),
							"New assayData Name:",
							value = "",
							placeholder = "e.g., exprs_combat"
						),
						helpText("Name for the ComBat-corrected matrix.  Will be added to all selected ExpressionSets.")
					)
				),
				
				fluidRow(
					column(
						width = 6,
						checkboxInput(
							ns("add_rc_version"),
							"Also add row-centered version (_RC)",
							value = FALSE
						),
						helpText("Adds an additional row-centered version with '_RC' suffix")
					),
					column(
						width = 6,
						checkboxInput(
							ns("add_pn_version"),
							"Also add Pooled Normal version (_PN)",
							value = FALSE
						),
						helpText("Adds version with only Pooled Normal samples")
					)
				),
				
				hr(),
				
				uiOutput(ns("combat_available_check")),
				
				fluidRow(
					column(
						width = 12,
						actionButton(
							ns("update_expset"),
							"Add ComBat Data to Selected ExpressionSet(s)",
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
#' @param assay_choices Reactive returning available assay choices from data module
#' @param corrected_eset Reactive returning the ComBat-corrected ExpressionSet
#' @param sample_group_column Reactive returning the sample group column (for PN filtering)
#' @param debug Enable debug mode
#' @export
mod_expset_manager_server <- function(id,
																			ExpSet_list,
																			selected_batch_factors,
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
		
		# Update assay choices from data module ####
		observe({
			req(ExpSet_list())
			
			choices <- get_expset_assay_names(ExpSet_list())
			selected = c('sample_loess_normalised','clinical_loess_normalised','clinical_loess_normalised_PN')
			
			updatePickerInput(session, "target_assays", choices = choices,
												selected = selected)
			updateSelectInput(session, "export_individual_expset", choices = names(choices))
		})
		
		# # Auto-generate assay name based on corrected data ####
		# observe({
		# 	req(corrected_eset())
		# 	
		# 	# Get the batch factors that were corrected from the notes
		# 	notes <- Biobase::notes(corrected_eset())
		# 	
		# 	if (! is.null(notes$combat_correction)) {
		# 		batch_factors <- notes$combat_correction$batch_factors
		# 		
		# 		# Get first selected assay to determine base name
		# 		if (!is.null(input$target_assays) && length(input$target_assays) > 0) {
		# 			first_assay <- input$target_assays[1]
		# 			# Extract just the assay name (after the " - ")
		# 			base_name <- sub(".*? - ", "", first_assay)
		# 			
		# 			# Generate name
		# 			suggested_name <- paste0(base_name, "_combat")
		# 			
		# 			updateTextInput(session, "combat_assay_name", value = suggested_name)
		# 		}
		# 	}
		# })
		# 
		# # Check if ComBat data is available ####
		# output$combat_available_check <- renderUI({
		# 	if (is.null(corrected_eset()) || is.null(corrected_eset())) {
		# 		div(
		# 			class = "alert alert-warning",
		# 			icon("exclamation-triangle"),
		# 			strong(" No ComBat-corrected data available"),
		# 			p("Please run ComBat correction in the 'Batch Correction' tab first.")
		# 		)
		# 	} else {
		# 		notes <- Biobase::notes(corrected_eset())
		# 		combat_info <- notes$combat_correction
		# 		
		# 		div(
		# 			class = "alert alert-info",
		# 			icon("info-circle"),
		# 			strong(" ComBat-corrected data ready"),
		# 			p(paste("Batch factors:", paste(combat_info$batch_factors, collapse = ", "))),
		# 			p(paste("Model:", combat_info$model))
		# 		)
		# 	}
		# })
		
		# Add ComBat data to ExpressionSet(s) ####
		update_success <- reactiveVal(FALSE)
		update_summary <- reactiveVal(NULL)
		
		observeEvent(input$update_expset, {
			req(corrected_eset())
			req(input$combat_assay_name)
			req(input$target_assays)
			req(ExpSet_list())
			
			assay_name <- input$combat_assay_name
			target_assays <- input$target_assays
			
			# Validate name
			if (assay_name == "" || grepl("^\\s*$", assay_name)) {
				showNotification("⚠️ Please provide a valid assay name", type = "warning", duration = 5)
				return()
			}
			
			if (length(target_assays) == 0) {
				showNotification("⚠️ Please select at least one ExpressionSet to update", type = "warning", duration = 5)
				return()
			}
			
			showNotification(
				paste("Updating", length(target_assays), "ExpressionSet(s)... "),
				id = "update_progress",
				duration = NULL,
				type = "message"
			)
			
			tryCatch({
				# Get the corrected data
				corrected_ExpSet <- corrected_eset()
				corrected_data <- Biobase::exprs(corrected_ExpSet)
				corrected_meta <- Biobase::pData(corrected_ExpSet)
				
				# Get combat info
				combat_notes <- Biobase::notes(corrected_ExpSet)$combat_correction
				
				# Get the list
				expset_list <- ExpSet_list()
				
				# Track what was added
				summary_list <- list()
				
				# Loop through each selected assay
				for (target_assay in target_assays) {
					# Extract ExpressionSet name from format "ExpSetName - assayName"
					expset_name <- sub(" - .*$", "", target_assay)
					
					if (! expset_name %in% names(expset_list)) {
						warning("ExpressionSet not found: ", expset_name)
						next
					}
					
					target_ExpSet <- expset_list[[expset_name]]
					
					# Add ComBat-corrected data as new assayData element
					Biobase::assayDataElement(target_ExpSet, assay_name) <- corrected_data
					
					added_names <- c(assay_name)
					
					# Optionally add row-centered version
					if (isTRUE(input$add_rc_version)) {
						rc_name <- paste0(assay_name, "_RC")
						corrected_data_rc <- row_scale_function(corrected_data)
						Biobase::assayDataElement(target_ExpSet, rc_name) <- corrected_data_rc
						added_names <- c(added_names, rc_name)
					}
					
					# Optionally add PN version
					if (isTRUE(input$add_pn_version) && !is.null(sample_group_column()) && !is.null(sample_group_column())) {
						pn_name <- paste0(assay_name, "_PN")
						
						# Filter for Pooled Normal samples
						sample_groups <- Biobase::pData(target_ExpSet)[[sample_group_column()]]
						is_pn <- grepl("pooled.*normal", sample_groups, ignore.case = TRUE)
						
						if (sum(is_pn) > 0) {
							corrected_data_pn <- corrected_data[, is_pn, drop = FALSE]
							Biobase::assayDataElement(target_ExpSet, pn_name) <- corrected_data_pn
							added_names <- c(added_names, pn_name)
						}
					}
					
					# Update pData with any new columns from correction
					original_meta <- Biobase::pData(target_ExpSet)
					new_cols <- setdiff(colnames(corrected_meta), colnames(original_meta))
					
					if (length(new_cols) > 0) {
						for (col in new_cols) {
							original_meta[[col]] <- corrected_meta[[col]]
						}
						Biobase::pData(target_ExpSet) <- original_meta
					}
					
					# Update experimentData with correction info
					exp_data <- Biobase::experimentData(target_ExpSet)
					if (is.null(exp_data@other$combat_corrections)) {
						exp_data@other$combat_corrections <- list()
					}
					
					exp_data@other$combat_corrections[[assay_name]] <- list(
						batch_factors = combat_notes$batch_factors,
						model = combat_notes$model,
						sample_group_preserved = combat_notes$sample_group_preserved,
						par_prior = combat_notes$par_prior,
						row_centered = combat_notes$row_centered,
						correction_date = combat_notes$correction_date,
						assay_names = added_names
					)
					
					Biobase::experimentData(target_ExpSet) <- exp_data
					
					# Update the list
					expset_list[[expset_name]] <- target_ExpSet
					
					# Track for summary
					summary_list[[expset_name]] <- added_names
				}
				
				# Update the reactive ExpSet_list
				if (is.function(ExpSet_list)) {
					ExpSet_list(expset_list)
				}
				
				removeNotification("update_progress")
				
				update_success(TRUE)
				update_summary(summary_list)
				
				# Build notification message
				summary_html <- paste0(
					"✅ Updated ", length(summary_list), " ExpressionSet(s) successfully! <br><br>"
				)
				
				for (name in names(summary_list)) {
					summary_html <- paste0(
						summary_html,
						"<strong>", name, ":</strong><br>",
						paste("  • ", summary_list[[name]], collapse = "<br>"),
						"<br><br>"
					)
				}
				
				showNotification(
					HTML(summary_html),
					type = "message",
					duration = 15
				)
				
			}, error = function(e) {
				removeNotification("update_progress")
				update_success(FALSE)
				showNotification(
					paste("❌ Failed to update ExpressionSet(s):", e$message),
					type = "error",
					duration = 10
				)
				message("Update error: ", e$message)
			})
		})
		
		# Show update status ####
		output$update_status <- renderUI({
			if (update_success()) {
				summary <- update_summary()
				
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(sprintf(" Successfully updated %d ExpressionSet(s)!  ", length(summary))),
					p("The ComBat-corrected data has been added as new assayData elements. "),
					
					tags$table(
						class = "table table-sm",
						style = "background-color: white; margin-top: 10px;",
						tags$thead(
							tags$tr(
								tags$th("ExpressionSet"),
								tags$th("Added Assays")
							)
						),
						tags$tbody(
							lapply(names(summary), function(name) {
								tags$tr(
									tags$td(tags$strong(name)),
									tags$td(paste(summary[[name]], collapse = ", "))
								)
							})
						)
					),
					
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
					# Highlight ComBat-corrected assays
					if (grepl("combat", assay, ignore.case = TRUE)) {
						cat("  ★ ", assay, " (ComBat-corrected)\n")
					} else {
						cat("  • ", assay, "\n")
					}
				}
				
				# Show ComBat corrections if any
				exp_data <- Biobase::experimentData(ExpSet)
				if (!is.null(exp_data@other$combat_corrections)) {
					cat("\nComBat correction history:\n")
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