# mod_expset_metadata_manager.R ####
# ExpressionSet Metadata Management Module

# UI Function ####

#' ExpressionSet Metadata Manager - UI
#'
#' @param id Character string. Namespace identifier.
#' @export
mod_expset_metadata_manager_ui <- function(id) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Metadata Management",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				## Debug Button ####
				actionButton(ns('metadata_debug'), 'Debug', class = "btn-warning btn-sm"),
				
				p("Manage sample (pData) and feature (fData) metadata across all ExpressionSets."),
				
				## Tab Panel ####
				tabsetPanel(
					id = ns("metadata_tabs"),
					
					### Sample Metadata (pData) ####
					tabPanel(
						"Sample Metadata (pData)",
						icon = icon("users"),
						br(),
						
						#### Current pData ####
						box(
							title = "Current Sample Metadata",
							width = 12,
							status = "info",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = FALSE,
							verbatimTextOutput(ns("pdata_summary"))
						),
																 
						
					
						
						#### Create PN/Clinical Grouping ####
						box(
							title = "Create PN/Clinical Grouping",
							width = 12,
							status = "info",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							p("Create a new variable by assigning existing groups to 'PN' or 'clinical'. Samples not assigned to either will be excluded."),
							
							fluidRow(
								column(
									width = 4,
									textInput(
										ns("pdata_new_var_name"),
										"New Variable Name:",
										value = "PN_clinical"
									),
									helpText("Name for the new grouping variable")
								),
								column(
									width = 4,
									selectInput(
										ns("pdata_source_var"),
										"Source Variable:",
										choices = NULL
									),
									helpText("Select which variable contains the groups to assign")
								),
								column(
									width = 4,
									br(),
									uiOutput(ns("pdata_var_status"))
								)
							),
							
							hr(),
							
							h5("Group Assignment"),
							
							fluidRow(
								column(
									width = 6,
									wellPanel(
										style = "background-color: #d4edda;",
										h5(strong("Pooled Normal (PN)")),
										selectInput(
											ns("pdata_pn_vals"),
											"Select groups to assign as PN:",
											choices = NULL,
											multiple = TRUE
										),
										helpText("Select all groups that should be classified as Pooled Normal samples")
									)
								),
								column(
									width = 6,
									wellPanel(
										style = "background-color: #d1ecf1;",
										h5(strong("Clinical")),
										selectInput(
											ns("pdata_clinical_vals"),
											"Select groups to assign as clinical:",
											choices = NULL,
											multiple = TRUE
										),
										helpText("Select all groups that should be classified as clinical samples")
									)
								)
							),
							
							fluidRow(
								column(
									width = 6,
									actionButton(
										ns("create_pdata_var"),
										"Create Grouping Variable",
										icon = icon("plus"),
										class = "btn-info btn-block btn-lg"
									)
								),
								column(
									width = 6,
									actionButton(
										ns("clear_pdata_var"),
										"Clear",
										icon = icon("times"),
										class = "btn-secondary btn-block"
									)
								)
							),
							
							hr(),
							
							h5("Preview"),
							verbatimTextOutput(ns("pdata_var_preview"))
						),
						
						hr(),
						
						#### Upload Metadata ####
						box(
							title = "Upload External Metadata",
							width = 12,
							status = "warning",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							p("Upload a CSV/TSV file to merge additional metadata. File must contain 'Sample' column matching sample names in your data."),
							
							fluidRow(
								column(6,
											 fileInput(
											 	ns("pdata_upload"),
											 	"Choose CSV/TSV File:",
											 	accept = c(".csv", ".tsv", ".txt")
											 )
								),
								column(6,
											 br(),
											 actionButton(
											 	ns("merge_pdata"),
											 	"Merge Metadata",
											 	icon = icon("merge"),
											 	class = "btn-warning btn-block"
											 )
								)
							),
							
							verbatimTextOutput(ns("pdata_upload_preview"))
						)
					),
					
					### Feature Metadata (fData) ####
					tabPanel(
						"Feature Metadata (fData)",
						icon = icon("dna"),
						br(),
						
						#### Current fData ####
						h4("Current Feature Metadata"),
						verbatimTextOutput(ns("fdata_summary")),
						
						hr(),
						
						#### Upload Metadata ####
						box(
							title = "Upload External Feature Metadata",
							width = 12,
							status = "warning",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = FALSE,
							
							p("Upload a CSV/TSV file to merge additional feature metadata. File must contain 'Protein' column."),
							
							fluidRow(
								column(6,
											 fileInput(
											 	ns("fdata_upload"),
											 	"Choose CSV/TSV File:",
											 	accept = c(".csv", ".tsv", ".txt")
											 )
								),
								column(6,
											 br(),
											 actionButton(
											 	ns("merge_fdata"),
											 	"Merge Metadata",
											 	icon = icon("merge"),
											 	class = "btn-warning btn-block"
											 )
								)
							),
							
							verbatimTextOutput(ns("fdata_upload_preview"))
						)
					),
					
					### Apply Changes ####
					tabPanel(
						"Apply & Save",
						icon = icon("save"),
						br(),
						
						h4("Propagate Changes"),
						
						p("Apply metadata changes to all ExpressionSets in the list and save."),
						
						box(
							title = "Pending Changes",
							width = 12,
							status = "info",
							solidHeader = TRUE,
							
							verbatimTextOutput(ns("pending_changes_summary"))
						),
						
						hr(),
						
						fluidRow(
							column(6,
										 actionButton(
										 	ns("apply_changes"),
										 	"Apply Changes to All ExpressionSets",
										 	icon = icon("sync"),
										 	class = "btn-primary btn-lg btn-block"
										 )
							),
							column(6,
										 downloadButton(
										 	ns("download_updated_expset_list"),
										 	"Download Updated ExpSet_list",
										 	class = "btn-success btn-lg btn-block"
										 )
							)
						),
						
						hr(),
						
						verbatimTextOutput(ns("apply_status"))
					)
				)
			)
		)
	)
}

# Server Function ####

#' ExpressionSet Metadata Manager - Server
#'
#' @param id Character string. Namespace identifier.
#' @param ExpSet_list Reactive. List of ExpressionSets.
#' @param update_ExpSet_list Function. Callback to update ExpSet_list in parent.
#' @param master_eset_name Character. Name of ExpSet with master metadata (default "RawData_ExpSet").
#' @param debug Logical. Enable debug mode (default FALSE).
#'
#' @return Reactive list with updated ExpSet_list
#' @export
mod_expset_metadata_manager_server <- function(id, 
																							 ExpSet_list,
																							 update_ExpSet_list = NULL,
																							 master_eset_name = "RawData_ExpSet",
																							 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		## Debug Handler ####
		if (debug) {
			observeEvent(input$metadata_debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Metadata Manager")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • rv$master_pdata - Master sample metadata")
				message("  • rv$master_fdata - Master feature metadata")
				message("  • rv$pending_pdata_changes - Pending pData changes")
				message("  • rv$pending_fdata_changes - Pending fData changes")
				message("  • rv$updated_expset_list - Updated ExpSet list")
				message("\nPending changes:")
				message("  pData: ", paste(names(rv$pending_pdata_changes), collapse = ", "))
				message("  fData: ", paste(names(rv$pending_fdata_changes), collapse = ", "))
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		## Reactive Values ####
		rv <- reactiveValues(
			master_pdata = NULL,
			master_fdata = NULL,
			pending_pdata_changes = list(),
			pending_fdata_changes = list(),
			updated_expset_list = NULL
		)
		
		## Get Master Metadata ####
		observe({
			req(ExpSet_list())
			
			expset_list <- ExpSet_list()
			
			# Get master ExpressionSet (default to first if master not found)
			if (master_eset_name %in% names(expset_list)) {
				master_eset <- expset_list[[master_eset_name]]
			} else {
				master_eset <- expset_list[[1]]
				warning(paste(master_eset_name, "not found. Using", names(expset_list)[1]))
			}
			
			# Store master metadata
			rv$master_pdata <- Biobase::pData(master_eset)
			rv$master_fdata <- Biobase::fData(master_eset)
			
			# Update UI choices
			pdata_cols <- colnames(rv$master_pdata)
			
			updateSelectInput(session, "pdata_source_var", choices = pdata_cols, selected = "Sample_Group")
		})
		
		## Update Group Value Choices ####
		observe({
			req(rv$master_pdata, input$pdata_source_var)
			
			unique_vals <- sort(unique(rv$master_pdata[[input$pdata_source_var]]))
			
			updateSelectInput(session, "pdata_pn_vals", choices = unique_vals)
			updateSelectInput(session, "pdata_clinical_vals", choices = unique_vals)
		})
		
		## Outputs ####
		
		### pData Summary ####
		output$pdata_summary <- renderPrint({
			req(rv$master_pdata)
			
			cat("═══════════════════════════════════════════════\n")
			cat("SAMPLE METADATA (pData) - Master Source\n")
			cat("═════════════��═════════════════════════════════\n\n")
			cat("Source:", master_eset_name, "\n")
			cat("Samples:", nrow(rv$master_pdata), "\n")
			cat("Variables:", ncol(rv$master_pdata), "\n\n")
			cat("Columns:\n")
			cat(paste("  •", colnames(rv$master_pdata), collapse = "\n"), "\n")
		})
		
		### fData Summary ####
		output$fdata_summary <- renderPrint({
			req(rv$master_fdata)
			
			cat("═══════════════════════════════════════════════\n")
			cat("FEATURE METADATA (fData) - Master Source\n")
			cat("═══════════════════════════════════════════════\n\n")
			cat("Source:", master_eset_name, "\n")
			cat("Features:", nrow(rv$master_fdata), "\n")
			cat("Variables:", ncol(rv$master_fdata), "\n\n")
			cat("Columns:\n")
			cat(paste("  •", colnames(rv$master_fdata), collapse = "\n"), "\n")
		})
		
		## Create PN/Clinical Grouping ####
		observeEvent(input$create_pdata_var, {
			req(rv$master_pdata, input$pdata_new_var_name, input$pdata_source_var)
			req(length(input$pdata_pn_vals) > 0 || length(input$pdata_clinical_vals) > 0)
			
			tryCatch({
				new_var_name <- input$pdata_new_var_name
				source_var <- input$pdata_source_var
				
				# Create new grouping variable
				new_col <- rep(NA_character_, nrow(rv$master_pdata))
				
				# Assign PN
				if (length(input$pdata_pn_vals) > 0) {
					pn_idx <- rv$master_pdata[[source_var]] %in% input$pdata_pn_vals
					new_col[pn_idx] <- "PN"
				}
				
				# Assign clinical
				if (length(input$pdata_clinical_vals) > 0) {
					clinical_idx <- rv$master_pdata[[source_var]] %in% input$pdata_clinical_vals
					new_col[clinical_idx] <- "clinical"
				}
				
				# Convert to factor
				new_col <- factor(new_col, levels = c("PN", "clinical"))
				
				# Store pending change
				rv$pending_pdata_changes[[new_var_name]] <- new_col
				
				# Count assignments
				n_pn <- sum(new_col == "PN", na.rm = TRUE)
				n_clinical <- sum(new_col == "clinical", na.rm = TRUE)
				n_excluded <- sum(is.na(new_col))
				
				showNotification(
					HTML(paste0(
						"<strong>✅ Variable created!</strong><br>",
						"Variable: <b>", new_var_name, "</b><br>",
						"PN samples: ", n_pn, "<br>",
						"Clinical samples: ", n_clinical, "<br>",
						"Excluded: ", n_excluded, "<br>",
						"Go to 'Apply & Save' tab to update all ExpressionSets"
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				showNotification(
					paste("❌ Failed to create variable:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
		## Clear Variable ####
		observeEvent(input$clear_pdata_var, {
			updateSelectInput(session, "pdata_pn_vals", selected = character(0))
			updateSelectInput(session, "pdata_clinical_vals", selected = character(0))
			
			# Remove from pending if exists
			var_name <- input$pdata_new_var_name
			if (var_name %in% names(rv$pending_pdata_changes)) {
				rv$pending_pdata_changes[[var_name]] <- NULL
			}
			
			showNotification("Cleared selections", type = "message", duration = 3)
		})
		
		### pData Variable Status ####
		output$pdata_var_status <- renderUI({
			if (length(rv$pending_pdata_changes) > 0) {
				tags$div(
					style = "color: green; font-weight: bold; margin-top: 30px;",
					icon("check-circle"),
					paste(length(rv$pending_pdata_changes), "variable(s) pending")
				)
			} else {
				tags$div(
					style = "color: grey; margin-top: 30px;",
					"No pending changes"
				)
			}
		})
		
		### pData Variable Preview ####
		output$pdata_var_preview <- renderPrint({
			if (length(rv$pending_pdata_changes) > 0) {
				cat("═══════════════════════════════════════════════\n")
				cat("PENDING GROUPING VARIABLES\n")
				cat("═══════════════════════════════════════════════\n\n")
				
				for (var_name in names(rv$pending_pdata_changes)) {
					cat("Variable:", var_name, "\n")
					cat("───────────────────────────────────────────────\n")
					
					var_data <- rv$pending_pdata_changes[[var_name]]
					
					cat("Distribution:\n")
					print(table(var_data, useNA = "ifany"))
					
					cat("\n")
					
					# Show which original groups went where
					if (!is.null(input$pdata_source_var) && input$pdata_source_var %in% colnames(rv$master_pdata)) {
						cat("Group Assignments from '", input$pdata_source_var, "':\n", sep = "")
						
						assignment_table <- table(
							Original = rv$master_pdata[[input$pdata_source_var]],
							New = var_data,
							useNA = "ifany"
						)
						
						print(assignment_table)
					}
					
					cat("\n═══════════════════════════════════════════════\n\n")
				}
			} else {
				cat("No pending changes.\n\n")
				cat("Select groups for PN and clinical, then click 'Create Grouping Variable'.")
			}
		})
		
		## Upload pData ####
		observeEvent(input$merge_pdata, {
			req(input$pdata_upload)
			
			tryCatch({
				# Read file
				file_path <- input$pdata_upload$datapath
				
				if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
					uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
				} else {
					uploaded_data <- read.delim(file_path, stringsAsFactors = FALSE)
				}
				
				# Validate
				if (!"Sample" %in% colnames(uploaded_data)) {
					stop("Uploaded file must contain 'Sample' column")
				}
				
				# Store each new column as pending change
				new_cols <- setdiff(colnames(uploaded_data), c("Sample", colnames(rv$master_pdata)))
				
				if (length(new_cols) == 0) {
					stop("No new columns found in uploaded file")
				}
				
				# Create a data frame with Sample column for merging
				master_with_sample <- rv$master_pdata
				master_with_sample$Sample <- rownames(master_with_sample)
				
				for (col in new_cols) {
					# Merge with master pdata
					merged <- master_with_sample %>%
						left_join(uploaded_data %>% select(Sample, !!sym(col)), by = "Sample")
					
					merged_col <- merged[[col]]
					
					rv$pending_pdata_changes[[col]] <- merged_col
				}
				
				showNotification(
					HTML(paste0(
						"<strong>✅ Metadata uploaded!</strong><br>",
						"Added ", length(new_cols), " new column(s): ",
						paste(new_cols, collapse = ", "), "<br>",
						"Go to 'Apply & Save' tab to update all ExpressionSets"
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				showNotification(
					paste("❌ Upload failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
		### pData Upload Preview ####
		output$pdata_upload_preview <- renderPrint({
			req(input$pdata_upload)
			
			file_path <- input$pdata_upload$datapath
			
			tryCatch({
				if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
					preview_data <- read.csv(file_path, stringsAsFactors = FALSE, nrows = 10)
				} else {
					preview_data <- read.delim(file_path, stringsAsFactors = FALSE, nrows = 10)
				}
				
				cat("═══════════════════════════════════════════════\n")
				cat("FILE PREVIEW (first 10 rows)\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Columns:", paste(colnames(preview_data), collapse = ", "), "\n")
				cat("Rows in file:", nrow(read.csv(file_path)), "\n\n")
				print(preview_data)
				
			}, error = function(e) {
				cat("Error reading file:", e$message, "\n")
			})
		})
		
		## Upload fData ####
		observeEvent(input$merge_fdata, {
			req(input$fdata_upload)
			
			tryCatch({
				file_path <- input$fdata_upload$datapath
				
				if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
					uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
				} else {
					uploaded_data <- read.delim(file_path, stringsAsFactors = FALSE)
				}
				
				if (!"Protein" %in% colnames(uploaded_data)) {
					stop("Uploaded file must contain 'Protein' column")
				}
				
				new_cols <- setdiff(colnames(uploaded_data), c("Protein", colnames(rv$master_fdata)))
				
				if (length(new_cols) == 0) {
					stop("No new columns found in uploaded file")
				}
				
				# Create a data frame with Protein column for merging
				master_with_protein <- rv$master_fdata
				master_with_protein$Protein <- rownames(master_with_protein)
				
				for (col in new_cols) {
					merged <- master_with_protein %>%
						left_join(uploaded_data %>% select(Protein, !!sym(col)), by = "Protein")
					
					merged_col <- merged[[col]]
					
					rv$pending_fdata_changes[[col]] <- merged_col
				}
				
				showNotification(
					HTML(paste0(
						"<strong>✅ Feature metadata uploaded!</strong><br>",
						"Added ", length(new_cols), " new column(s): ",
						paste(new_cols, collapse = ", "), "<br>",
						"Go to 'Apply & Save' tab to update all ExpressionSets"
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				showNotification(
					paste("❌ Upload failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
		### fData Upload Preview ####
		output$fdata_upload_preview <- renderPrint({
			req(input$fdata_upload)
			
			file_path <- input$fdata_upload$datapath
			
			tryCatch({
				if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
					preview_data <- read.csv(file_path, stringsAsFactors = FALSE, nrows = 10)
				} else {
					preview_data <- read.delim(file_path, stringsAsFactors = FALSE, nrows = 10)
				}
				
				cat("═══════════════════════════════════════════════\n")
				cat("FILE PREVIEW (first 10 rows)\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Columns:", paste(colnames(preview_data), collapse = ", "), "\n")
				cat("Rows in file:", nrow(read.csv(file_path)), "\n\n")
				print(preview_data)
				
			}, error = function(e) {
				cat("Error reading file:", e$message, "\n")
			})
		})
		
		## Pending Changes Summary ####
		output$pending_changes_summary <- renderPrint({
			cat("═══════════════════════════════════════════════\n")
			cat("PENDING METADATA CHANGES\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			if (length(rv$pending_pdata_changes) > 0) {
				cat("Sample Metadata (pData) Changes:\n")
				for (col_name in names(rv$pending_pdata_changes)) {
					n_values <- sum(!is.na(rv$pending_pdata_changes[[col_name]]))
					cat("  • ", col_name, " (", n_values, " values)\n", sep = "")
				}
				cat("\n")
			} else {
				cat("No pending pData changes\n\n")
			}
			
			if (length(rv$pending_fdata_changes) > 0) {
				cat("Feature Metadata (fData) Changes:\n")
				for (col_name in names(rv$pending_fdata_changes)) {
					n_values <- sum(!is.na(rv$pending_fdata_changes[[col_name]]))
					cat("  • ", col_name, " (", n_values, " values)\n", sep = "")
				}
				cat("\n")
			} else {
				cat("No pending fData changes\n\n")
			}
			
			if (length(rv$pending_pdata_changes) == 0 && length(rv$pending_fdata_changes) == 0) {
				cat("⚠️ No changes to apply.\n")
				cat("Create grouping variables or upload metadata files first.\n")
			} else {
				cat("✅ Ready to apply changes to all ExpressionSets.\n")
			}
		})
		
		## Apply Changes ####
		observeEvent(input$apply_changes, {
			req(ExpSet_list())
			req(length(rv$pending_pdata_changes) > 0 || length(rv$pending_fdata_changes) > 0)
			
			showNotification("Applying changes to all ExpressionSets...", id = "apply_progress", duration = NULL)
			
			tryCatch({
				expset_list <- ExpSet_list()
				
				# Apply to each ExpressionSet
				for (eset_name in names(expset_list)) {
					eset <- expset_list[[eset_name]]
					
					# Add pData columns
					if (length(rv$pending_pdata_changes) > 0) {
						pdata <- Biobase::pData(eset)
						
						for (col_name in names(rv$pending_pdata_changes)) {
							# Match by Sample rownames
							pdata[[col_name]] <- rv$pending_pdata_changes[[col_name]][match(rownames(pdata), rownames(rv$master_pdata))]
						}
						
						Biobase::pData(eset) <- pdata
					}
					
					# Add fData columns
					if (length(rv$pending_fdata_changes) > 0) {
						fdata <- Biobase::fData(eset)
						
						for (col_name in names(rv$pending_fdata_changes)) {
							# Match by Protein/feature rownames
							fdata[[col_name]] <- rv$pending_fdata_changes[[col_name]][match(rownames(fdata), rownames(rv$master_fdata))]
						}
						
						Biobase::fData(eset) <- fdata
					}
					
					expset_list[[eset_name]] <- eset
				}
				
				# Store updated list
				rv$updated_expset_list <- expset_list
				
				# Update parent if callback provided
				if (!is.null(update_ExpSet_list)) {
					update_ExpSet_list(expset_list)
				}
				
				# Clear pending changes
				rv$pending_pdata_changes <- list()
				rv$pending_fdata_changes <- list()
				
				removeNotification("apply_progress")
				showNotification(
					HTML("<strong>✅ Changes applied to all ExpressionSets!</strong><br>Download updated list to save, or continue analysis."),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				removeNotification("apply_progress")
				showNotification(
					paste("❌ Apply failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)
			})
		})
		
		### Apply Status ####
		output$apply_status <- renderPrint({
			if (!is.null(rv$updated_expset_list)) {
				cat("═══════════════════════════════════════════════\n")
				cat("✅ CHANGES SUCCESSFULLY APPLIED\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Updated ExpressionSets:\n")
				for (name in names(rv$updated_expset_list)) {
					eset <- rv$updated_expset_list[[name]]
					cat("  • ", name, " (", nrow(eset), " × ", ncol(eset), ")\n", sep = "")
				}
				cat("\n")
				cat("You can now:\n")
				cat("  1. Download the updated ExpSet_list using the button above\n")
				cat("  2. Continue analysis using the new metadata columns\n")
			} else {
				cat("No changes applied yet.\n\n")
				cat("Click 'Apply Changes to All ExpressionSets' when ready.\n")
			}
		})
		
		## Download Updated ExpSet_list ####
		output$download_updated_expset_list <- downloadHandler(
			filename = function() {
				paste0("ExpSet_list_updated_", Sys.Date(), ".rds")
			},
			content = function(file) {
				req(rv$updated_expset_list)
				
				saveRDS(rv$updated_expset_list, file)
				
				showNotification(
					"✅ Updated ExpressionSet list downloaded!",
					type = "message",
					duration = 5
				)
			}
		)
		
		## Return Values ####
		return(reactive({
			list(
				updated_expset_list = rv$updated_expset_list,
				pending_pdata_changes = rv$pending_pdata_changes,
				pending_fdata_changes = rv$pending_fdata_changes
			)
		}))
	})
}