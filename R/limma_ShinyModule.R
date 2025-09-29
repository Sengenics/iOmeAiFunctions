#' Limma Contrast Selection UI Module
#'
#' UI component for selecting variables, building contrasts, and configuring limma analysis
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   limmaContrastUI("limma1")
#' )
#' }
limmaContrastUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             selectInput(ns("variable"),
                         "Select Comparison Variable:",
                         choices = NULL)
      )
    ),

    # Case/Control selection
    fluidRow(
      column(6,
             selectInput(ns("control_group"),
                         "Control Group:",
                         choices = NULL)
      ),
      column(6,
             selectInput(ns("case_group"),
                         "Case Group:",
                         choices = NULL)
      )
    ),

    # Comparison display
    fluidRow(
      column(12,
             h4("Comparison Setup"),
             verbatimTextOutput(ns("contrast_formula"))
      )
    ),

    # Covariate selection
    fluidRow(
      column(6,
             selectInput(ns("covariate1"),
                         "Covariate 1 (optional):",
                         choices = c("None" = ""))
      ),
      column(6,
             selectInput(ns("covariate2"),
                         "Covariate 2 (optional):",
                         choices = c("None" = ""))
      )
    ),

    # Analysis parameters
    fluidRow(
      column(6,
             numericInput(ns("p_val"), "P-value threshold:",
                          value = 0.05, min = 0, max = 1, step = 0.01)
      ),
      column(6,
             numericInput(ns("fc_cut"), "Fold change cutoff:",
                          value = 1.1, min = 1, step = 0.1)
      )
    ),

    fluidRow(
      column(6,
             checkboxInput(ns("eb_trend"), "eBayes trend", value = TRUE)
      ),
      column(6,
             checkboxInput(ns("eb_robust"), "eBayes robust", value = TRUE)
      )
    ),

    # Sample distribution summary
    fluidRow(
      column(12,
             h4("Sample Distribution"),
             tableOutput(ns("group_summary"))
      )
    ),

    # Run button
    fluidRow(
      column(12,
             actionButton(ns("run_limma"),
                          "Run Limma Analysis",
                          class = "btn-primary",
                          width = "100%")
      )
    )
  )
}


#' Limma Contrast Selection Server Module
#'
#' Server logic for contrast specification and limma configuration
#'
#' @param id Character string, namespace ID matching the UI
#' @param eSet Reactive expression returning an ExpressionSet object
#' @param feature_select Reactive expression returning character vector of features to analyze
#'
#' @return Reactive expression returning limma analysis results
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import limma
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   results <- limmaContrastServer("limma1",
#'                                   reactive(my_eset),
#'                                   reactive(selected_features))
#' }
#' }
limmaContrastServer <- function(id, eSet, feature_select) {
  moduleServer(id, function(input, output, session) {

    # Extract metadata from ExpressionSet
    metadata <- reactive({
      req(eSet())
      pData(eSet())
    })

    # Update variable choices and case/control selection
    observe({
      req(metadata())
      choices <- colnames(metadata())

      # Set "Labels" as default if it exists, otherwise use first choice
      default_variable <- if("Labels" %in% choices) "Labels" else choices[1]
      updateSelectInput(session, "variable", choices = choices, selected = default_variable)

      # Update covariate choices
      covariate_choices <- c("None" = "", choices)

      # Set "PSA_class" as default for covariate1 if it exists
      default_covariate1 <- if("PSA_class" %in% choices) "PSA_class" else ""
      updateSelectInput(session, "covariate1", choices = covariate_choices, selected = default_covariate1)

      updateSelectInput(session, "covariate2", choices = covariate_choices)
    })

    # Update case/control choices when variable changes
    observe({
      req(input$variable, metadata())

      var_data <- metadata()[[input$variable]]
      levels_available <- unique(as.character(var_data[!is.na(var_data)]))

      # Auto-detect control group (contains "control" or "healthy")
      control_candidates <- levels_available[grepl("control|healthy", levels_available, ignore.case = TRUE)]
      default_control <- if(length(control_candidates) > 0) control_candidates[1] else levels_available[1]

      # Case group is everything else
      case_candidates <- setdiff(levels_available, default_control)
      default_case <- if(length(case_candidates) > 0) case_candidates[1] else levels_available[2]

      updateSelectInput(session, "control_group",
                        choices = levels_available,
                        selected = default_control)

      updateSelectInput(session, "case_group",
                        choices = levels_available,
                        selected = default_case)
    })

    # Display current comparison
    output$contrast_formula <- renderText({
      req(input$control_group, input$case_group)

      if(input$control_group == input$case_group) {
        "Error: Case and Control groups cannot be the same"
      } else {
        paste("Comparison:", input$case_group, "vs", input$control_group, "(reference)")
      }
    })

    # Group summary table
    output$group_summary <- renderTable({
      req(input$variable, metadata(), input$control_group, input$case_group)

      var_data <- metadata()[[input$variable]]
      summary_df <- as.data.frame(table(var_data))
      colnames(summary_df) <- c(input$variable, "N")

      # Add group assignment column
      summary_df$Assignment <- ifelse(summary_df[[input$variable]] == input$control_group, "Control",
                                      ifelse(summary_df[[input$variable]] == input$case_group, "Case", "Excluded"))

      summary_df
    }, rownames = FALSE)

    # Run limma analysis when button clicked
    # Run limma analysis when button clicked
    limma_results <- eventReactive(input$run_limma, {
      req(eSet(), feature_select(), input$control_group, input$case_group)

      # Validation
      if(input$control_group == input$case_group) {
        showNotification("Case and Control groups cannot be the same", type = "error")
        return(NULL)
      }

      withProgress(message = 'Running limma analysis...', {
        tryCatch({
          # Step 1: Get your original data
          eset_temp <- eSet()
          metadata_temp <- pData(eset_temp)

          # Step 2: Filter to only the samples you want (autoimmune + healthy)
          keep_samples <- metadata_temp[[input$variable]] %in% c(input$case_group, input$control_group)
          eset_filtered <- eset_temp[, keep_samples]

          # Step 3: Clean up the factor levels (THIS IS THE KEY FIX)
          # Remove any "ghost" factor levels that don't exist anymore
          pData(eset_filtered)[[input$variable]] <- factor(
            pData(eset_filtered)[[input$variable]],
            levels = c(input$control_group, input$case_group)
          )

          # Step 4: Clean up covariate factors too (PSA_class)
          if(!is.null(input$covariate1) && input$covariate1 != "") {
            pData(eset_filtered)[[input$covariate1]] <- droplevels(pData(eset_filtered)[[input$covariate1]])
          }

          if(!is.null(input$covariate2) && input$covariate2 != "") {
            pData(eset_filtered)[[input$covariate2]] <- droplevels(pData(eset_filtered)[[input$covariate2]])
          }

          # Step 5: Now run limma on the clean data
          results <- limma_analysis(
            eSet = eset_filtered,
            variable = input$variable,
            feature_select = feature_select(),
            covariate1 = if(input$covariate1 == "") NULL else input$covariate1,
            covariate2 = if(input$covariate2 == "") NULL else input$covariate2,
            user_contrast = NULL,
            EB_trend = input$eb_trend,
            EB_robust = input$eb_robust,
            FC_cut = input$fc_cut,
            p_val = input$p_val
          )

          # Step 6: Show results
          if(!is.null(results) && length(results$sig_features) > 0) {
            sig_table <- results$topTable[results$sig_features, ]
            n_upregulated <- sum(sig_table$logFC > 0)
            n_downregulated <- sum(sig_table$logFC < 0)
            total_sig <- length(results$sig_features)

            showNotification(
              paste0("Analysis completed! Found ", total_sig, " significant features: ",
                     n_upregulated, " higher in ", input$case_group, ", ",
                     n_downregulated, " higher in ", input$control_group),
              type = "message",
              duration = 10
            )
          } else {
            showNotification("Analysis completed - no significant features found", type = "warning")
          }

          return(results)

        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          NULL
        })
      })
    })

    return(limma_results)
  })
}


#' Limma Volcano Plot UI Module
#'
#' UI component for interactive volcano plot of limma results
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaVolcanoUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(3,
						 numericInput(ns("max_labels"), "Max labels:",
						 						 value = 20, min = 0, max = 100)
			),
			column(3,
						 numericInput(ns("point_size"), "Point size:",
						 						 value = 2, min = 0.5, max = 5, step = 0.5)
			),
			column(3,
						 selectInput(ns("color_palette"), "Color palette:",
						 						choices = c("Dark2", "Set1", "Set2", "Paired"))
			),
			column(3,
						 downloadButton(ns("download_plot"), "Download", class = "btn-sm")
			)
		),
		plotOutput(ns("volcano_plot"), height = "600px")
	)
}


#' Limma Volcano Plot Server Module
#'
#' Server logic for generating interactive volcano plots
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import ggplot2
#' @import ggrepel
#' @export
limmaVolcanoServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {

		volcano_plot <- reactive({
			req(limma_results())

			results <- limma_results()
			TT <- results$topTable
			p_val <- results$p_threshold
			FC_cut <- results$fc_threshold
			max_labels <- input$max_labels

			# Prepare data
			sigs_ordered <- TT[order(TT$P.Value),]
			sigs_ordered$genelabels <- sigs_ordered$P.Value < p_val &
				abs(sigs_ordered$logFC) > log2(FC_cut)
			sigs_ordered$threshold <- sigs_ordered$genelabels
			sigs_ordered$symbol <- rownames(sigs_ordered)

			# Limit number of labels
			if(sum(sigs_ordered$genelabels) > max_labels){
				label_idx <- which(sigs_ordered$genelabels)[1:max_labels]
				sigs_ordered$genelabels <- FALSE
				sigs_ordered$genelabels[label_idx] <- TRUE
			}

			# Create plot
			p <- ggplot(sigs_ordered) +
				geom_point(aes(x = logFC, y = -log10(P.Value), colour = threshold),
									 size = input$point_size) +
				scale_color_brewer(palette = input$color_palette) +
				geom_text_repel(aes(x = logFC, y = -log10(P.Value),
														label = ifelse(genelabels, symbol, "")),
												max.overlaps = 50) +
				geom_vline(xintercept = c(-log2(FC_cut), log2(FC_cut)),
									 linetype = "dashed", alpha = 0.5) +
				geom_hline(yintercept = -log10(p_val),
									 linetype = "dashed", alpha = 0.5) +
				ggtitle("Differential Expression Volcano Plot") +
				xlab("log2 fold change") +
				ylab("-log10 p-value") +
				theme_bw() +
				theme(legend.position = "none",
							plot.title = element_text(size = rel(1.5), hjust = 0.5),
							axis.title = element_text(size = rel(1.25)))

			return(p)
		})

		output$volcano_plot <- renderPlot({
			volcano_plot()
		})

		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("volcano_plot_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				ggsave(file, volcano_plot(), width = 10, height = 7)
			}
		)
	})
}


#' Limma Heatmap UI Module
#'
#' UI component for interactive heatmap of significant features
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaHeatmapUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(3,
						 checkboxInput(ns("row_center"), "Row-center data", value = FALSE)
			),
			column(3,
						 checkboxInput(ns("cluster_cols"), "Cluster columns", value = FALSE)
			),
			column(3,
						 checkboxInput(ns("show_rownames"), "Show row names", value = TRUE)
			),
			column(3,
						 downloadButton(ns("download_plot"), "Download", class = "btn-sm")
			)
		),
		fluidRow(
			column(12,
						 uiOutput(ns("add_anno_ui"))
			)
		),
		fluidRow(
			column(12,
						 plotOutput(ns("heatmap_plot"), height = "800px")
			)
		)
	)
}


#' Limma Heatmap Server Module
#'
#' Server logic for generating interactive heatmaps
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import pheatmap
#' @export
limmaHeatmapServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {

		# UI for additional annotations
		output$add_anno_ui <- renderUI({
			req(limma_results())

			ns <- session$ns
			metadata <- limma_results()$metadata
			variable <- limma_results()$variable

			# Get other columns for annotation
			other_cols <- setdiff(colnames(metadata), variable)

			if(length(other_cols) > 0){
				selectInput(ns("add_anno"),
										"Additional annotations:",
										choices = c("None" = "", other_cols),
										multiple = TRUE)
			}
		})

		# Prepare plot data
		plot_data_prep <- reactive({
			req(limma_results())

			add_anno <- if(is.null(input$add_anno) || input$add_anno == "") {
				NULL
			} else {
				input$add_anno
			}

			tryCatch({
				prepare_limma_plot_data(
					limma_results = limma_results(),
					add_anno = add_anno,
					row_center = input$row_center
				)
			}, error = function(e) {
				showNotification(paste("Error preparing plot data:", e$message), type = "error")
				NULL
			})
		})

		heatmap_plot <- reactive({
			req(plot_data_prep())

			plot_prep <- plot_data_prep()

			# Create heatmap
			p = pheatmap(plot_prep$plot_data,
							 annotation_col = plot_prep$plot_metadata,
							 annotation_colors = plot_prep$annotation_colors,
							 cluster_cols = input$cluster_cols,
							 cluster_rows = TRUE,
							 show_rownames = input$show_rownames,
							 show_colnames = FALSE,
							 gaps_col = if(!input$cluster_cols) plot_prep$gap_col else NULL,
							 main = "Significant Features Heatmap")
			return(p)
		})

		output$heatmap_plot <- renderPlot({
			req(heatmap_plot())
			print(heatmap_plot())
		})

		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("heatmap_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				pdf(file, width = 12, height = 10)
				print(heatmap_plot())
				dev.off()
			}
		)
	})
}


#' Limma Violin Plot UI Module
#'
#' UI component for violin plots of significant features
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaViolinUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             radioButtons(ns("protein_selection"), "Protein Selection:",
                          choices = c("Significant all" = "sig_all",
                                      "Significant up" = "sig_up",
                                      "Significant down" = "sig_down",
                                      "Manual significant" = "manual_sig",
                                      "Manual all" = "manual_all"),
                          selected = "sig_all")
      ),
      column(8,
             conditionalPanel(
               condition = paste0("input['", ns("protein_selection"), "'] == 'manual_sig' || input['", ns("protein_selection"), "'] == 'manual_all'"),
               selectizeInput(ns("selected_proteins"), "Select Proteins:",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Choose proteins..."))
             )
      )
    ),

    # NEW ROW for color selection
    fluidRow(
      column(12,
             selectInput(ns("color_variable"), "Color points by:",
                         choices = NULL,
                         selected = NULL)
      )
    ),

    fluidRow(
      column(3,
             numericInput(ns("plots_per_page"), "Plots per page:",
                          value = 9, min = 1, max = 50)
      ),
      column(3,
             numericInput(ns("n_col"), "Columns:",
                          value = 3, min = 1, max = 6)
      ),
      column(3,
             checkboxInput(ns("free_scales"), "Free scales", value = TRUE)
      ),
      column(3,
             downloadButton(ns("download_plot"), "Download", class = "btn-sm")
      )
    ),

    # ... rest of the UI remains the same
    conditionalPanel(
      condition = paste0("input['", ns("protein_selection"), "'] == 'sig_all' || input['", ns("protein_selection"), "'] == 'sig_up' || input['", ns("protein_selection"), "'] == 'sig_down'"),
      fluidRow(
        column(12, style = "text-align: center;",
               actionButton(ns("prev_page"), "◀ Previous", class = "btn-sm"),
               span(" Page ", textOutput(ns("page_info"), inline = TRUE), " "),
               actionButton(ns("next_page"), "Next ▶", class = "btn-sm")
        )
      )
    ),

    uiOutput(ns("violin_plot_ui"))
  )
}

#' Limma Violin Plot Server Module
#'
#' Server logic for generating violin plots
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import ggplot2
#' @import ggforce
#' @import reshape2
#' @export
limmaViolinServer <- function(id, limma_results) {
  moduleServer(id, function(input, output, session) {

    # Current page reactive
    current_page <- reactiveVal(1)

    # Update color variable choices when results change
    observe({
      req(limma_results())
      results <- limma_results()

      # Get all column names from metadata
      metadata_cols <- colnames(results$metadata)

      # Create choices: always include "flag" option, plus all metadata columns
      color_choices <- c("flag", metadata_cols)
      color_choices <- unique(color_choices)  # Remove duplicates if flag is in metadata

      # Always default to "flag"
      updateSelectInput(session, "color_variable",
                        choices = color_choices,
                        selected = "flag")
    })

    # Update protein choices when results change
    observe({
      req(limma_results())
      results <- limma_results()

      if(input$protein_selection == "manual_sig") {
        updateSelectizeInput(session, "selected_proteins",
                             choices = results$sig_features)
      } else if(input$protein_selection == "manual_all") {
        updateSelectizeInput(session, "selected_proteins",
                             choices = rownames(results$topTable))
      }
    })

    # Get proteins to display based on selection mode
    proteins_to_display <- reactive({
      req(limma_results())
      results <- limma_results()

      if(input$protein_selection == "manual_sig") {
        req(input$selected_proteins)
        return(input$selected_proteins)
      } else if(input$protein_selection == "manual_all") {
        req(input$selected_proteins)
        return(input$selected_proteins)
      } else {
        if(input$protein_selection == "sig_all") {
          all_proteins <- results$sig_features
        } else if(input$protein_selection == "sig_up") {
          sig_table <- results$topTable[results$sig_features, ]
          all_proteins <- rownames(sig_table[sig_table$logFC > 0, ])
        } else if(input$protein_selection == "sig_down") {
          sig_table <- results$topTable[results$sig_features, ]
          all_proteins <- rownames(sig_table[sig_table$logFC < 0, ])
        }

        start_idx <- (current_page() - 1) * input$plots_per_page + 1
        end_idx <- min(start_idx + input$plots_per_page - 1, length(all_proteins))

        if(start_idx <= length(all_proteins)) {
          return(all_proteins[start_idx:end_idx])
        } else {
          return(character(0))
        }
      }
    })

    # Calculate total pages
    total_pages <- reactive({
      req(limma_results())
      results <- limma_results()

      if(input$protein_selection %in% c("sig_all", "sig_up", "sig_down")) {
        if(input$protein_selection == "sig_all") {
          total_proteins <- length(results$sig_features)
        } else if(input$protein_selection == "sig_up") {
          sig_table <- results$topTable[results$sig_features, ]
          total_proteins <- sum(sig_table$logFC > 0)
        } else if(input$protein_selection == "sig_down") {
          sig_table <- results$topTable[results$sig_features, ]
          total_proteins <- sum(sig_table$logFC < 0)
        }
        return(ceiling(total_proteins / input$plots_per_page))
      } else {
        return(1)
      }
    })

    # Page navigation
    observeEvent(input$prev_page, {
      if(current_page() > 1) {
        current_page(current_page() - 1)
      }
    })

    observeEvent(input$next_page, {
      if(current_page() < total_pages()) {
        current_page(current_page() + 1)
      }
    })

    # Reset to page 1 when plots per page or protein selection changes
    observeEvent(c(input$plots_per_page, input$protein_selection), {
      current_page(1)
    })

    # Page info display
    output$page_info <- renderText({
      paste(current_page(), "of", total_pages())
    })

    # Dynamic plot height based on number of proteins
    plot_height <- reactive({
      n_proteins <- length(proteins_to_display())
      if(n_proteins == 0) return(400)

      n_rows <- ceiling(n_proteins / input$n_col)
      base_height <- 200
      height_per_row <- 150
      max(400, base_height + (n_rows * height_per_row))
    })

    # Dynamic UI for plot height
    output$violin_plot_ui <- renderUI({
      ns <- session$ns
      plotOutput(ns("violin_plot"), height = paste0(plot_height(), "px"))
    })

    violin_plot <- reactive({
      req(limma_results())
      req(length(proteins_to_display()) > 0)
      req(input$color_variable)

      results <- limma_results()
      selected_features <- proteins_to_display()

      # Prepare data
      meta_df <- results$metadata
      meta_df$Sample <- rownames(meta_df)

      merge_df <- meta_df %>%
        left_join(
          results$expression %>%
            as.data.frame() %>%
            rownames_to_column('feature') %>%
            gather(key = Sample, value = value, -1)) %>%
        left_join(
          results$cv %>%
            as.data.frame() %>%
            rownames_to_column('feature') %>%
            gather(key = Sample, value = cv, -1)) %>%
        left_join(
          results$flag %>%
            as.data.frame() %>%
            rownames_to_column('feature') %>%
            gather(key = Sample, value = flag, -1)) %>%
        filter(feature %in% selected_features)

      # Define colors
      violin_cols <- c("#009E73", "#BEAED4", "#80B1D3", "goldenrod2",
                       "coral2", "palevioletred2")
      color_select <- violin_cols[1:nlevels(as.factor(results$metadata[, results$variable]))]

      # Check if the selected color variable exists in the dataframe
      if(!(input$color_variable %in% colnames(merge_df))) {
        showNotification(paste("Column", input$color_variable, "not found in data"), type = "warning")
        return(NULL)
      }

      # Create plot
      p <- ggplot(merge_df, aes(x = .data[[results$variable]], y = value)) +
        geom_violin(aes(fill = .data[[results$variable]]), alpha = 0.25) +
        scale_fill_manual(values = color_select) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        theme_minimal() +
        geom_point(aes(col = .data[[input$color_variable]]),
                   position = position_jitter(seed = 1, width = 0.2), size = 2) +
        theme(legend.position = "top") +
        facet_wrap(~ feature,
                   ncol = input$n_col,
                   scales = if(input$free_scales) "free" else "fixed")

      return(p)
    })

    output$violin_plot <- renderPlot({
      req(violin_plot())
      violin_plot()
    })

    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("violin_plot_", input$protein_selection, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(limma_results())
        req(input$color_variable)

        if(input$protein_selection %in% c("manual_sig", "manual_all")) {
          req(violin_plot())
          ggsave(file, violin_plot(),
                 width = 12,
                 height = plot_height() / 100,
                 limitsize = FALSE)
        } else {
          results <- limma_results()

          if(input$protein_selection == "sig_all") {
            all_features <- results$sig_features
          } else if(input$protein_selection == "sig_up") {
            sig_table <- results$topTable[results$sig_features, ]
            all_features <- rownames(sig_table[sig_table$logFC > 0, ])
          } else if(input$protein_selection == "sig_down") {
            sig_table <- results$topTable[results$sig_features, ]
            all_features <- rownames(sig_table[sig_table$logFC < 0, ])
          }

          if(length(all_features) == 0) return()

          meta_df <- results$metadata
          meta_df$Sample <- rownames(meta_df)

          merge_df <- meta_df %>%
            left_join(
              results$expression %>%
                as.data.frame() %>%
                rownames_to_column('feature') %>%
                gather(key = Sample, value = value, -1)) %>%
            left_join(
              results$cv %>%
                as.data.frame() %>%
                rownames_to_column('feature') %>%
                gather(key = Sample, value = cv, -1)) %>%
            left_join(
              results$flag %>%
                as.data.frame() %>%
                rownames_to_column('feature') %>%
                gather(key = Sample, value = flag, -1)) %>%
            filter(feature %in% all_features)

          violin_cols <- c("#009E73", "#BEAED4", "#80B1D3", "goldenrod2",
                           "coral2", "palevioletred2")
          color_select <- violin_cols[1:nlevels(as.factor(results$metadata[, results$variable]))]

          p_all <- ggplot(merge_df, aes(x = .data[[results$variable]], y = value)) +
            geom_violin(aes(fill = .data[[results$variable]]), alpha = 0.25) +
            scale_fill_manual(values = color_select) +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            theme_minimal() +
            geom_point(aes(col = .data[[input$color_variable]]),
                       position = position_jitter(seed = 1, width = 0.2), size = 2) +
            theme(legend.position = "top") +
            facet_wrap(~ feature,
                       ncol = input$n_col,
                       scales = if(input$free_scales) "free" else "fixed")

          n_proteins <- length(all_features)
          n_rows <- ceiling(n_proteins / input$n_col)
          base_height <- 3
          height_per_row <- 2
          total_height <- base_height + (n_rows * height_per_row)
          final_height <- min(total_height, 48)

          ggsave(file, p_all,
                 width = 12,
                 height = final_height,
                 limitsize = FALSE)
        }
      }
    )

  })
}

#' Limma Results Summary UI Module
#'
#' UI component for displaying limma results summary and tables
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaResultsUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(12,
						 h3("Analysis Summary"),
						 verbatimTextOutput(ns("summary_text"))
			)
		),
		fluidRow(
			column(12,
						 h4("Design Matrix"),
						 verbatimTextOutput(ns("design_matrix"))
			)
		),
		fluidRow(
			column(12,
						 h4("Top Results"),
						 numericInput(ns("n_top"), "Show top N features:",
						 						 value = 50, min = 10, max = 500, step = 10),
						 DT::dataTableOutput(ns("top_results_table"))
			)
		),
		fluidRow(
			column(12,
						 downloadButton(ns("download_results"), "Download Full Results")
			)
		)
	)
}


#' Limma Results Summary Server Module
#'
#' Server logic for displaying limma results summary
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import DT
#' @export
limmaResultsServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {

		# Summary text
		output$summary_text <- renderPrint({
			req(limma_results())

			results <- limma_results()

			cat("Limma Analysis Results\n")
			cat("======================\n\n")
			cat("Variable:", results$variable, "\n")
			cat("Total features analyzed:", nrow(results$topTable), "\n")
			cat("Significant features (p <", results$p_threshold,
					"& |FC| >", results$fc_threshold, "):",
					length(results$sig_features), "\n\n")

			cat("Group Information:\n")
			for(i in seq_along(results$contrast_info$groups)){
				cat("  ", results$contrast_info$groups[i], ": ",
						results$contrast_info$n_per_group[i], "samples\n")
			}

			if(!is.null(results$contrast_info$contrast_matrix)){
				cat("\nContrast Matrix:\n")
				print(results$contrast_info$contrast_matrix)
			}
		})

		# Design matrix
		output$design_matrix <- renderPrint({
			req(limma_results())
			print(limma_results()$design)
		})

		# Top results table
		output$top_results_table <- DT::renderDataTable({
			req(limma_results())

			TT <- limma_results()$topTable
			TT_display <- head(TT, input$n_top)

			# Format for display
			TT_display$logFC <- round(TT_display$logFC, 3)
			TT_display$AveExpr <- round(TT_display$AveExpr, 3)
			TT_display$t <- round(TT_display$t, 3)
			TT_display$P.Value <- formatC(TT_display$P.Value, format = "e", digits = 2)
			TT_display$adj.P.Val <- formatC(TT_display$adj.P.Val, format = "e", digits = 2)

			DT::datatable(
				TT_display,
				options = list(
					scrollX = TRUE,
					pageLength = 25
				),
				rownames = TRUE
			)
		})

		# Download handler
		output$download_results <- downloadHandler(
			filename = function() {
				paste0("limma_results_", Sys.Date(), ".csv")
			},
			content = function(file) {
				req(limma_results())
				write.csv(limma_results()$topTable, file, row.names = TRUE)
			}
		)
	})
}
