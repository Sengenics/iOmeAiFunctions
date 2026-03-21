annotated_violin_UI <- function(id, use_box = FALSE) {
	ns <- NS(id)
	
	ui_content <- tagList(
		#conditionalPanel(
		#	condition = "true",
		tags$div(
			style = "background: yellow; color: black; padding: 8px; font-weight: bold;",
			"annotated_violin_UI loaded"
		),
		
		actionButton(ns('debug_mod_violin'),'Debug 2'),
		# uiOutput(ns("debug_mod_ui")),
		# ),
		fluidRow(
			column(
				width = 3,
				selectInput(
					ns("plot_scale"),
					"Plot Scale:",
					choices = c("log2", "RFU"),
					selected = "log2"
				),
				checkboxGroupInput(
					ns("plot_lines"),
					"Threshold Lines:",
					choices = c(
						"Baseline Median" = "median",
						"FC Threshold" = "fc",
						"2MAD" = "2mad",
						"3MAD" = "3mad",
						"4MAD" = "4mad"
					),
					selected = c("median", "fc", "3mad")
				),
				checkboxGroupInput(
					ns("plot_labels"),
					"Text Overlays:",
					choices = c(
						"Penetrance %" = "penetrance",
						"Global P-values / FDR" = "pvalues",
						"Group P-values" = "group_pvalues"
					),
					selected = c("penetrance", "pvalues")
				),
				
				numericInput(
					ns("violin_ncol"),
					"Plots Per Row:",
					value = 3,
					min = 1,
					max = 6,
					step = 1
				),
				numericInput(
					ns("violin_nrow"),
					"Rows Per Page:",
					value = 3,
					min = 1,
					max = 6,
					step = 1
				),
				numericInput(
					ns("text_size"),
					"Text Size:",
					value = 3,
					min = 2,
					max = 10,
					step = 0.5
				)
			),
			column(
				width = 9,
				uiOutput(ns("plot_ui"))
			)
		)
	)
	
	if (use_box) {
		shinydashboard::box(
			title = "Annotated Violin Plots",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			collapsible = TRUE,
			ui_content
		)
	} else {
		ui_content
	}
}

annotated_violin_Server <- function(id, plot_spec_reactive, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# output$debug_mod_ui <- renderUI({
		# 	#if (isTRUE(debug)) {
		# 		actionButton(session$ns("debug_mod_violin_2"), "Plot Debug 2", class = "btn-warning btn-sm")
		# 	#}
		# })
		
		observeEvent(input$debug_mod_violin, {
			showNotification("debug_mod_violin clicked", type = "message", duration = 3)
			browser()
		})
		
		# observeEvent(input$debug_mod_violin, {
		# 	message("\n========== Annotated Violin DEBUG ==========")
		# 	
		# 	ps <- NULL
		# 	pages <- NULL
		# 	
		# 	ps <- tryCatch(plot_spec_reactive(), error = function(e) e)
		# 	pages <- tryCatch(plot_pages(), error = function(e) e)
		# 	
		# 	message("plot_spec_reactive() class:")
		# 	print(class(ps))
		# 	
		# 	if (!inherits(ps, "error")) {
		# 		message("\nplot_spec_reactive() names:")
		# 		print(names(ps))
		# 		
		# 		message("\nplot_data:")
		# 		if (!is.null(ps$plot_data)) {
		# 			print(dim(ps$plot_data))
		# 			print(colnames(ps$plot_data))
		# 			print(utils::head(ps$plot_data[, intersect(colnames(ps$plot_data),
		# 																								 c("feature", "sample_id", "group", "y_log2", "y_rfu", "facet_label"))]))
		# 		} else {
		# 			print("plot_data is NULL")
		# 		}
		# 		
		# 		message("\nline_annotations:")
		# 		if (!is.null(ps$line_annotations)) {
		# 			print(dim(ps$line_annotations))
		# 			print(utils::head(ps$line_annotations))
		# 		} else {
		# 			print("line_annotations is NULL")
		# 		}
		# 		
		# 		message("\ntext_annotations names:")
		# 		print(names(ps$text_annotations))
		# 		
		# 		message("\nplot_meta:")
		# 		print(ps$plot_meta)
		# 	} else {
		# 		message("\nplot_spec_reactive() error:")
		# 		print(ps)
		# 	}
		# 	
		# 	message("\nCurrent inputs:")
		# 	message("input$plot_scale = ", input$plot_scale)
		# 	message("input$plot_lines = ", paste(input$plot_lines, collapse = ", "))
		# 	message("input$plot_labels = ", paste(input$plot_labels, collapse = ", "))
		# 	message("input$violin_ncol = ", input$violin_ncol)
		# 	message("input$violin_nrow = ", input$violin_nrow)
		# 	
		# 	message("\nplot_pages() status:")
		# 	if (inherits(pages, "error")) {
		# 		print(pages)
		# 	} else {
		# 		print(length(pages))
		# 	}
		# 	
		# 	message("===========================================\n")
		# 	
		# 	browser()
		# })
		
		
		plot_pages <- reactive({
			req(plot_spec_reactive())
			
			plot_spec <- plot_spec_reactive()
			plot_data <- plot_spec$plot_data
			line_annotations <- plot_spec$line_annotations
			text_annotations <- plot_spec$text_annotations
			meta <- plot_spec$plot_meta
			
			req(nrow(plot_data) > 0)
			
			scale_mode <- req(input$plot_scale)
			line_types <- input$plot_lines
			label_types <- input$plot_labels
			violin_ncol <- req(input$violin_ncol)
			violin_nrow <- req(input$violin_nrow)
			
			y_col <- if (identical(scale_mode, "RFU")) "y_rfu" else "y_log2"
			
			line_df <- line_annotations %>%
				dplyr::filter(line_type %in% line_types)
			
			line_df$y_value <- line_df[[y_col]]
			
			line_styles <- tibble::tibble(
				line_type = c("median", "fc", "2mad", "3mad", "4mad"),
				colour = c("grey40", "#1f78b4", "#33a02c", "#d7301f", "#6a3d9a"),
				linetype = c("dashed", "dotdash", "dotted", "solid", "longdash")
			)
			
			line_df <- line_df %>%
				dplyr::left_join(line_styles, by = "line_type")
			
			text_df <- dplyr::bind_rows(text_annotations) %>%
				dplyr::filter(annotation_type %in% label_types)
			
			if (nrow(text_df) > 0) {
				text_df$y_value <- text_df[[y_col]]
			}
			
			group_levels <- unique(as.character(plot_data[[meta$group_col]]))
			n_levels <- length(group_levels)
			
			if (n_levels <= 2) {
				violin_cols <- c("#009E73", "#BEAED4")
			} else if (n_levels == 3) {
				violin_cols <- c("#009E73", "#BEAED4", "#80B1D3")
			} else {
				violin_cols <- scales::hue_pal()(n_levels)
			}
			names(violin_cols) <- group_levels
			
			shape_col <- meta$shape_col
			
			if (!is.null(shape_col) && shape_col %in% colnames(plot_data)) {
				point_mapping <- ggplot2::aes(
					x = .data[[meta$group_col]],
					y = .data[[y_col]],
					color = .data[[meta$group_col]],
					shape = .data[[shape_col]]
				)
			} else {
				point_mapping <- ggplot2::aes(
					x = .data[[meta$group_col]],
					y = .data[[y_col]],
					color = .data[[meta$group_col]]
				)
			}
			
			p = ggplot2::ggplot(plot_data, point_mapping) + 
			
			
			# p <- ggplot2::ggplot(
			# 	plot_data,
			# 	ggplot2::aes(
			# 		x = .data[[meta$group_col]],
			# 		y = .data[[y_col]],
			# 		color = .data[[meta$group_col]]
			# 	)
			# ) +
				ggplot2::geom_violin(alpha = 0.35, trim = FALSE) +
				ggplot2::geom_point(
					position = ggplot2::position_jitter(width = 0.18, height = 0, seed = 1),
					size = 1.2,
					alpha = 0.8
				) +
				ggplot2::scale_color_manual(values = violin_cols) +
				ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
				ggplot2::labs(
					x = NULL,
					y = if (identical(scale_mode, "RFU")) "RFU" else "Log2 Intensity"
				) +
				ggplot2::theme_minimal(base_size = 11) +
				ggplot2::theme(
					legend.position = "none",
					panel.grid.minor = ggplot2::element_blank(),
					strip.text = ggplot2::element_text(face = "bold", size = 9)
				)
			
			if (nrow(line_df) > 0) {
				p <- p +
					ggplot2::geom_hline(
						data = line_df,
						ggplot2::aes(
							yintercept = y_value,
							color = NULL,
							linetype = line_type
						),
						inherit.aes = FALSE,
						linewidth = 0.7
					) +
					ggplot2::scale_linetype_manual(
						values = stats::setNames(line_styles$linetype, line_styles$line_type)
					)
			}
			
			text_size <- req(input$text_size)
			if (nrow(text_df) > 0) {
				p <- p +
					ggplot2::geom_text(
						data = text_df,
						ggplot2::aes(
							x = x,
							y = y_value,
							label = label
						),
						inherit.aes = FALSE,
						color = "black",
						size = text_size
					)
			}
			
			p <- p +
				ggforce::facet_wrap_paginate(
					stats::as.formula(paste("~", meta$facet_col)),
					ncol = violin_ncol,
					nrow = violin_nrow,
					scales = "free_y"
				)
			
			n_pages <- ggforce::n_pages(p)
			
			lapply(seq_len(n_pages), function(i) {
				p + ggforce::facet_wrap_paginate(
					stats::as.formula(paste("~", meta$facet_col)),
					ncol = violin_ncol,
					nrow = violin_nrow,
					page = i,
					scales = "free_y"
				)
			})
		})
		
		output$plot_ui <- renderUI({
			req(plot_pages())
			
			n_pages <- length(plot_pages())
			
			tagList(
				if (n_pages > 1) {
					sliderInput(
						session$ns("plot_page"),
						"Page:",
						min = 1,
						max = n_pages,
						value = 1,
						step = 1,
						width = "100%"
					)
				},
				plotOutput(session$ns("plot"), height = "900px")
			)
		})
		
		output$plot <- renderPlot({
			req(plot_pages())
			
			page_num <- if (!is.null(input$plot_page)) input$plot_page else 1
			plot_pages()[[page_num]]
		})
		
		return(reactive(plot_pages()))
	})
}


mod_simple_violin_UI <- function(id, use_box = FALSE) {
	ns <- NS(id)
	
	ui_content <- tagList(
		
		conditionalPanel(
			condition = "true",
			uiOutput(ns("debug_ui")),
		),
		fluidRow(
			column(
				width = 3,
				
				## Group Variable ####
				selectInput(
					ns("group_var"),
					"Grouping Variable:",
					choices = NULL,
					selected = NULL
				),
				## Proteins ####
				selectizeInput(
					ns("proteins"),
					"Proteins to Plot:",
					choices = NULL,
					selected = NULL,
					multiple = TRUE,
					options = list(placeholder = "Select one or more proteins")
				),
				
				## Group Annotations ####
				checkboxInput(
					ns("show_group_labels"),
					"Show Group Annotations",
					value = FALSE
				),
				
				selectInput(
					ns("group_label_assay"),
					"Group Annotation Assay:",
					choices = c("none"),
					selected = "none"
				),
				
				## Pages ####
				numericInput(
					ns("plot_ncol"),
					"Plots Per Row:",
					value = 3,
					min = 1,
					max = 6,
					step = 1
				),
				
				numericInput(
					ns("plot_nrow"),
					"Rows Per Page:",
					value = 3,
					min = 1,
					max = 6,
					step = 1
				)
			),
			
			column(
				width = 9,
				uiOutput(ns("plot_ui"))
			)
		)
	)
	
	if (use_box) {
		shinydashboard::box(
			title = "Simple Violin Plots",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			collapsible = TRUE,
			ui_content
		)
	} else {
		ui_content
	}
}


mod_simple_violin_Server <- function(id,
																		 eset_reactive,
																		 assay_name,
																		 default_group_var_reactive = reactive(NULL),
																		 default_proteins_reactive = reactive(NULL),
																		 group_eset_reactive = reactive(NULL),
																		 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		output$debug_ui <- renderUI({
			if (debug == TRUE) {
				actionButton(session$ns("debug"), "violin Debug", class = "btn-warning btn-sm")
			}
		})
		observeEvent(input$debug, {
			browser()
		})
		
		
		## Group Annotations ####
		group_eset <- reactive({
			ge <- group_eset_reactive()
			if (is.null(ge)) {
				return(NULL)
			}
			ge
		})
		
		observe({
			ge <- group_eset()
			
			if (is.null(ge)) {
				updateSelectInput(
					session,
					"group_label_assay",
					choices = c("none"),
					selected = "none"
				)
			} else {
				assay_choices <- Biobase::assayDataElementNames(ge)
				updateSelectInput(
					session,
					"group_label_assay",
					choices = c("none", assay_choices),
					selected = "none"
				)
			}
		})
		
		group_label_data <- reactive({
			if (!isTRUE(input$show_group_labels)) {
				return(NULL)
			}
			
			ge <- group_eset()
			if (is.null(ge)) {
				return(NULL)
			}
			
			if (is.null(input$group_label_assay) || identical(input$group_label_assay, "none")) {
				return(NULL)
			}
			
			proteins <- input$proteins
			if (is.null(proteins) || length(proteins) == 0) {
				return(NULL)
			}
			
			assay_name_group <- input$group_label_assay
			available_assays <- Biobase::assayDataElementNames(ge)
			
			if (!assay_name_group %in% available_assays) {
				return(NULL)
			}
			
			mat <- Biobase::assayDataElement(ge, assay_name_group)
			proteins <- intersect(proteins, rownames(mat))
			
			if (length(proteins) == 0) {
				return(NULL)
			}
			
			mat[proteins, , drop = FALSE] %>%
				as.data.frame(check.names = FALSE) %>%
				tibble::rownames_to_column("Protein") %>%
				tidyr::pivot_longer(
					cols = -Protein,
					names_to = "Group",
					values_to = "label_value"
				) %>%
				dplyr::mutate(
					label = as.character(signif(label_value, 3))
				)
		})
		
		
		group_label_plot_data <- reactive({
			gl <- group_label_data()
			if (is.null(gl) || nrow(gl) == 0) {
				return(NULL)
			}
			
			df <- plot_data()
			if (is.null(df) || nrow(df) == 0) {
				return(NULL)
			}
			
			y_max_df <- df %>%
				dplyr::group_by(Protein) %>%
				dplyr::summarise(
					y = max(value, na.rm = TRUE) * 1.08,
					.groups = "drop"
				)
			
			gl %>%
				dplyr::left_join(y_max_df, by = "Protein")
		})
		
		
		
		
		
		
		## observe ####
		observe({
			req(eset_reactive())
			
			eset <- eset_reactive()
			pd_cols <- colnames(Biobase::pData(eset))
			default_group_var <- default_group_var_reactive()
			
			selected_group_var <- if (!is.null(default_group_var) && default_group_var %in% pd_cols) {
				default_group_var
			} else {
				pd_cols[1]
			}
			
			updateSelectInput(
				session,
				"group_var",
				choices = pd_cols,
				selected = selected_group_var
			)
		})
		
		observe({
			req(eset_reactive())
			
			eset <- eset_reactive()
			protein_choices <- rownames(Biobase::assayDataElement(eset, assay_name))
			default_proteins <- default_proteins_reactive()
			
			selected_proteins <- if (!is.null(default_proteins)) {
				intersect(default_proteins, protein_choices)
			} else {
				protein_choices
			}
			
			updateSelectizeInput(
				session,
				"proteins",
				choices = protein_choices,
				selected = selected_proteins,
				server = TRUE
			)
		})
		
		## plot_data ####
		plot_data <- reactive({
			req(eset_reactive(), assay_name, input$group_var, input$proteins)
			
			eset <- eset_reactive()
			group_var <- input$group_var
			proteins <- input$proteins
			
			available_assays <- Biobase::assayDataElementNames(eset)
			req(assay_name %in% available_assays)
			
			pd <- Biobase::pData(eset)
			req(group_var %in% colnames(pd))
			
			mat <- Biobase::assayDataElement(eset, assay_name)
			proteins <- intersect(proteins, rownames(mat))
			req(length(proteins) > 0)
			
			pd$Sample_ID <- rownames(pd)
			
			mat[proteins, , drop = FALSE] %>%
				as.data.frame(check.names = FALSE) %>%
				tibble::rownames_to_column("Protein") %>%
				tidyr::pivot_longer(
					cols = -Protein,
					names_to = "Sample_ID",
					values_to = "value"
				) %>%
				dplyr::left_join(pd, by = "Sample_ID") %>%
				dplyr::mutate(
					Group = .data[[group_var]]
				) %>%
				dplyr::filter(!is.na(Group), Group != "")
		})
		
		## plot_base ####
		plot_base <- reactive({
			req(plot_data(), input$plot_ncol, input$plot_nrow)
			
			df <- plot_data()
			req(nrow(df) > 0)
			
			group_levels <- unique(as.character(df$Group))
			n_levels <- length(group_levels)
			
			if (n_levels <= 2) {
				violin_cols <- c("#009E73", "#BEAED4")
			} else if (n_levels == 3) {
				violin_cols <- c("#009E73", "#BEAED4", "#80B1D3")
			} else {
				violin_cols <- scales::hue_pal()(n_levels)
			}
			
			names(violin_cols) <- group_levels
			
			ggplot2::ggplot(
				df,
				ggplot2::aes(x = Group, y = value, color = Group)
			) +
				ggplot2::geom_violin(alpha = 0.35, trim = FALSE) +
				ggplot2::geom_point(
					position = ggplot2::position_jitter(width = 0.18, height = 0, seed = 1),
					size = 1.2,
					alpha = 0.8
				) +
				ggplot2::scale_color_manual(values = violin_cols) +
				ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
				ggplot2::labs(
					x = NULL,
					y = assay_name
				) +
				ggplot2::theme_minimal(base_size = 11) +
				ggplot2::theme(
					legend.position = "none",
					panel.grid.minor = ggplot2::element_blank(),
					strip.text = ggplot2::element_text(face = "bold", size = 9)
				)
		})
		
		## plot_n_pages ####
		
		plot_n_pages <- reactive({
			req(plot_base(), input$plot_ncol, input$plot_nrow)
			
			ggforce::n_pages(
				plot_base() +
					ggforce::facet_wrap_paginate(
						~ Protein,
						ncol = input$plot_ncol,
						nrow = input$plot_nrow,
						scales = "free_y"
					)
			)
		})
		
		
		
		## plot_pages ####
		# plot_pages <- reactive({
		# 	df <- plot_data()
		# 	req(nrow(df) > 0)
		# 	
		# 	group_levels <- unique(as.character(df$Group))
		# 	n_levels <- length(group_levels)
		# 	
		# 	if (n_levels <= 2) {
		# 		violin_cols <- c("#009E73", "#BEAED4")
		# 	} else if (n_levels == 3) {
		# 		violin_cols <- c("#009E73", "#BEAED4", "#80B1D3")
		# 	} else {
		# 		violin_cols <- scales::hue_pal()(n_levels)
		# 	}
		# 	
		# 	names(violin_cols) <- group_levels
		# 	
		# 	p <- ggplot2::ggplot(
		# 		df,
		# 		ggplot2::aes(x = Group, y = value, color = Group)
		# 	) +
		# 		ggplot2::geom_violin(alpha = 0.35, trim = FALSE) +
		# 		ggplot2::geom_point(
		# 			position = ggplot2::position_jitter(width = 0.18, height = 0, seed = 1),
		# 			size = 1.2,
		# 			alpha = 0.8
		# 		) +
		# 		ggplot2::scale_color_manual(values = violin_cols) +
		# 		ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
		# 		ggplot2::labs(
		# 			x = NULL,
		# 			y = assay_name
		# 		) +
		# 		ggplot2::theme_minimal(base_size = 11) +
		# 		ggplot2::theme(
		# 			legend.position = "none",
		# 			panel.grid.minor = ggplot2::element_blank(),
		# 			strip.text = ggplot2::element_text(face = "bold", size = 9)
		# 		) +
		# 		ggforce::facet_wrap_paginate(
		# 			~ Protein,
		# 			ncol = input$plot_ncol,
		# 			nrow = input$plot_nrow,
		# 			scales = "free_y"
		# 		)
		# 	
		# 	n_pages <- ggforce::n_pages(p)
		# 	
		# 	lapply(seq_len(n_pages), function(i) {
		# 		p + ggforce::facet_wrap_paginate(
		# 			~ Protein,
		# 			ncol = input$plot_ncol,
		# 			nrow = input$plot_nrow,
		# 			page = i,
		# 			scales = "free_y"
		# 		)
		# 	})
		# })
		
		
		## plot_ui ####
		output$plot_ui <- renderUI({
			req(plot_n_pages())
			
			n_pages <- plot_n_pages()
			
			tagList(
				if (n_pages > 1) {
					sliderInput(
						session$ns("plot_page"),
						"Page:",
						min = 1,
						max = n_pages,
						value = 1,
						step = 1,
						width = "100%"
					)
				},
				#plotOutput(session$ns("plot"), height = "900px")
				shinycssloaders::withSpinner(
					plotOutput(session$ns("plot"), height = "900px"),
					type = 4,
					color = "darkblue"
				)
			)
		})
		
		## output$plot ####
		output$plot <- renderPlot({
			req(plot_base(), plot_n_pages(), input$plot_ncol, input$plot_nrow)
			
			page_num <- if (!is.null(input$plot_page)) input$plot_page else 1
			
			p = plot_base() +
				ggforce::facet_wrap_paginate(
					~ Protein,
					ncol = input$plot_ncol,
					nrow = input$plot_nrow,
					page = page_num,
					scales = "free_y"
				)
			
			gl_df <- group_label_plot_data()
			if (!is.null(gl_df) && nrow(gl_df) > 0) {
				p <- p +
					ggplot2::geom_text(
						data = gl_df,
						ggplot2::aes(x = Group, y = y, label = label),
						inherit.aes = FALSE,
						color = "black",
						size = 3
					)
			}
			
			p
		})
		
	})
}




