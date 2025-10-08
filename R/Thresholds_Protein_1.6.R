# Protein Threshold Module
# This module handles the protein threshold visualization

library(plotly)
library(dplyr)

#' Protein Threshold UI Module
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
#' @export
proteinsUI <- function(id) {
	ns <- NS(id)

	fluidPage(
		sidebarLayout(
			sidebarPanel(
				h3("Threshold Parameters"),

				hr(),
				h4("Region 1: Constant # Proteins"),
				numericInput(ns("x1"), "End at Total Proteins:",
										 value = 10, min = 1, max = 100, step = 1),
				numericInput(ns("n_fail_1"), "# Failing Proteins:",
										 value = 1, min = 0.1, max = 10, step = 0.1),

				hr(),
				h4("Region 2: Linear Increase (# Proteins)"),
				numericInput(ns("x2"), "End at Total Proteins:",
										 value = 500, min = 10, max = 1000, step = 10),
				numericInput(ns("n_fail_2"), "# Failing Proteins at End:",
										 value = 25, min = 1, max = 100, step = 1),

				hr(),
				h4("Region 3: Constant # Proteins"),
				numericInput(ns("x3"), "End at Total Proteins:",
										 value = 1000, min = 100, max = 2000, step = 10),
				p("# Failing stays at Region 2 value"),

				hr(),
				h4("Region 4: Constant % Proteins"),
				numericInput(ns("x4"), "End at Total Proteins (or NA for infinite):",
										 value = NA, min = 1000, max = 5000, step = 100),
				numericInput(ns("pct_fail_4"), "Max % Failing (or NA for infinite):",
										 value = NA, min = 0.1, max = 50, step = 0.1),
				numericInput(ns("n_fail_4"), "Max # Failing (or NA for infinite):",
										 value = 50, min = 10, max = 200, step = 5),

				hr(),
				h4("Plot Display Settings"),
				numericInput(ns("pct_max"), "Max % on Y-axis:",
										 value = 20, min = 5, max = 100, step = 1),

				hr(),
				actionButton(ns("reset"), "Reset Thresholds to Defaults",
										 class = "btn-primary btn-block"),

				width = 3
			),

			mainPanel(
				plotlyOutput(ns("threshold_plot"), height = "600px"),
				hr(),
				verbatimTextOutput(ns("debug_info")),
				width = 9
			)
		)
	)
}

#' Protein Threshold Server Module
#'
#' @param id Module namespace ID
#' @param global_preset Reactive value containing preset name
#' @param product_positions Reactive value containing product position data
#' @return Reactive value containing product data
#' @export
proteinsServer <- function(id, global_preset, product_positions) {
	moduleServer(id, function(input, output, session) {

		# Preset configurations
		presets <- list(
			"v2 Thresholds" = list(
				x1 = 10, n_fail_1 = 1,
				x2 = 500, n_fail_2 = 25,
				x3 = 1000,
				x4 = NA, pct_fail_4 = NA, n_fail_4 = 50,
				pct_max = 20
			),
			"2025 Transition" = list(
				x1 = 10, n_fail_1 = 1,
				x2 = 540, n_fail_2 = 27,
				x3 = 1800,
				x4 = NA, pct_fail_4 = NA, n_fail_4 = 50,
				pct_max = 20
			),
			"2023 Thresholds" = list(
				x1 = 120, n_fail_1 = 5,
				x2 = 120, n_fail_2 = 5,
				x3 = 500,
				x4 = NA, pct_fail_4 = 10, n_fail_4 = NA,
				pct_max = 20
			)
		)

		# Observe global preset selection
		observeEvent(global_preset(), {
			preset <- presets[[global_preset()]]
			updateNumericInput(session, "x1", value = preset$x1)
			updateNumericInput(session, "n_fail_1", value = preset$n_fail_1)
			updateNumericInput(session, "x2", value = preset$x2)
			updateNumericInput(session, "n_fail_2", value = preset$n_fail_2)
			updateNumericInput(session, "x3", value = preset$x3)
			updateNumericInput(session, "x4", value = preset$x4)
			updateNumericInput(session, "pct_fail_4", value = preset$pct_fail_4)
			updateNumericInput(session, "n_fail_4", value = preset$n_fail_4)
			updateNumericInput(session, "pct_max", value = preset$pct_max)
		})

		# Reset button observer
		observeEvent(input$reset, {
			preset <- presets[[global_preset()]]
			updateNumericInput(session, "x1", value = preset$x1)
			updateNumericInput(session, "n_fail_1", value = preset$n_fail_1)
			updateNumericInput(session, "x2", value = preset$x2)
			updateNumericInput(session, "n_fail_2", value = preset$n_fail_2)
			updateNumericInput(session, "x3", value = preset$x3)
			updateNumericInput(session, "x4", value = preset$x4)
			updateNumericInput(session, "pct_fail_4", value = preset$pct_fail_4)
			updateNumericInput(session, "n_fail_4", value = preset$n_fail_4)
			updateNumericInput(session, "pct_max", value = preset$pct_max)
		})

		# Calculate # failing proteins for given total proteins
		calculate_n_fail <- function(x_total, x1, n_fail_1, x2, n_fail_2, x3, x4, pct_fail_4, n_fail_4) {
			n_fail <- numeric(length(x_total))

			for (i in seq_along(x_total)) {
				x <- x_total[i]

				if (x <= x1) {
					n_calc <- ceiling(n_fail_1)
					if (!is.na(pct_fail_4)) {
						n_max_from_pct <- ceiling((pct_fail_4 / 100) * x)
						n_fail[i] <- min(n_calc, n_max_from_pct)
					} else {
						n_fail[i] <- n_calc
					}

				} else if (x <= x2) {
					slope <- (n_fail_2 - n_fail_1) / (x2 - x1)
					n_calc <- ceiling(n_fail_1 + slope * (x - x1))
					if (!is.na(pct_fail_4)) {
						n_max_from_pct <- ceiling((pct_fail_4 / 100) * x)
						n_fail[i] <- min(n_calc, n_max_from_pct)
					} else {
						n_fail[i] <- n_calc
					}

				} else if (x <= x3) {
					n_calc <- ceiling(n_fail_2)
					if (!is.na(pct_fail_4)) {
						n_max_from_pct <- ceiling((pct_fail_4 / 100) * x)
						n_fail[i] <- min(n_calc, n_max_from_pct)
					} else {
						n_fail[i] <- n_calc
					}

				} else {
					pct_at_x3 <- (ceiling(n_fail_2) / x3) * 100
					n_from_pct <- ceiling((pct_at_x3 / 100) * x)

					if (!is.na(n_fail_4)) {
						n_fail[i] <- min(n_from_pct, ceiling(n_fail_4))
					} else {
						n_fail[i] <- n_from_pct
					}
				}
			}

			return(n_fail)
		}

		# Generate plot data
		plot_data <- reactive({
			req(product_positions())
			prods <- product_positions()$proteins

			product_vals <- c(prods$pai, prods$cta, prods$iome_cancer,
												prods$iome_discovery, prods$iome_discovery_v2)
			if (!is.na(prods$custom) && !is.null(prods$custom)) {
				product_vals <- c(product_vals, prods$custom)
			}

			product_vals <- product_vals[!is.na(product_vals)]

			# Calculate x_max based on threshold regions
			x_max <- ifelse(!is.na(input$x4) && input$x4 > input$x3,
											input$x4 + 500,
											input$x3 + 1000)

			# Extend to accommodate products
			max_products <- ifelse(length(product_vals) > 0, max(product_vals), 0)
			x_max <- max(x_max, max_products) + 100

			x <- seq(10, x_max, 10)

			n_fail <- calculate_n_fail(x,
																 input$x1, input$n_fail_1,
																 input$x2, input$n_fail_2,
																 input$x3, input$x4, input$pct_fail_4, input$n_fail_4)

			pct_fail <- (n_fail / x) * 100

			data.frame(
				x = x,
				n_fail = n_fail,
				pct_fail = pct_fail
			)
		})

		# Product data
		product_data <- reactive({
			req(product_positions())
			prods <- product_positions()$proteins

			# Safely extract values, converting NULL to NA
			safe_val <- function(x) if(is.null(x)) NA else x

			# Create initial dataframe
			products <- data.frame(
				Product = c("PAI", "CTA", "i-Ome Cancer", "i-Ome Discovery v1", "i-Ome Discovery v2"),
				x_pos = c(safe_val(prods$pai), safe_val(prods$cta), safe_val(prods$iome_cancer),
									safe_val(prods$iome_discovery), safe_val(prods$iome_discovery_v2)),
				stringsAsFactors = FALSE
			)

			# Add custom if present
			custom_val <- safe_val(prods$custom)
			if (!is.na(custom_val)) {
				products <- rbind(products, data.frame(Product = "Custom", x_pos = custom_val, stringsAsFactors = FALSE))
			}

			# Filter out NA positions
			products <- products[!is.na(products$x_pos), ]

			# Calculate thresholds for each product
			if (nrow(products) > 0) {
				products$n_fail <- sapply(products$x_pos, function(x) {
					calculate_n_fail(x,
													 input$x1, input$n_fail_1,
													 input$x2, input$n_fail_2,
													 input$x3, input$x4, input$pct_fail_4, input$n_fail_4)
				})
				products$pct_fail <- (products$n_fail / products$x_pos) * 100
			}

			return(products)
		})

		# Render interactive plot
		output$threshold_plot <- renderPlotly({
			df <- plot_data()
			products <- product_data()

			product_colors <- c('PAI' = '#ef4444',
													'CTA' = '#3b82f6',
													'i-Ome Cancer' = '#f97316',
													'i-Ome Discovery v1' = '#22c55e',
													'i-Ome Discovery v2' = '#a855f7',
													'Custom' = '#fbbf24')

			p <- plot_ly() %>%
				add_trace(
					data = df,
					x = ~x,
					y = ~pct_fail,
					type = 'scatter',
					mode = 'lines',
					name = '% Failing',
					line = list(color = '#8884d8', width = 3),
					yaxis = 'y',
					customdata = ~n_fail,
					hovertemplate = paste(
						'<b style="color:#8884d8">% Failing:</b> %{y:.2f}%<br>',
						'<extra></extra>'
					),
					connectgaps = FALSE
				) %>%
				add_trace(
					data = df,
					x = ~x,
					y = ~n_fail,
					type = 'scatter',
					mode = 'lines',
					name = '# Failing',
					line = list(color = '#82ca9d', width = 3),
					yaxis = 'y2',
					customdata = ~pct_fail,
					hovertemplate = paste(
						'<b style="color:#82ca9d"># Failing:</b> %{y:.0f}<br>',
						'<extra></extra>'
					),
					connectgaps = FALSE
				) %>%
				add_segments(
					x = input$x1, xend = input$x1,
					y = 0, yend = max(df$pct_fail),
					line = list(dash = 'dash', color = 'lightgray', width = 1),
					showlegend = FALSE,
					hoverinfo = 'skip'
				) %>%
				add_segments(
					x = input$x2, xend = input$x2,
					y = 0, yend = max(df$pct_fail),
					line = list(dash = 'dash', color = 'lightgray', width = 1),
					showlegend = FALSE,
					hoverinfo = 'skip'
				) %>%
				add_segments(
					x = input$x3, xend = input$x3,
					y = 0, yend = max(df$pct_fail),
					line = list(dash = 'dash', color = 'lightgray', width = 1),
					showlegend = FALSE,
					hoverinfo = 'skip'
				)

			if (!is.na(input$x4)) {
				p <- p %>%
					add_segments(
						x = input$x4, xend = input$x4,
						y = 0, yend = max(df$pct_fail),
						line = list(dash = 'dash', color = 'lightgray', width = 1),
						showlegend = FALSE,
						hoverinfo = 'skip'
					)
			}

			for (i in 1:nrow(products)) {
				p <- p %>%
					add_trace(
						x = products$x_pos[i],
						y = products$pct_fail[i],
						type = 'scatter',
						mode = 'markers',
						name = products$Product[i],
						yaxis = 'y',
						marker = list(
							size = 12,
							color = product_colors[products$Product[i]],
							line = list(color = 'black', width = 2)
						),
						hovertemplate = paste0(
							'<b>', products$Product[i], '</b><br>',
							'<b>Total Proteins:</b> ', products$x_pos[i], '<br>',
							'<b>% Failing:</b> ', sprintf("%.2f%%", products$pct_fail[i]), '<br>',
							'<b># Failing:</b> ', sprintf("%.0f", products$n_fail[i]), '<br>',
							'<extra></extra>'
						)
					)
			}

			p <- p %>%
				layout(
					title = list(
						text = "Smooth Threshold Transition Across Array Sizes",
						font = list(size = 18)
					),
					xaxis = list(
						title = "Total Proteins on Array",
						gridcolor = '#e5e5e5'
					),
					yaxis = list(
						title = "% Failing Proteins",
						gridcolor = '#e5e5e5',
						side = 'left',
						range = c(0, input$pct_max)
					),
					yaxis2 = list(
						title = "# Failing Proteins",
						overlaying = 'y',
						side = 'right',
						range = c(0, max(df$n_fail) * 1.1),
						showgrid = FALSE,
						fixedrange = FALSE
					),
					hovermode = 'x unified',
					legend = list(
						x = 1.15,
						y = 1
					),
					margin = list(r = 150),
					plot_bgcolor = 'white',
					paper_bgcolor = 'white'
				) %>%
				config(displayModeBar = TRUE, displaylogo = FALSE)

			p
		})

		output$debug_info <- renderText({
			df <- plot_data()
			paste0("Plot range: ", min(df$x), " to ", max(df$x),
						 " (", nrow(df), " points)")
		})

		# Return product data for use in main app
		return(product_data)

	})
}
