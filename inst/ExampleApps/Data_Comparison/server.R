# Server logic for File Comparison App

server <- function(input, output, session) {
	
	# Debug button (only visible in RStudio)
	output$debug_ui <- renderUI({
		if (debug == TRUE) {
			actionButton('debug', 'Debug', class = "btn-warning btn-sm")
		}
	})
	
	observeEvent(input$debug, {
		browser()
	})
	
	# Reactive values to store loaded data
	data_file1 <- reactiveVal(NULL)
	data_file2 <- reactiveVal(NULL)
	comparison_results <- reactiveVal(NULL)
	
	# Load File 1
	observeEvent(input$file1, {
		req(input$file1)
		
		loaded <- load_data_file(input$file1$datapath, input$file1$name)
		data_file1(loaded)
		
		if (!is.null(loaded)) {
			output$file1_status <- renderText({
				paste("✓ File loaded successfully:", loaded$file_name)
			})
		} else {
			output$file1_status <- renderText({
				"✗ Error loading file"
			})
		}
	})
	
	# Load File 2
	observeEvent(input$file2, {
		req(input$file2)
		
		loaded <- load_data_file(input$file2$datapath, input$file2$name)
		data_file2(loaded)
		
		if (!is.null(loaded)) {
			output$file2_status <- renderText({
				paste("✓ File loaded successfully:", loaded$file_name)
			})
		} else {
			output$file2_status <- renderText({
				"✗ Error loading file"
			})
		}
	})
	
	# File 1 Summary
	output$file1_summary <- renderPrint({
		req(data_file1())
		d <- data_file1()
		cat("File:", d$file_name, "\n")
		cat("Proteins:", d$n_proteins, "\n")
		cat("Samples:", d$n_samples, "\n")
		cat("Protein Column:", d$protein_col, "\n")
		cat("Sample Columns:", paste(d$sample_cols, collapse = ", "), "\n")
	})
	
	# File 2 Summary
	output$file2_summary <- renderPrint({
		req(data_file2())
		d <- data_file2()
		cat("File:", d$file_name, "\n")
		cat("Proteins:", d$n_proteins, "\n")
		cat("Samples:", d$n_samples, "\n")
		cat("Protein Column:", d$protein_col, "\n")
		cat("Sample Columns:", paste(d$sample_cols, collapse = ", "), "\n")
	})
	
	# Display File 1 Data
	output$file1_table <- renderDT({
		req(data_file1())
		datatable(
			data_file1()$data,
			options = list(
				scrollX = TRUE,
				pageLength = 25,
				dom = 'Bfrtip'
			),
			filter = "top",
			rownames = FALSE
		)
	})
	
	# Display File 2 Data
	output$file2_table <- renderDT({
		req(data_file2())
		datatable(
			data_file2()$data,
			options = list(
				scrollX = TRUE,
				pageLength = 25,
				dom = 'Bfrtip'
			),
			filter = "top",
			rownames = FALSE
		)
	})
	
	# Run comparison when both files are loaded
	observe({
		req(data_file1(), data_file2())
		
		comparison <- compare_files(data_file1(), data_file2())
		comparison_results(comparison)
	})
	
	# Value boxes
	output$common_proteins_box <- renderValueBox({
		req(comparison_results())
		comp <- comparison_results()
		
		valueBox(
			value = comp$n_common,
			subtitle = "Common Proteins",
			icon = icon("check-circle"),
			color = "green"
		)
	})
	
	output$unique_file1_box <- renderValueBox({
		req(comparison_results())
		comp <- comparison_results()
		
		valueBox(
			value = comp$n_unique_file1,
			subtitle = paste("Unique to", data_file1()$file_name),
			icon = icon("file"),
			color = "blue"
		)
	})
	
	output$unique_file2_box <- renderValueBox({
		req(comparison_results())
		comp <- comparison_results()
		
		valueBox(
			value = comp$n_unique_file2,
			subtitle = paste("Unique to", data_file2()$file_name),
			icon = icon("file"),
			color = "orange"
		)
	})
	
	# Comparison summary
	output$comparison_summary <- renderPrint({
		req(comparison_results())
		comp <- comparison_results()
		
		cat("File 1:", data_file1()$file_name, "\n")
		cat("  Proteins:", data_file1()$n_proteins, "\n\n")
		
		cat("File 2:", data_file2()$file_name, "\n")
		cat("  Proteins:", data_file2()$n_proteins, "\n\n")
		
		cat("Common proteins:", comp$n_common, "\n")
		cat("Unique to File 1:", comp$n_unique_file1, "\n")
		cat("Unique to File 2:", comp$n_unique_file2, "\n")
		cat("Overlap percentage:", comp$percent_overlap, "%\n")
	})
	
	# Venn diagram
	output$venn_diagram <- renderPlot({
		req(comparison_results())
		comp <- comparison_results()
		
		# Simple venn diagram using base graphics
		par(mar = c(2, 2, 2, 2))
		plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
				 xlab = "", ylab = "", axes = FALSE)
		
		# Draw circles
		symbols(c(3.5, 6.5), c(5, 5), circles = c(2.5, 2.5), 
						inches = FALSE, add = TRUE, fg = c("blue", "orange"), lwd = 3)
		
		# Add labels
		text(2.5, 5, comp$n_unique_file1, cex = 2, font = 2)
		text(5, 5, comp$n_common, cex = 2, font = 2)
		text(7.5, 5, comp$n_unique_file2, cex = 2, font = 2)
		
		text(2.5, 8.5, "File 1\nOnly", cex = 1.2, col = "blue", font = 2)
		text(7.5, 8.5, "File 2\nOnly", cex = 1.2, col = "orange", font = 2)
		text(5, 1.5, "Common", cex = 1.2, col = "darkgreen", font = 2)
	})
	
	# Common proteins table
	output$common_proteins_table <- renderDT({
		req(comparison_results())
		comp <- comparison_results()
		req(comp$n_common > 0)
		
		datatable(
			data.frame(Protein = comp$common_proteins),
			options = list(pageLength = 25),
			rownames = FALSE
		)
	})
	
	# Unique to file 1 table
	output$unique_file1_table <- renderDT({
		req(comparison_results())
		comp <- comparison_results()
		req(comp$n_unique_file1 > 0)
		
		datatable(
			data.frame(Protein = comp$unique_to_file1),
			options = list(pageLength = 25),
			rownames = FALSE
		)
	})
	
	# Unique to file 2 table
	output$unique_file2_table <- renderDT({
		req(comparison_results())
		comp <- comparison_results()
		req(comp$n_unique_file2 > 0)
		
		datatable(
			data.frame(Protein = comp$unique_to_file2),
			options = list(pageLength = 25),
			rownames = FALSE
		)
	})
	
	# Update scatter plot dropdowns
	observe({
		req(comparison_results())
		comp <- comparison_results()
		
		if (!is.null(comp$merged_data)) {
			file1_cols <- grep("_file1$", names(comp$merged_data), value = TRUE)
			file2_cols <- grep("_file2$", names(comp$merged_data), value = TRUE)
			
			updateSelectInput(session, "scatter_x", choices = file1_cols)
			updateSelectInput(session, "scatter_y", choices = file2_cols)
		}
	})
	
	# Correlation heatmap
	# Fixed 600x600 square version:
	
	output$correlation_heatmap <- renderPlotly({
		req(comparison_results(), data_file1(), data_file2())
		
		cor_results <- calculate_correlation(comparison_results(), data_file1(), data_file2())
		
		if (is.null(cor_results) || nrow(cor_results) == 0) {
			return(NULL)
		}
		
		# Create matrix for heatmap
		cor_matrix <- cor_results %>%
			tidyr::pivot_wider(names_from = sample_file2, values_from = correlation) %>%
			tibble::column_to_rownames("sample_file1") %>%
			as.matrix()
		
		plot_ly(
			x = colnames(cor_matrix),
			y = rownames(cor_matrix),
			z = cor_matrix,
			type = "heatmap",
			colors = colorRamp(c("blue", "white", "red")),
			zmin = -1,
			zmax = 1
		) %>%
			layout(
				title = "Sample Correlation Between Files",
				xaxis = list(
					title = "File 2 Samples",
					scaleanchor = "y",
					scaleratio = 1
				),
				yaxis = list(
					title = "File 1 Samples"
				),
				width = 1200,
				height = 1200,
				margin = list(l = 150, r = 50, t = 80, b = 150)
			)
	})
	# output$correlation_heatmap <- renderPlotly({
	# 	req(comparison_results(), data_file1(), data_file2())
	# 	
	# 	cor_results <- calculate_correlation(comparison_results(), data_file1(), data_file2())
	# 	
	# 	if (is.null(cor_results) || nrow(cor_results) == 0) {
	# 		return(NULL)
	# 	}
	# 	
	# 	# Create matrix for heatmap
	# 	cor_matrix <- cor_results %>%
	# 		tidyr::pivot_wider(names_from = sample_file2, values_from = correlation) %>%
	# 		tibble::column_to_rownames("sample_file1") %>%
	# 		as.matrix()
	# 	
	# 	plot_ly(
	# 		x = colnames(cor_matrix),
	# 		y = rownames(cor_matrix),
	# 		z = cor_matrix,
	# 		type = "heatmap",
	# 		colors = colorRamp(c("blue", "white", "red")),
	# 		zmin = -1,
	# 		zmax = 1
	# 	) %>%
	# 		layout(
	# 			title = "Sample Correlation Between Files",
	# 			xaxis = list(title = "File 2 Samples"),
	# 			yaxis = list(title = "File 1 Samples")
	# 		)
	# })
	
	# Scatter plot
	output$scatter_plot <- renderPlotly({
		req(comparison_results())
		req(input$scatter_x, input$scatter_y)
		
		comp <- comparison_results()
		
		if (is.null(comp$merged_data)) {
			return(NULL)
		}
		
		merged <- comp$merged_data
		protein_col <- data_file1()$protein_col
		
		plot_ly(
			data = merged,
			x = ~get(input$scatter_x),
			y = ~get(input$scatter_y),
			text = ~get(protein_col),
			type = "scatter",
			mode = "markers",
			marker = list(size = 8, opacity = 0.6)
		) %>%
			layout(
				title = paste(input$scatter_x, "vs", input$scatter_y),
				xaxis = list(title = input$scatter_x),
				yaxis = list(title = input$scatter_y),
				hovermode = "closest"
			)
	})
	
	
	# Add after the comparison_summary output:
	
	# Identity check
	identity_results <- reactive({
		req(comparison_results(), data_file1(), data_file2())
		check_data_identity(comparison_results(), data_file1(), data_file2())
	})
	
	output$identity_check <- renderPrint({
		req(identity_results())
		id_res <- identity_results()
		
		cat("═══════════════════════════════════════\n")
		cat("         DATA IDENTITY CHECK\n")
		cat("═══════════════════════════════════════\n\n")
		
		if (id_res$is_identical) {
			cat("Result: ✅ IDENTICAL\n")
			cat("All values match exactly.\n")
		} else if (id_res$is_near_identical) {
			cat("Result: ⚠️  NEARLY IDENTICAL\n")
			cat("Values are within tolerance.\n")
		} else {
			cat("Result: ❌ NOT IDENTICAL\n")
			cat("Significant differences detected.\n")
		}
		
		if (length(id_res$differences) > 0) {
			cat("\n───────────────────────────────────────\n")
			cat("Differences Summary:\n")
			cat("───────────────────────────────────────\n")
			for (sample in names(id_res$differences)) {
				diff <- id_res$differences[[sample]]
				cat("\n", sample, ":\n", sep = "")
				cat("  Max difference:  ", round(diff$max_difference, 6), "\n")
				cat("  Mean difference: ", round(diff$mean_difference, 6), "\n")
				cat("  Correlation:     ", round(diff$correlation, 6), "\n")
				cat("  Different values:", diff$n_differences, 
						"(", diff$percent_different, "%)\n")
			}
		}
	})
	
	output$identity_explanations <- renderUI({
		req(identity_results())
		id_res <- identity_results()
		
		# Convert explanations to HTML
		html_text <- paste(id_res$explanations, collapse = "<br>")
		html_text <- gsub("✅", "<span style='color: green; font-weight: bold;'>✅</span>", html_text)
		html_text <- gsub("❌", "<span style='color: red; font-weight: bold;'>❌</span>", html_text)
		html_text <- gsub("⚠️", "<span style='color: orange; font-weight: bold;'>⚠️</span>", html_text)
		
		HTML(paste0("<div style='font-family: monospace; white-space: pre-wrap;'>", 
								html_text, 
								"</div>"))
	})
}