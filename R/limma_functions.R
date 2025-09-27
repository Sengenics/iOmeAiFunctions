#' Run Limma Differential Expression Analysis
#'
#' Performs differential expression analysis using limma on an ExpressionSet object.
#' Supports optional covariates, custom contrasts, and returns comprehensive results
#' including design matrix, top table, and significant features.
#'
#' @param eSet An ExpressionSet object with expression data and phenoData
#' @param variable Character string, column name in pData(eSet) for the main comparison variable
#' @param feature_select Character vector of feature names to analyze. Required.
#' @param covariate1 Character string, optional first covariate column name from pData(eSet)
#' @param covariate2 Character string, optional second covariate column name from pData(eSet)
#' @param user_contrast Optional contrast matrix created with makeContrasts()
#' @param EB_trend Logical, use eBayes trend parameter (default TRUE)
#' @param EB_robust Logical, use eBayes robust parameter for robustifying (default TRUE)
#' @param FC_cut Numeric, fold change cutoff threshold (default 1.1)
#' @param p_val Numeric, p-value threshold for significance (default 0.05)
#'
#' @return A list containing:
#' \describe{
#'   \item{design}{Design matrix used in the linear model}
#'   \item{topTable}{Full limma results table with all features}
#'   \item{sig_features}{Character vector of significant feature names (p < p_val & |FC| > FC_cut)}
#'   \item{fit}{eBayes fit object from limma}
#'   \item{metadata}{Processed metadata (phenoData) used in analysis}
#'   \item{expression}{Expression matrix used in analysis}
#'   \item{variable}{Name of the main comparison variable}
#'   \item{contrast_info}{List with contrast details (groups, sample sizes, contrast matrix)}
#'   \item{p_threshold}{P-value threshold used}
#'   \item{fc_threshold}{Fold change threshold used}
#'   \item{sig_data}{Expression data for significant features only}
#'   \item{sig_data_centered}{Row-centered expression data for significant features}
#' }
#'
#' @details
#' The function handles:
#' \itemize{
#'   \item Automatic conversion of non-syntactic variable names using make.names()
#'   \item Removal of samples with NA values in specified variables/covariates
#'   \item Design matrix construction with or without covariates
#'   \item Custom contrast specification or automatic coefficient selection
#'   \item Feature filtering and sorting by p-value
#' }
#'
#' For robust variance estimation, see Phipson et al. (2016) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5373812/
#'
#' @note
#' Version 1.0.0
#' File: limma_function.R
#'
#' @import limma
#' @import Biobase
#' @import dplyr
#' @import tibble
#' @importFrom stats model.matrix
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage without contrasts
#' results <- limma_analysis(
#'   eSet = my_eset,
#'   variable = "Sample_Group",
#'   feature_select = selected_features
#' )
#'
#' # With custom contrast
#' my_contrast <- makeContrasts("case-control", levels = c("case", "control"))
#' results <- limma_analysis(
#'   eSet = my_eset,
#'   variable = "Sample_Group",
#'   feature_select = selected_features,
#'   user_contrast = my_contrast
#' )
#'
#' # With covariates
#' results <- limma_analysis(
#'   eSet = my_eset,
#'   variable = "Sample_Group",
#'   feature_select = selected_features,
#'   covariate1 = "age",
#'   covariate2 = "gender"
#' )
#' }
limma_analysis <- function(eSet, 
													 variable, 
													 feature_select = NULL,
													 covariate1 = NULL,
													 covariate2 = NULL,
													 user_contrast = NULL,
													 EB_trend = TRUE, 
													 EB_robust = TRUE, 
													 FC_cut = 1.1, 
													 p_val = 0.05) {
	
	# Validate inputs
	if(is.null(feature_select)) {
		stop("please specify filtered feature list as feature_select")
	}
	
	# Extract metadata and expression data
	metadata <- pData(eSet)
	input <- exprs(eSet)
	
	# Make syntactically valid names for non-numeric variables
	if(!is.numeric(metadata[,variable]) & !is.integer(metadata[,variable])){
		metadata[,variable][!is.na(metadata[,variable])] <- 
			make.names(metadata[,variable][!is.na(metadata[,variable])])
	}
	if(!is.null(covariate1) & !is.numeric(metadata[,covariate1]) & !is.integer(metadata[,covariate1])){
		metadata[,covariate1][!is.na(metadata[,covariate1])] <- 
			make.names(metadata[,covariate1][!is.na(metadata[,covariate1])])
	}
	if(!is.null(covariate2) & !is.numeric(metadata[,covariate2]) & !is.integer(metadata[,covariate2])){
		metadata[,covariate2][!is.na(metadata[,covariate2])] <- 
			make.names(metadata[,covariate2][!is.na(metadata[,covariate2])])
	}
	
	# Handle NA values in covariates
	covariate_cols <- c(variable, covariate1, covariate2)
	covariate_cols <- covariate_cols[!sapply(covariate_cols, is.null)]
	
	if(length(covariate_cols) > 1){
		na_rows <- which(rowSums(is.na(metadata[, covariate_cols])) > 0)
	} else {
		na_rows <- which(is.na(metadata[, covariate_cols]))
	}
	
	if(length(na_rows) > 0) {
		warning(paste("Removed", length(na_rows), "rows with NA values in variables/covariates"))
		metadata <- metadata[-na_rows, ]
		input <- input[, row.names(metadata)]
	}
	
	# Build design matrix
	if(!is.null(user_contrast)){
		# With contrast: use ~0+ formula (no intercept)
		if(!is.null(covariate1) & is.null(covariate2)){
			design <- model.matrix(~0 + metadata[,variable] + metadata[,covariate1], data = metadata)
			colnames(design) <- sub("metadata\\[, variable\\]", "", colnames(design))
			colnames(design) <- sub("metadata\\[, covariate1\\]", "", colnames(design))
		} else if(!is.null(covariate1) & !is.null(covariate2)){
			design <- model.matrix(~0 + metadata[,variable] + metadata[,covariate1] + metadata[,covariate2], data = metadata)
			colnames(design) <- sub("metadata\\[, variable\\]", "", colnames(design))
			colnames(design) <- sub("metadata\\[, covariate1\\]", "", colnames(design))
			colnames(design) <- sub("metadata\\[, covariate2\\]", "", colnames(design))
		} else {
			design <- model.matrix(~0 + metadata[,variable], data = metadata)
			colnames(design) <- sub("metadata\\[, variable\\]", "", colnames(design))
		}
		
		fit <- lmFit(eSet, design)
		fit2 <- contrasts.fit(fit, user_contrast)
		fit2 <- eBayes(fit2, trend = EB_trend, robust = EB_robust)
		TT <- topTable(fit2, number = Inf)
		
	} else {
		# Without contrast: standard formula with intercept
		if(!is.null(covariate1) & !is.null(covariate2)){
			design <- model.matrix(~metadata[,variable] + metadata[,covariate1] + metadata[,covariate2], data = metadata)
		} else if(!is.null(covariate1) & is.null(covariate2)){
			design <- model.matrix(~metadata[,variable] + metadata[,covariate1], data = metadata)
		} else {
			design <- model.matrix(~metadata[,variable], data = metadata)
		}
		
		fit <- lmFit(eSet, design)
		fit2 <- eBayes(fit, trend = EB_trend, robust = EB_robust)
		TT <- topTable(fit2, coef = 2, number = Inf)
	}
	
	# Filter and sort results
	TT <- TT[feature_select, ]
	TT <- TT %>% 
		rownames_to_column(var = "protein") %>% 
		arrange(P.Value) %>%
		column_to_rownames(var = "protein")
	
	# Remove NA p-values (coefficients could not be estimated)
	if(any(is.na(TT$P.Value))){
		TT <- TT[!is.na(TT$P.Value), ]
	}
	
	# Identify significant features
	sig_features <- rownames(TT[TT$P.Value < p_val & abs(TT$logFC) > log2(FC_cut), ])
	
	# Extract contrast information
	contrast_info <- list(
		groups = NULL,
		n_per_group = NULL,
		contrast_matrix = user_contrast
	)
	
	if(!is.null(user_contrast)){
		contrast_info$groups <- rownames(user_contrast)[which(user_contrast != 0)]
		contrast_info$n_per_group <- table(metadata[,variable])[contrast_info$groups]
	} else {
		contrast_info$groups <- levels(as.factor(metadata[,variable]))
		contrast_info$n_per_group <- table(metadata[,variable])
	}
	
	# Prepare significant feature data
	sig_data <- NULL
	sig_data_centered <- NULL
	
	if(length(sig_features) > 0){
		sig_data <- input[sig_features, rownames(metadata), drop = FALSE]
		sig_data_centered <- t(scale(t(sig_data), scale = FALSE, center = TRUE))
	}
	
	# Return comprehensive results
	return(list(
		design = design,
		topTable = TT,
		sig_features = sig_features,
		fit = fit2,
		metadata = metadata,
		expression = input,
		variable = variable,
		contrast_info = contrast_info,
		p_threshold = p_val,
		fc_threshold = FC_cut,
		sig_data = sig_data,
		sig_data_centered = sig_data_centered
	))
}


#' Prepare Data for Limma Visualization
#'
#' Prepares expression data and metadata for visualization (heatmaps, violin plots)
#' by ordering samples according to group membership and performing hierarchical clustering
#' within each group.
#'
#' @param limma_results List output from limma_analysis() function
#' @param add_anno Character vector of additional annotation columns from metadata to include
#' @param row_center Logical, whether to row-center the data (default FALSE)
#'
#' @return A list containing:
#' \describe{
#'   \item{plot_data}{Expression matrix ordered for plotting}
#'   \item{plot_metadata}{Metadata ordered to match plot_data columns}
#'   \item{annotation_colors}{Named list of colors for annotation variables}
#'   \item{gap_col}{Integer position for column gap in heatmap (between groups)}
#' }
#'
#' @note
#' Version 1.0.0
#' File: limma_function.R
#'
#' @import dplyr
#' @import vegan
#' @importFrom stats hclust
#' @export
#'
#' @examples
#' \dontrun{
#' results <- limma_analysis(eSet, "Sample_Group", features)
#' plot_data <- prepare_limma_plot_data(results, add_anno = c("age", "gender"))
#' }
prepare_limma_plot_data <- function(limma_results, 
																		add_anno = NULL, 
																		row_center = FALSE) {
	
	if(length(limma_results$sig_features) < 2){
		stop("Not enough significant features for plotting (minimum 2 required)")
	}
	
	# Select data to plot
	if(row_center){
		data_sig <- limma_results$sig_data_centered
	} else {
		data_sig <- limma_results$sig_data
	}
	
	# Prepare metadata
	variable <- limma_results$variable
	metadata <- limma_results$metadata
	
	if(is.null(add_anno)){
		meta_plot <- metadata[, variable, drop = FALSE]
	} else {
		cols_anno <- unique(c(variable, add_anno))
		meta_plot <- data.frame(metadata[, cols_anno, drop = FALSE])
	}
	
	# Order samples by group and cluster within groups
	if(!is.numeric(meta_plot[,variable])){
		# Get group order from contrast if available
		contrast_info <- limma_results$contrast_info
		
		if(!is.null(contrast_info$contrast_matrix)){
			get_groups <- contrast_info$contrast_matrix[contrast_info$contrast_matrix != 0, ]
			get_groups <- sort(get_groups, decreasing = TRUE)
			meta_plot[,variable] <- factor(meta_plot[,variable], levels = names(get_groups))
		} else {
			get_groups <- levels(as.factor(meta_plot[,variable]))
			meta_plot[,variable] <- factor(meta_plot[,variable], levels = get_groups)
		}
		
		# Cluster within each group
		data_sig <- data_sig[, rownames(meta_plot)]
		
		for(j in 1:length(levels(meta_plot[,variable]))){
			ns <- data_sig[, which(meta_plot[,variable] == levels(meta_plot[,variable])[j]), drop = FALSE]
			
			if(ncol(ns) > 1){
				ns_clust <- hclust(vegdist(t(ns), method = "correlation"))
				order <- ns_clust$labels[ns_clust$order]
			} else {
				order <- colnames(ns)
			}
			
			if(j == 1){
				toplot <- ns[, order, drop = FALSE]
			} else {
				toplot <- cbind(toplot, ns[, order, drop = FALSE])
			}
		}
		
		meta_plot <- meta_plot[colnames(toplot), , drop = FALSE]
		
	} else {
		# If variable is numeric, sort by value
		meta_plot <- meta_plot %>% arrange(meta_plot[,variable])
		toplot <- data_sig[, rownames(meta_plot), drop = FALSE]
	}
	
	# Calculate gap position for heatmap
	gap_tab <- table(meta_plot[,variable])
	
	if(!is.null(contrast_info$contrast_matrix)){
		get_groups <- contrast_info$contrast_matrix[contrast_info$contrast_matrix != 0, ]
		if(0.5 %in% get_groups | -0.5 %in% get_groups){
			add_groups <- names(get_groups[get_groups == 0.5 | get_groups == -0.5])
			gap_finder <- sum(gap_tab[add_groups])
		} else {
			gap_finder <- gap_tab[1]
		}
	} else {
		gap_finder <- gap_tab[1]
	}
	
	# Generate annotation colors
	anno_col <- generate_annotation_colors(meta_plot)
	
	return(list(
		plot_data = toplot,
		plot_metadata = meta_plot,
		annotation_colors = anno_col,
		gap_col = gap_finder
	))
}


#' Generate Annotation Colors for Heatmap
#'
#' Creates a named list of color palettes for heatmap annotations
#'
#' @param metadata Data frame with annotation variables
#'
#' @return Named list of color vectors for each annotation variable
#'
#' @note
#' Version 1.0.0
#' File: limma_function.R
#'
#' @importFrom grDevices colorRampPalette
#' @keywords internal
generate_annotation_colors <- function(metadata) {
	
	# Define color palettes
	distinct_colors <- c("#009E73", "#BEAED4", "#80B1D3", "goldenrod2", 
											 "coral2", "palevioletred2", "#E69F00", "#56B4E9",
											 "#CC79A7", "#D55E00")
	
	anno_col <- list()
	
	for(col in colnames(metadata)){
		if(is.numeric(metadata[,col]) | is.integer(metadata[,col])){
			# Continuous variable: use gradient
			anno_col[[col]] <- colorRampPalette(c("blue", "white", "red"))(100)
		} else {
			# Categorical variable: use distinct colors
			levels <- unique(as.character(metadata[,col]))
			n_levels <- length(levels)
			colors <- distinct_colors[1:min(n_levels, length(distinct_colors))]
			names(colors) <- levels
			anno_col[[col]] <- colors
		}
	}
	
	return(anno_col)
}


#' Convert Log Ratio to Fold Change
#'
#' Converts log2 fold change values to linear fold change
#'
#' @param logFC Numeric vector of log2 fold change values
#'
#' @return Numeric vector of fold change values
#'
#' @note
#' Version 1.0.0
#' File: limma_function.R
#'
#' @export
#'
#' @examples
#' logratio2foldchange(c(1, -1, 2, -2))
logratio2foldchange <- function(logFC) {
	ifelse(logFC < 0, -1 / (2^logFC), 2^logFC)
}