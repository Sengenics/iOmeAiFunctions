# Helper Functions for File Comparison App
# Date: 2026-01-28

#' Load and validate data file
#'
#' @param file_path Character. Path to file to load
#' @param file_name Character. Original filename for reference
#'
#' @return List containing data.table and metadata, or NULL if error
#' @export
#'
#' @details
#' Expects first column to be protein names, remaining columns to be samples
load_data_file <- function(file_path, file_name) {
  tryCatch({
    # Load file using fread
    dt <- data.table::fread(file_path, header = TRUE)
    
    # Validate structure
    if (ncol(dt) < 2) {
      stop("File must have at least 2 columns (proteins + 1 sample)")
    }
    
    # Get protein column (assume first column)
    protein_col <- names(dt)[1]
    sample_cols <- names(dt)[-1]
    
    # Check for numeric data in sample columns
    numeric_check <- sapply(dt[, ..sample_cols], is.numeric)
    if (!all(numeric_check)) {
      warning("Some sample columns contain non-numeric data")
    }
    
    # Return structured list
    list(
      data = dt,
      protein_col = protein_col,
      sample_cols = sample_cols,
      n_proteins = nrow(dt),
      n_samples = length(sample_cols),
      file_name = file_name,
      loaded_at = Sys.time()
    )
    
  }, error = function(e) {
    message("Error loading file: ", e$message)
    return(NULL)
  })
}

#' Compare two data files
#'
#' @param data1 List. Output from load_data_file()
#' @param data2 List. Output from load_data_file()
#'
#' @return List containing comparison results
#' @export
compare_files <- function(data1, data2) {
  if (is.null(data1) || is.null(data2)) {
    return(NULL)
  }
  
  # Get protein lists
  proteins1 <- data1$data[[data1$protein_col]]
  proteins2 <- data2$data[[data2$protein_col]]
  
  # Find overlaps and unique proteins
  common_proteins <- intersect(proteins1, proteins2)
  unique_to_file1 <- setdiff(proteins1, proteins2)
  unique_to_file2 <- setdiff(proteins2, proteins1)
  
  # Summary statistics
  comparison <- list(
    n_common = length(common_proteins),
    n_unique_file1 = length(unique_to_file1),
    n_unique_file2 = length(unique_to_file2),
    common_proteins = common_proteins,
    unique_to_file1 = unique_to_file1,
    unique_to_file2 = unique_to_file2,
    percent_overlap = round(length(common_proteins) / length(union(proteins1, proteins2)) * 100, 1)
  )
  
  # Merge data for common proteins if they exist
  if (length(common_proteins) > 0) {
    # Create merged dataset
    merged_dt <- merge(
      data1$data[get(data1$protein_col) %in% common_proteins],
      data2$data[get(data2$protein_col) %in% common_proteins],
      by.x = data1$protein_col,
      by.y = data2$protein_col,
      suffixes = c("_file1", "_file2")
    )
    comparison$merged_data <- merged_dt
  } else {
    comparison$merged_data <- NULL
  }
  
  return(comparison)
}

#' Calculate correlation between files
#'
#' @param comparison List. Output from compare_files()
#' @param data1 List. Output from load_data_file()
#' @param data2 List. Output from load_data_file()
#'
#' @return Data frame of correlations or NULL
#' @export
calculate_correlation <- function(comparison, data1, data2) {
  if (is.null(comparison$merged_data)) {
    return(NULL)
  }
  
  tryCatch({
    merged <- comparison$merged_data
    
    # Get numeric columns for each file
    file1_cols <- grep("_file1$", names(merged), value = TRUE)
    file2_cols <- grep("_file2$", names(merged), value = TRUE)
    
    if (length(file1_cols) == 0 || length(file2_cols) == 0) {
      return(NULL)
    }
    
    # Calculate correlations
    cor_results <- data.frame(
      sample_file1 = character(),
      sample_file2 = character(),
      correlation = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (col1 in file1_cols) {
      for (col2 in file2_cols) {
        if (all(!is.na(merged[[col1]])) && all(!is.na(merged[[col2]]))) {
          cor_val <- cor(merged[[col1]], merged[[col2]], use = "pairwise.complete.obs")
          cor_results <- rbind(cor_results, data.frame(
            sample_file1 = col1,
            sample_file2 = col2,
            correlation = cor_val
          ))
        }
      }
    }
    
    return(cor_results)
    
  }, error = function(e) {
    message("Error calculating correlation: ", e$message)
    return(NULL)
  })
}

#' Check if data between files is identical or similar
#'
#' @param comparison List. Output from compare_files()
#' @param data1 List. Output from load_data_file()
#' @param data2 List. Output from load_data_file()
#' @param tolerance Numeric. Tolerance for "near identical" (default 1e-6)
#'
#' @return List containing identity results and explanations
#' @export
check_data_identity <- function(comparison, data1, data2, tolerance = 1e-6) {
  
  results <- list(
    is_identical = FALSE,
    is_near_identical = FALSE,
    differences = list(),
    explanations = character()
  )
  
  # Check if we have common proteins
  if (is.null(comparison$merged_data) || comparison$n_common == 0) {
    results$explanations <- c(
      "❌ NO COMMON PROTEINS: The files have no overlapping proteins to compare."
    )
    return(results)
  }
  
  merged <- comparison$merged_data
  protein_col <- names(merged)[1]
  
  # Get matching column pairs (same sample name in both files)
  file1_cols <- grep("_file1$", names(merged), value = TRUE)
  file2_cols <- grep("_file2$", names(merged), value = TRUE)
  
  # Extract base sample names
  file1_base <- gsub("_file1$", "", file1_cols)
  file2_base <- gsub("_file2$", "", file2_cols)
  
  # Find matching samples
  matching_samples <- intersect(file1_base, file2_base)
  
  if (length(matching_samples) == 0) {
    results$explanations <- c(
      "❌ NO MATCHING SAMPLE NAMES: The sample column names don't match between files.",
      "   File 1 samples:", paste(file1_base, collapse = ", "),
      "   File 2 samples:", paste(file2_base, collapse = ", ")
    )
    return(results)
  }
  
  # Check each matching sample
  all_identical <- TRUE
  all_near_identical <- TRUE
  
  for (sample in matching_samples) {
    col1 <- paste0(sample, "_file1")
    col2 <- paste0(sample, "_file2")
    
    vec1 <- merged[[col1]]
    vec2 <- merged[[col2]]
    
    # Check for identical
    is_identical_sample <- identical(vec1, vec2)
    
    # Check for near identical (within tolerance)
    max_diff <- max(abs(vec1 - vec2), na.rm = TRUE)
    is_near_identical_sample <- max_diff < tolerance
    
    if (!is_identical_sample) {
      all_identical <- FALSE
      
      results$differences[[sample]] <- list(
        max_difference = max_diff,
        mean_difference = mean(abs(vec1 - vec2), na.rm = TRUE),
        correlation = cor(vec1, vec2, use = "complete.obs"),
        n_differences = sum(vec1 != vec2, na.rm = TRUE),
        percent_different = round(sum(vec1 != vec2, na.rm = TRUE) / length(vec1) * 100, 2)
      )
    }
    
    if (!is_near_identical_sample) {
      all_near_identical <- FALSE
    }
  }
  
  results$is_identical <- all_identical
  results$is_near_identical <- all_near_identical
  
  # Generate explanations
  if (all_identical) {
    results$explanations <- c(
      "✅ DATA IS IDENTICAL: All matching samples have exactly the same values."
    )
  } else if (all_near_identical) {
    results$explanations <- c(
      "✅ DATA IS NEARLY IDENTICAL: Values differ by less than the tolerance threshold.",
      paste0("   Tolerance: ", tolerance),
      "",
      "Possible reasons for minor differences:",
      "  • Rounding during export/import",
      "  • Floating-point precision differences",
      "  • Different software versions used for processing"
    )
  } else {
    results$explanations <- c(
      "❌ DATA IS NOT IDENTICAL: Significant differences found between files.",
      "",
      "Common reasons for differences:",
      "  1. DIFFERENT PROCESSING: Files processed with different parameters",
      "  2. DIFFERENT NORMALIZATION: Different normalization methods applied",
      "  3. BACKGROUND CORRECTION: Different background subtraction methods",
      "  4. SOFTWARE VERSIONS: Different analysis software versions",
      "  5. DATA FILTERING: One file has additional filtering applied",
      "  6. TRANSFORMATION: Different data transformations (log, sqrt, etc.)",
      "  7. BATCH EFFECTS: Samples processed in different batches",
      "  8. PARTIAL UPDATE: One file updated but not the other",
      "  9. DIFFERENT SOURCE: Files from different experiments/runs",
      "  10. MANUAL EDITS: One file manually edited after export"
    )
    
    # Add specific sample differences
    if (length(results$differences) > 0) {
      results$explanations <- c(
        results$explanations,
        "",
        "Sample-specific differences:"
      )
      
      for (sample in names(results$differences)) {
        diff_info <- results$differences[[sample]]
        results$explanations <- c(
          results$explanations,
          paste0("  • ", sample, ":"),
          paste0("    - Max difference: ", round(diff_info$max_difference, 4)),
          paste0("    - Mean difference: ", round(diff_info$mean_difference, 4)),
          paste0("    - Correlation: ", round(diff_info$correlation, 4)),
          paste0("    - Values different: ", diff_info$n_differences, 
                 " (", diff_info$percent_different, "%)")
        )
      }
    }
  }
  
  return(results)
}