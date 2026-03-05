#' List Available Pipeline Templates
#'
#' @return Character vector of available template names
#' @export
#' @examples
#' list_templates()
list_templates <- function() {
	template_dir <- system.file("templates/pipelines", package = "iOmeAiFunctions")
	
	if (!dir.exists(template_dir)) {
		message("No templates found in package")
		return(character(0))
	}
	
	templates <- list.files(template_dir, pattern = "\\.R$")
	gsub("_template\\.R$", "", templates)
}


#' Copy Pipeline Template to Current Directory
#'
#' @param template Character; template name (see list_templates())
#' @param destination Character; destination path (default: current directory)
#' @param overwrite Logical; overwrite existing file?
#'
#' @return Path to copied file (invisibly)
#' @export
#'
#' @examples
#' # List available templates
#' list_templates()
#'
#' # Copy denoiser template
#' use_template("denoiser_pipeline")
#'
#' # Copy to specific location
#' use_template("denoiser_pipeline", destination = "analysis/")
use_template <- function(template, destination = ".", overwrite = FALSE) {
	
	# Find template file
	template_file <- paste0(template, "_template.R")
	template_path <- system.file("templates/pipelines", template_file, 
															 package = "iOmeAiFunctions")
	
	if (!file.exists(template_path)) {
		stop("Template '", template, "' not found. Use list_templates() to see available templates.")
	}
	
	# Destination
	dest_file <- file.path(destination, gsub("_template", "", template_file))
	
	if (file.exists(dest_file) && !overwrite) {
		stop("File '", dest_file, "' already exists. Use overwrite = TRUE to replace.")
	}
	
	# Copy file
	file.copy(template_path, dest_file, overwrite = overwrite)
	
	message("✓ Template copied to: ", dest_file)
	message("\nNext steps:")
	message("  1. Open the file in RStudio")
	message("  2. Edit the CONFIGURATION section")
	message("  3. Run the pipeline!")
	
	invisible(dest_file)
}


#' View Template in Editor
#'
#' @param template Character; template name
#'
#' @export
#' @examples
#' \dontrun{
#' view_template("denoiser_pipeline")
#' }
view_template <- function(template) {
	template_file <- paste0(template, "_template.R")
	template_path <- system.file("templates/pipelines", template_file, 
															 package = "iOmeAiFunctions")
	
	if (!file.exists(template_path)) {
		stop("Template '", template, "' not found.")
	}
	
	if (requireNamespace("rstudioapi", quietly = TRUE) && 
			rstudioapi::isAvailable()) {
		rstudioapi::navigateToFile(template_path)
	} else {
		file.edit(template_path)
	}
}


#' Get Path to Template File
#'
#' @param template Character; template name
#'
#' @return Path to template file
#' @export
#' @examples
#' get_template_path("denoiser_pipeline")
get_template_path <- function(template) {
	template_file <- paste0(template, "_template.R")
	template_path <- system.file("templates/pipelines", template_file, 
															 package = "iOmeAiFunctions")
	
	if (!file.exists(template_path)) {
		stop("Template '", template, "' not found.")
	}
	
	template_path
}