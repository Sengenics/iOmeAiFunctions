# tools/setup_package.R
version = "0.1.2"
release_data = '2025-11-17'

options(
	renv.consent = TRUE,
	renv.config.install.prompt = FALSE,
	renv.config.restore.prompt = FALSE,
	renv.config.snapshot.prompt = FALSE,
	install.packages.check.source = "no",
	install.packages.compile.from.source = "never",
	menu.graphics = FALSE
)

# --- Scaffolding ---
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# if (!file.exists("renv.lock")) {
# 	renv::init(bare = TRUE)
# } else {
# 	renv::restore()
# }

if (file.exists("renv/activate.R")) {
	source("renv/activate.R")
}
#usethis::create_package(".")
usethis::use_mit_license("Shaun Garnett")
usethis::use_readme_rmd()
usethis::use_build_ignore("tools/")
#usethis::use_git(commit_first = FALSE)
#usethis::use_github_links()

# Source the PSA_ROs vector from the temp directory
source("data-raw/PSA_ROs_data.R")
usethis::use_data(PSA_ROs_pkg, overwrite = TRUE)

# Save it as internal data (won’t be exported to users)
usethis::use_data(PSA_ROs_pkg, internal = TRUE, overwrite = TRUE)

# Source and prepare ExpSet example data
source("data-raw/ExpSet_data.R")

# --- Install & sync dependencies ---
source("tools/install_dependencies.R")

# --- Set Version and Encoding if missing ---
desc_path <- "DESCRIPTION"
desc <- readLines(desc_path)

if (!any(grepl("^Version:", desc))) {
	desc <- append(desc, paste("Version: ",version), after = grep("^Package:", desc))
	writeLines(desc, desc_path)
	message(paste("✔ DESCRIPTION Version set to",version))
}
if (!any(grepl("^Encoding:", desc))) {
	desc <- append(desc, "Encoding: UTF-8", after = grep("^Version:", desc))
	writeLines(desc, desc_path)
	message("✔ DESCRIPTION Encoding set to UTF-8")
}

# --- Finalize ---
unlink("NAMESPACE")
unlink("man", recursive = TRUE)
devtools::document()
devtools::install(upgrade = "never")
#remotes::install_local(upgrade = "never")

#renv::snapshot()
renv::snapshot(prompt = FALSE, force = TRUE)

# --- Manage package conflicts ---
if (!requireNamespace("conflicted", quietly = TRUE)) install.packages("conflicted")
library(conflicted)

# Set conflict preferences
conflict_prefer('exprs', 'Biobase')
conflict_prefer('setdiff', 'base')
conflict_prefer('filter', 'dplyr')
#conflict_prefer('shinydashboard','box')
conflicts_prefer(shinydashboard::box)

message("✔ Package setup complete")




