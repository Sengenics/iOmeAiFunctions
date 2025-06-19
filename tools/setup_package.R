# tools/setup_package.R

options(
	renv.consent = TRUE,
	renv.config.install.prompt = FALSE,
	renv.config.restore.prompt = FALSE,
	renv.config.snapshot.prompt = FALSE,
	install.packages.check.source = "no",
	install.packages.compile.from.source = "never",
	menu.graphics = FALSE  # Important for macOS
)

# --- Install required scaffolding packages ---
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# --- Initialize renv ---
if (!file.exists("renv.lock")) {
	renv::init(bare = TRUE)
} else {
	renv::restore()
}

# --- usethis scaffolding ---
usethis::use_mit_license("Shaun Garnett")
usethis::use_readme_rmd()
usethis::use_build_ignore("tools/")
usethis::use_build_ignore("tools/required_packages.R")
usethis::use_git()
usethis::use_github_links()


# --- Install and load all required packages into the renv environment ---
source("tools/required_packages.R")
# Install dependencies
source("tools/install_dependencies.R")

# --- Lock versions for reproducibility going forward ---
renv::snapshot()

message("✔️ Package setup complete with version control.")

devtools::clean_vignettes()
devtools::document()
devtools::install(build_vignettes = FALSE)

# --- Lock versions for reproducibility going forward ---
renv::snapshot()

message("✔️ Package setup complete with version control.")

