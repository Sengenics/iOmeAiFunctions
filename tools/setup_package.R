# tools/setup_package.R

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

if (!file.exists("renv.lock")) {
	renv::init(bare = TRUE)
} else {
	renv::restore()
}

usethis::use_mit_license("Shaun Garnett")
usethis::use_readme_rmd()
usethis::use_build_ignore("tools/")
#usethis::use_git(commit_first = FALSE)
#usethis::use_github_links()

# --- Install & sync dependencies ---
source("tools/install_dependencies.R")

# --- Set Version and Encoding if missing ---
desc_path <- "DESCRIPTION"
desc <- readLines(desc_path)
if (!any(grepl("^Version:", desc))) {
	desc <- append(desc, "Version: 0.1.0", after = grep("^Package:", desc))
	writeLines(desc, desc_path)
	message("✔ DESCRIPTION Version set to 0.1.0")
}
if (!any(grepl("^Encoding:", desc))) {
	desc <- append(desc, "Encoding: UTF-8", after = grep("^Version:", desc))
	writeLines(desc, desc_path)
	message("✔ DESCRIPTION Encoding set to UTF-8")
}

# --- Finalize ---
unlink(file.path("NAMESPACE"))
devtools::document()
remotes::install_local(upgrade = "never")

renv::snapshot()
message("✔ Package setup complete")
