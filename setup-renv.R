# setup_renv.R

# Initialize renv and snapshot the package environment
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

renv::init(bare = TRUE)
renv::restore()  # or snapshot() to capture current state
