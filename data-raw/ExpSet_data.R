# data-raw/ExpSet_data.R
# Prepare ExpSet example data for package

# Load the RDS file
ExpSet <- readRDS("ExampleData/ExpSet_list.rds")

# Save as package data (will be available to users)
usethis::use_data(ExpSet, overwrite = TRUE)

message("✔ ExpSet data prepared for package")