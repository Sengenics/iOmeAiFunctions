# File Comparison Shiny App
# Purpose: Compare protein data between two files
# Author: Sengenics
# Date: 2026-01-28

# Load libraries
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(corrplot)

# Debug flag (only shows in RStudio)
debug <- !is.na(Sys.getenv("RSTUDIO", unset = NA))

# App settings
options(shiny.maxRequestSize = 100*1024^2)  # 100MB max file size

# Color palette for visualizations
colors_sengenics <- list(
	primary = "#0066CC",
	secondary = "#FF6600",
	neutral = "#666666"
)

source('functions.R')