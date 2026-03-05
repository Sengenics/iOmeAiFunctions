# iOmeAiFunctions Development Guide

This guide explains how to develop and maintain the iOmeAiFunctions package using our streamlined renv-based workflow.

---

## 📋 Table of Contents

- [Quick Start](#quick-start)
- [File Structure](#file-structure)
- [Common Tasks](#common-tasks)
- [Workflow Guide](#workflow-guide)
- [Troubleshooting](#troubleshooting)
- [Team Collaboration](#team-collaboration)

---

## 🚀 Quick Start

### First Time Setup (New Team Member)

```r
# 1. Clone the repository
git clone https://github.com/Sengenics/iOmeAiFunctions.git
cd iOmeAiFunctions

# 2. Open R in the project directory
R

getwd()
# 3.  Clean Up
unlink("NAMESPACE")
unlink("man", recursive = TRUE)
#unlink("src", recursive = TRUE)

# 4. Restore all dependencies from renv.lock
source("renv/activate.R")
renv::restore(promt = FALSE)

# 4. Build the package
source("tools/setup_package.R")

# 6. Test it works
library(iOmeAiFunctions)

# Check what functions you DO have
ls("package:iOmeAiFunctions")

# Check package info
packageVersion("iOmeAiFunctions")

# Check dependencies are loaded
sessionInfo()

