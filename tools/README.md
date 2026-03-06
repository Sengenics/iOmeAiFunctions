# iOmeAiFunctions Development Guide

This guide explains how to develop and maintain the iOmeAiFunctions package using our streamlined renv-based workflow.

---

## 📋 Table of Contents

- [Quick Start](#quick-start)
- [Package Management](#package-management)
- [Resolving Conflicts](#resolving-conflicts)
- [Troubleshooting](#troubleshooting)

---

## 🚀 Quick Start

### First Time Setup (New Team Member)

```
# 1. Clone the repository
git clone https://github.com/Sengenics/iOmeAiFunctions.git
cd iOmeAiFunctions

# 2. Open R in the project directory
R

# 3. Clean Up
unlink("NAMESPACE")
unlink("DESCRIPTION")
unlink("man", recursive = TRUE)

# 4. Restore all dependencies from renv.lock
source("renv/activate.R")
renv::restore(prompt = FALSE)

# 5. Build the package
source("tools/setup_package.R")

# 6. Test it works
library(iOmeAiFunctions)

# Check what functions you have
ls("package:iOmeAiFunctions")

# Check package info
packageVersion("iOmeAiFunctions")

# Check dependencies are loaded
sessionInfo()
```

---

## 📦 Package Management

### Adding New Package Dependencies

#### Step 1: Edit `tools/package_requirements.R`

Add the package to the appropriate section:

**For CRAN packages:**

```
imports_cran = c(
    "AUC",
    "callr",
    # ... existing packages ...
    "newpackage",  # <- Add here
    "yourpackage"
)
```

**For Bioconductor packages:**

```
imports_bioc = c(
    "Biobase",
    "EnhancedVolcano",
    # ... existing packages ...
    "newbiocpackage"  # <- Add here
)
```

See the file for additional sections, update appropriately.

#### Step 2: Regenerate DESCRIPTION

```
source("tools/update_description.R")
```

#### Step 3: Install and Test

```
# Install new dependencies
install.packages("newpackage")

# Rebuild package
devtools::document()
devtools::load_all()

# Test that it works
library(iOmeAiFunctions)
```

#### Step 4: Commit and Push

```
system("git add DESCRIPTION tools/package_requirements.R")
system("git commit -m 'Add newpackage dependency'")
system("git push")
```

---

## ⚠️ Resolving Conflicts

If you see warnings like this:

```
✖ lubridate::setdiff conflicts with BiocGenerics::setdiff and base::setdiff
```

### Step 1: Decide Which Function to Use

- **Base R:** Prefer for common operations (`setdiff`, `intersect`, `union`)
- **dplyr:** Prefer for data manipulation (`filter`, `select`, `mutate`)
- **Package-specific:** Prefer specialized functions (`Biobase::exprs`, `DT::renderDataTable`)

### Step 2: Edit `R/zzz.R`

Add conflict resolution to `.onAttach()` function:

```
.onAttach <- function(libname, pkgname) {
    if (requireNamespace("conflicted", quietly = TRUE)) {
        
        # Base R preferences (for set operations)
        conflicted::conflict_prefer("setdiff", "base", quiet = TRUE)
        conflicted::conflict_prefer("intersect", "base", quiet = TRUE)
        conflicted::conflict_prefer("union", "base", quiet = TRUE)
        
        # dplyr preferences (for data manipulation)
        conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
        conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
        
        # plotly preferences
        conflicted::conflict_prefer("layout", "plotly", quiet = TRUE)
        
        # Package-specific preferences
        conflicted::conflict_prefer("exprs", "Biobase", quiet = TRUE)
        conflicted::conflict_prefer("renderDataTable", "DT", quiet = TRUE)
    }
}
```

### Step 3: Rebuild Package

```
devtools::document()
pkgload::load_all()
```

### Step 4: Test and Commit

```
# Test - should load without warnings
library(iOmeAiFunctions)

# Commit changes
system("git add R/zzz.R")
system("git commit -m 'Resolve function conflicts'")
system("git push")
```

---

## 🔧 Common Conflict Resolutions

| Conflict | Prefer | Reason |
|----------|--------|--------|
| `setdiff`, `intersect`, `union` | `base` | Standard R operations |
| `filter`, `select`, `mutate` | `dplyr` | Data manipulation |
| `layout` | `plotly` | Interactive plots |
| `exprs`, `pData`, `fData` | `Biobase` | ExpressionSet operations |
| `renderDataTable` | `DT` | Shiny tables |
| `box` | `shinydashboard` | Dashboard UI |

---

## 🐛 Troubleshooting

**Q: New package not available on CRAN?**

```
# Check if it's Bioconductor
BiocManager::available("packagename")

# If yes, add to imports_bioc instead of imports_cran
```

**Q: Conflict warnings still appearing?**

```
# Check if conflicted is installed
install.packages("conflicted")

# Verify zzz.R loaded correctly
iOmeAiFunctions:::.onAttach()
```

**Q: Package build fails?**

```
# Clean and rebuild
unlink("NAMESPACE")
unlink("man", recursive = TRUE)
source("tools/setup_package.R")
```

## 🏷️ Version Management

### Creating a New Release

#### Step 1: Update Version Number

Edit `tools/package_requirements.R`:

```
# Package metadata
package_name <- "iOmeAiFunctions"
version <- "0.1.5"  # <- Update this
```

#### Step 2: Regenerate Package Files

```
source("tools/update_description.R")
devtools::document()
```

#### Step 3: Commit Changes

```
git status
git commit -am 'update message'
git push github master
```

#### Step 4: Create Git Tag

```
# Create annotated tag
git tag InHouse_3_14 -m 'Release version for InHouse 3.14 all necessary packages exported

# Push tag to GitHub
 git push github InHouse_3_14
```

#### Step 5: Create GitHub Release (Optional)

1. Go to https://github.com/Sengenics/iOmeAiFunctions/releases
2. Click "Draft a new release"
3. Choose tag `InHouse_3_14`
4. Add release notes
5. Click "Publish release"

---

### Installing Specific Versions

**Install latest version:**

```
remotes::install_github("Sengenics/iOmeAiFunctions")
```

**Install specific tagged version:**

```
remotes::install_github("Sengenics/iOmeAiFunctions@v0.1.5")
```

**Install specific branch:**

```
remotes::install_github("Sengenics/iOmeAiFunctions@denoiser_debug")
```

**Install specific commit:**

```
remotes::install_github("Sengenics/iOmeAiFunctions@a1b2c3d")
```

---

### Version Numbering (Semantic Versioning)

Format: `MAJOR.MINOR.PATCH` (e.g., `0.1.5`)

- **MAJOR** (`1.0.0`): Breaking changes, incompatible API changes
- **MINOR** (`0.2.0`): New features, backwards compatible
- **PATCH** (`0.1.1`): Bug fixes, backwards compatible

**Examples:**
- `v0.1.5` → `v0.1.6`: Fixed a bug
- `v0.1.5` → `v0.2.0`: Added new function
- `v0.1.5` → `v1.0.0`: Changed function signatures (breaking change)

---

### Viewing Available Versions

**List all tags:**

```
system("cd ~/iOmeAiFunctions && git tag -l")
```

**View on GitHub:**

https://github.com/Sengenics/iOmeAiFunctions/tags

---

### Using Versions in Shiny Apps

In your app's `global.R`:

```
# Production: Use specific stable version
if (!requireNamespace("iOmeAiFunctions", quietly = TRUE)) {
  remotes::install_github(
    "Sengenics/iOmeAiFunctions@v0.1.5",  # <- Pin to specific version
    upgrade = "never",
    force = FALSE
  )
}

library(iOmeAiFunctions)
```

**Version file per app:** Create `inst/ExampleApps/YourApp/VERSION`:

```
v0.1.5
```

Then in `global.R`:

```
version <- trimws(readLines("VERSION", n = 1))
remotes::install_github(paste0("Sengenics/iOmeAiFunctions@", version))
```

---

### Version History

| Version | Date | Changes |
|---------|------|---------|
| v0.1.5 | 2026-03-06 | Added denoiser module, fixed conflicts |
| v0.1.4 | 2026-03-01 | Added dendextend support |
| v0.1.3 | 2026-02-15 | Initial Shiny modules |

---

### Rollback to Previous Version

**If something breaks:**

```
# Install previous stable version
remotes::install_github("Sengenics/iOmeAiFunctions@v0.1.4", force = TRUE)
```

**In development:**

```
# Checkout previous tag
system("cd ~/iOmeAiFunctions && git checkout v0.1.4")

# Or revert to commit
system("cd ~/iOmeAiFunctions && git log --oneline")  # Find commit hash
system("cd ~/iOmeAiFunctions && git checkout a1b2c3d")
```

---

### Best Practices

1. **Always tag stable releases** before deploying to production
2. **Use branches for development** (`denoiser_debug`, `feature-x`)
3. **Pin versions in production apps** - don't use `@main`
4. **Document breaking changes** in release notes
5. **Test before tagging** - tags should be stable