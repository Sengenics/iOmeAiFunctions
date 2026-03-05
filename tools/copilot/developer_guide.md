# Sengenics Developer Guide

**Last Updated:** 2025-11-26  
**Organization:** Sengenics  
**Purpose:** Developer setup, project management, troubleshooting, and workflows

> **For AI Coding Context:** See `copilot_context.md` for patterns and standards.

---

## 📋 Table of Contents

1. [Getting Started](#getting-started)
2. [GitHub Authentication Setup](#github-authentication-setup)
3. [Project Management](#project-management)
4.  [Current Sprint](#current-sprint)
5. [Active Repositories](#active-repositories)
6.  [Storyboard System](#storyboard-system)
7. [Troubleshooting](#troubleshooting)
8. [Team Preferences](#team-preferences)
9.  [Maintenance Schedule](#maintenance-schedule)

---

## 🚀 Getting Started

### New Developer Onboarding

**Day 1:**
1. Clone repositories
2. Set up GitHub token (see below)
3. Install required R packages
4. Review `copilot_context.md`
5. Read this guide

**Week 1:**
- Familiarize with storyboard system
- Review recent sprint work
- Set up local development environment

**Required Tools:**
- R (version 4.x)
- RStudio
- Git
- GitHub account with Sengenics organization access
- MacPorts (macOS) or appropriate package manager

---

### RStudio Setup

**Enable Code Navigation:**
1. Open RStudio
2. Go to: View → Show Document Outline (or press Cmd+Shift+O on Mac)
3. You'll see an outline panel appear on the right side
4. This shows all section headers in your code

**Why this matters:**
- Quickly jump to any section in large files
- See code structure at a glance
- Works with the section header format (# Section ====)

**Recommended Settings:**
- Tools → Global Options → Code → Display
  - ✓ Show document outline
  - ✓ Show margin (80 characters)
  - ✓ Highlight R function calls

## 🔑 GitHub Authentication Setup

### Why You Need This
- Access private Sengenics repositories
- Fetch live data in dashboards
- Avoid API rate limits (60/hour → 5000/hour)

### Step-by-Step Setup

#### 1. Create Personal Access Token

```
1. Go to https://github.com/settings/tokens
2. Click "Generate new token" → "Generate new token (classic)"
3.  Name: "R Development" or "Sengenics Dashboard"
4. Expiration: 90 days (set calendar reminder!)
5.  Scopes needed:
   ✓ repo (full control of private repositories)
   ✓ read:org (if accessing organization data)
6. Click "Generate token"
7.  COPY the token immediately (you won't see it again!)
```

#### 2. Store Token in .  Renviron (RECOMMENDED)

```r
# In R console:
usethis::edit_r_environ()

# In the file that opens, add:
GITHUB_PAT=ghp_your_actual_token_here

# Save file and close

# CRITICAL: Restart R
# RStudio: Session > Restart R
# Or: Cmd+Shift+F10 (Mac) / Ctrl+Shift+F10 (Windows)

# Verify:
Sys.getenv("GITHUB_PAT")  # Should show your token
```

**Why .  Renviron? **
- ✅ Persistent across all R sessions
- ✅ Automatically loaded on R startup
- ✅ Already in `. gitignore`
- ✅ Standard R practice
- ✅ Works with all R packages (httr, gh, usethis)

#### 3.  Verify Setup

```r
# Quick check
token <- Sys.getenv("GITHUB_PAT")
if (token == "") {
  message("❌ Token not set - did you restart R?")
} else {
  message("✓ Token is set (", nchar(token), " characters)")
}

# Test with gh package
if (! require("gh")) install.packages("gh")
gh::gh_whoami()
# Should show your GitHub username

# Full test (see Troubleshooting section for test_github_auth. R)
source("test_github_auth.R")
test_github_authentication()
```

#### 4. Alternative: Config File (Production Only)

For deployed apps on servers:

```r
# Create config directory
dir.create(path. expand("~/.sengenics_config"), showWarnings = FALSE)
writeLines("ghp_your_token", path.expand("~/.sengenics_config/github_token. txt"))

# Add to .gitignore:
*_config/
. sengenics_config/

# Load in global.R:
token_file <- path.expand("~/. sengenics_config/github_token.txt")
if (file.exists(token_file)) {
  github_token <- readLines(token_file, warn = FALSE)
  Sys.setenv(GITHUB_PAT = github_token)
}
```

---

## 📊 Project Management

### Current Focus & Objectives

**Primary Goal:**  
Modularizing protein microarray analysis applications with feature-level control

**Focus Areas:**
1. Converting monolithic Shiny app into reusable modules
2. Implementing Advanced/Basic mode switching
3. Creating standalone module apps that accept ExpSet_list inputs
4. Dual deployment: InHouse (Advanced) + Public (Basic)
5. Package development for core functions

---

## 🎯 Current Sprint (Updated: 2025-11-26)

### This Week
- **Module Development:** Converting ShinyApp functions to modules with feature flags
- **Test Case:** Using denoiser app as reference for ExpSet import and standalone usage
- **Feature Switching:** Implementing Advanced/Basic mode toggle system
- **Package Migration:** Moving functions to iOmeAiFunctions with proper documentation

### Completed Recently
- Dashboard repository monitoring with GitHub API
- Storyboard system implementation
- Context file restructuring

### Next Up
- Complete QC module migration
- Standalone heatmap app
- Public app CSS branding

---

## 📦 Active Repositories

### Core Development

#### 1. iOmeAiFunctions
- **URL:** `github.com/Sengenics/iOmeAiFunctions`
- **Type:** R Package
- **Purpose:** Central repository for all analysis functions and modules
- **Key Files:**
  - `R/` - Function definitions
  - `man/` - Documentation
  - `NAMESPACE` - Exports
- **Dev Branch:** `develop`
- **Maintainer:** Shaun Garnett

#### 2. i-Ome-AI (InHouse)
- **URL:** `github.com/Sengenics/i-Ome-AI`
- **Type:** Shiny App (Private)
- **Purpose:** Internal analysis platform with full advanced features
- **Deployment:** Internal server
- **Mode:** Advanced (all features enabled)

#### 3. i-Ome-AI_Public
- **URL:** `github.com/Sengenics/i-Ome-AI_Public`
- **Type:** Shiny App (Public)
- **Purpose:** Customer-facing simplified platform
- **Deployment:** Public server
- **Mode:** Basic (simplified features)
- **Note:** Has specific CSS branding requirements

#### 4. KREX_App_Dashboard
- **URL:** `github.com/Sengenics/KREX_App_Dashboard`
- **Type:** Shiny Dashboard
- **Purpose:** Development tracking and project management
- **Features:**
  - Repository monitoring
  - Storyboard system
  - Context file viewer
  - Sprint tracking

### Infrastructure

#### 5.  KREX-Protein-Design
- Automation for KREX protein workflows

#### 6. VennApp
- Venn diagram visualization tool

#### 7. Analysis-Pipeline
- Core analysis pipeline infrastructure

#### 8. i-Ome-AI_Helm
- Kubernetes deployment charts

### Archived
- ShinyApp (2022) - Being converted to modular system
- Pro-MAP series (2022-2023)
- ProductDB (2023)
- New_Array_Content (2023)

---

## 📖 Storyboard System

### Purpose
Track daily development work, decisions, and progress. 

### File Format (EXACT - DO NOT CHANGE)

```markdown
---
date: YYYY-MM-DD
title: "Brief Title of Work Done"
project: ProjectName
tags: [tag1, tag2, tag3]
hours: 2. 5
status: completed
---

## Summary
Brief 1-2 sentence overview. 

## What I Did
- Detailed bullet points
- Specific changes made
- Features implemented

## Challenges
- Problems encountered
- How they were resolved
- Lessons learned

## Next Steps
- What needs to happen next
- Dependencies or blockers
- Future improvements

## Files Changed
- `path/to/file1.R`
- `path/to/file2.R`

## Screenshots/Notes
Additional context, links, references.
```

### File Structure

```
StoryBoard/
├── YYYYMM/                    # Folder: 202511
│   ├── YYYYMMDD_title-slug.md  # File: 20251126_dashboard-setup.md
│   ├── YYYYMMDD_another. md
│   └── ...
├── 202510/
└── ... 
```

**Rules:**
- ✅ Folder naming: `YYYYMM/` (e.g., `202511/`)
- ✅ File naming: `YYYYMMDD_title-slug. md` (e.g., `20251126_github-auth.md`)
- ✅ Location: `StoryBoard/YYYYMM/YYYYMMDD_title. md`
- ✅ Note capital S and B in "StoryBoard"

### Adding Entries

**Via Dashboard (Recommended):**
1. Open KREX_App_Dashboard
2.  Navigate to Storyboard tab
3. Click "Add New Entry"
4. Fill in form
5. Submit

**Manually:**
1. Create file in correct location
2. Follow template exactly
3.  Commit and push to git

**Best Practices:**
- Log substantial work (>30 minutes)
- Be specific about changes
- Include code file paths
- Document decisions and why
- Add links to relevant issues/PRs

---

## 🔧 Troubleshooting

### GitHub Token Issues

#### Token Not Found
```r
# Check if set
Sys.getenv("GITHUB_PAT")
# Returns "" → Not set

# Solution:
# 1. usethis::edit_r_environ()
# 2. Add: GITHUB_PAT=ghp_your_token
# 3. Save and RESTART R
```

#### 404 Errors on Private Repos
```
Error: 404 Not Found
```

**Causes:**
1. Token doesn't have `repo` scope
2. Token expired
3. Not member of organization

**Solution:**
```r
# Check token scopes at: https://github.com/settings/tokens
# Recreate token with 'repo' scope
# Update .  Renviron
# Restart R
```

#### Rate Limit Exceeded
```
Error: 403 Forbidden (rate limit exceeded)
```

**Check status:**
```r
check_rate_limit <- function() {
  token <- Sys.getenv("GITHUB_PAT", unset = NA)
  
  if(! is.na(token)) {
    req <- httr::GET(
      "https://api.github.com/rate_limit",
      httr::add_headers(Authorization = paste("Bearer", token))
    )
  } else {
    req <- httr::GET("https://api.github.com/rate_limit")
  }
  
  rate_data <- httr::content(req, as = "parsed")
  
  message("Limit: ", rate_data$rate$limit)
  message("Remaining: ", rate_data$rate$remaining)
  message("Resets at: ", as.POSIXct(rate_data$rate$reset, origin = "1970-01-01"))
}

check_rate_limit()
```

**Solution:**
- With token: 5000 requests/hour (usually sufficient)
- Without token: 60 requests/hour (add token!)
- Wait until reset time if exceeded

### Test Script: `test_github_auth.R`

Save this for testing authentication:

```r
test_github_authentication <- function() {
  message("\n╔═══════════════════════════════════════════╗")
  message("║   Testing GitHub Authentication Setup    ║")
  message("╚═══════════════════════════════════════════╝\n")
  
  all_pass <- TRUE
  
  # Test 1: Check token exists
  message("Test 1: Checking for GITHUB_PAT...")
  token <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (is.na(token) || token == "") {
    message("  ❌ GITHUB_PAT not set")
    message("  Solution: usethis::edit_r_environ()\n")
    return(FALSE)
  }
  message("  ✓ GITHUB_PAT is set\n")
  
  # Test 2: Test public repo
  message("Test 2: Testing public repo access...")
  tryCatch({
    pub_req <- httr::GET("https://api.github.com/repos/octocat/Hello-World")
    if(httr::status_code(pub_req) == 200) {
      message("  ✓ Public repo access works\n")
    }
  }, error = function(e) {
    message("  ❌ Failed: ", e$message, "\n")
    all_pass <<- FALSE
  })
  
  # Test 3: Test authenticated user
  message("Test 3: Testing token authentication...")
  tryCatch({
    auth_req <- httr::GET(
      "https://api.github.com/user",
      httr::add_headers(Authorization = paste("Bearer", token))
    )
    
    if (httr::status_code(auth_req) == 200) {
      user <- httr::content(auth_req)
      message("  ✓ Token valid")
      message("    Authenticated as: @", user$login, "\n")
    } else {
      message("  ❌ Token invalid (status ", httr::status_code(auth_req), ")\n")
      all_pass <<- FALSE
    }
  }, error = function(e) {
    message("  ❌ Error: ", e$message, "\n")
    all_pass <<- FALSE
  })
  
  # Test 4: Test private repo
  message("Test 4: Testing private repo access...")
  tryCatch({
    priv_req <- httr::GET(
      "https://api.github.com/repos/Sengenics/iOmeAiFunctions",
      httr::add_headers(Authorization = paste("Bearer", token))
    )
    
    if (httr::status_code(priv_req) == 200) {
      repo <- httr::content(priv_req)
      message("  ✓ Private repo access works")
      message("    Repo: ", repo$full_name, "\n")
    } else {
      message("  ❌ Failed (status ", httr::status_code(priv_req), ")\n")
      all_pass <<- FALSE
    }
  }, error = function(e) {
    message("  ❌ Error: ", e$message, "\n")
    all_pass <<- FALSE
  })
  
  # Summary
  if (all_pass) {
    message("╔═══════════════════════════════════════════╗")
    message("║        ✓ ALL TESTS PASSED                ║")
    message("╚═══════════════════════════════════════════╝\n")
  } else {
    message("╔═══════════════════════════════════════════╗")
    message("║        ❌ SOME TESTS FAILED               ║")
    message("╚═══════════════════════════════════════════╝\n")
  }
  
  return(all_pass)
}

# Run if called directly
if (! interactive()) {
  test_github_authentication()
}
```

**Usage:**
```r
source("test_github_auth.R")
test_github_authentication()
```

### R Package Issues

#### Package Not Found
```r
# Install missing package
install.packages("package_name")

# Or for Bioconductor packages
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install("Biobase")
```

#### Module Not Loading from Package
```r
# Check if package is installed
"iOmeAiFunctions" %in% installed.packages()[,"Package"]

# Reinstall from source
devtools::install_github("Sengenics/iOmeAiFunctions")

# Or local install
devtools::install_local("path/to/iOmeAiFunctions")
```

---

## 💡 Team Preferences

### Development Environment
- **OS:** macOS (using MacPorts, not Homebrew)
- **Shell:** zsh
- **IDE:** RStudio (primary), VS Code (occasional)
- **Terminal:** iTerm2 or RStudio terminal

### Code Style
- **Style Guide:** tidyverse style guide
- **Formatting:** Use styler package
- **Naming:** snake_case for functions and variables
- **Documentation:** Roxygen2 with comprehensive examples

### Git Workflow
- **Main branch:** `main` (protected)
- **Dev branch:** `develop` for active development
- **Feature branches:** `feature/description-here`
- **Commit messages:** Clear, descriptive, present tense
- **PR reviews:** Required before merge to main

### File Organization
- Simple, flat structure until complexity demands otherwise
- No deeply nested directories
- Clear naming conventions
- Date formats: `YYYYMMDD` for files, `YYYYMM` for folders
- Time format: 24-hour clock (HH:MM:SS)

### Testing
- Prioritize for package functions
- Develop standalone apps for module testing
- Use testthat for unit tests
- Manual testing in both InHouse and Public apps

### Documentation
- Comprehensive roxygen2 with examples
- Separate basic and advanced mode examples
- Include edge cases in documentation
- Keep `copilot_context.md` updated for patterns
- Use storyboard for development progress

---

## 🔄 Maintenance Schedule

### Daily
- [ ] Log substantial work in storyboard
- [ ] Commit and push code changes
- [ ] Review open issues

### Weekly
- [ ] Review `copilot_context.md` for needed updates
- [ ] Check sprint progress
- [ ] Update dashboard with latest repo data
- [ ] Team sync meeting

### Monthly
- [ ] Update project documentation
- [ ] Review and archive old storyboard entries
- [ ] Check GitHub token expiration dates
- [ ] Package version updates

### Quarterly
- [ ] Major documentation review
- [ ] Rotate GitHub tokens (90-day expiration)
- [ ] Archive completed projects
- [ ] Technology stack review

---

## 📞 When You're Stuck

### Code Issues
1. Check `copilot_context.md` for documented patterns
2. Review recent storyboard entries for similar work
3. Look at reference implementation (e.g., denoiser app)
4. Use debug button in RStudio
5. Ask team in Slack

### Setup Issues
1. Check this guide first
2. Run `test_github_auth.R` for token issues
3. Verify R and package versions
4. Check git configuration
5. Contact IT for server/deployment issues

### Project Direction Questions
1. Review current sprint objectives
2. Check project README files
3. Look at recent storyboard decisions
4. Discuss in team meeting
5. Document decision in storyboard

---

## 📚 Additional Resources

### Internal
- `copilot_context.md` - AI coding patterns and standards
- `README. md` files in each repository
- Storyboard entries for project history
- Team Slack channels

### External
- [Shiny Documentation](https://shiny.rstudio.com/)
- [tidyverse Style Guide](https://style.tidyverse.org/)
- [R Packages Book](https://r-pkgs. org/)
- [GitHub API Documentation](https://docs.github.com/en/rest)

---

**Last Updated:** 2025-11-26  
**Maintained By:** Shaun Garnett (@DrGarnett)  
**Questions? ** Check Slack or create an issue