# GitHub Copilot Context for KREX Development

**Last Updated:** 2026-03-02  
**Organization:** KREX  
**Purpose:** AI coding assistant context - patterns, standards, and structures

> **For Developers:** See `developer_guide.md` for setup instructions, project management, and troubleshooting. 

---

## ⚠️ CRITICAL: READ THIS FIRST

### For GitHub Copilot
Before answering ANY request:
1. **READ this entire file** - Don't assume, don't guess
2. **CHECK for existing patterns** - Is there already a documented way to do this?
3. **QUESTION requests that contradict standards** - If the user asks for something that violates documented patterns, ASK WHY
4. **FOLLOW documented patterns exactly** - Don't "improve" or "be flexible" unless explicitly instructed
5. **WHEN IN DOUBT** - Reference this file and ask for clarification

---

## 🚫 COMMON MISTAKES TO AVOID

### 1. DON'T Overcomplicate Solutions
❌ **Bad:** Parsing markdown into sections, extracting headers, reconstructing content  
✅ **Good:** Read entire file → Render as HTML  

❌ **Bad:** Creating complex data structures when simple ones exist  
✅ **Good:** Use documented data structures from this file  

### 2. DON'T Ignore Documented Patterns
❌ **Bad:** "Let me create a new way to do this"  
✅ **Good:** "I see we have a pattern for this in the context file. Let me use that."  

### 3. DON'T Create New Patterns When One Exists
❌ **Bad:** Writing custom parsing logic for markdown files  
✅ **Good:** Using the documented markdown rendering pattern  

### 4. DON'T Add Spaces in Variable Names
❌ **Bad:** `data.  frame` or `github. com`  
✅ **Good:** `data.frame` or `github.com`  

**This is a recurring error - always verify no spaces are added to code identifiers or URLs**

### 5. DON'T Forget to Restart R After Setting Environment Variables
❌ **Bad:** Edit `.Renviron`, immediately try to use token  
✅ **Good:** Edit `.Renviron`, **RESTART R**, then use token  

---

## 📋 DECISION-MAKING FRAMEWORK

```
1. Does a documented pattern exist for this?
   → YES: Use it
   → NO: Proceed to step 2

2. Does this contradict any documented standard?
   → YES: Stop and ask the user for clarification
   → NO: Proceed to step 3

3. Is this a common operation that should become a standard?
   → YES: Suggest documenting it in this file
   → NO: Implement as requested

4. After implementation, should this be documented?
   → YES: Remind user to update this file
   → NO: Continue
```

---

## 📦 Data Structures (CRITICAL - USE THESE)

### Main Data Structure
```r
# Primary data structure - USE THIS, DON'T CREATE NEW ONES
datCollate <- list(
  manifest = <data.frame>,      # Sample information
  data = list(
    RawData = <data.frame>,     # GPR raw data (Sample, X, Y, FG, BG, NetI, Protein, Block, Row, Column, Flags)
    Data = <data.frame>          # Protein annotations (Protein, data, flag, num_test)
  ),
  param = list(
    Wavelength = <character>     # "532" or "635"
  )
)

dat <- list(
  data = list(
    gpr = <list>                 # GPR file data by name
  )
)

QC <- list(
  # QC metrics and flagging data
)

# ExpSet_list - NEW STANDARD for modular apps
ExpSet_list <- list(
  eset1 = <ExpressionSet>,      # Biobase ExpressionSet object
  eset2 = <ExpressionSet>,
  # ... additional esets
)
```

---

## 🎨 Shiny Module Pattern (REQUIRED STANDARD)

### All modules MUST follow this pattern:

```r
#' Module UI Function
#'
#' @param id Character string. Namespace identifier.
#' @return UI elements
#' @export
module_name_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements using ns()
  )
}

#' Module Server Function
#'
#' @param id Character string. Namespace identifier.
#' @param data_input Reactive or static. Input data structure.
#' @param mode Character or reactive. "basic" or "advanced". Default "basic".
#' @param features List or reactive. Feature flags controlling functionality.
#'
#' @return Reactive expression or NULL
#' @export
#'
#' @details
#' This module supports two operational modes:
#' - **Basic Mode**: Simplified UI and limited features for general users
#' - **Advanced Mode**: Full functionality for expert users
#'
#' Feature flags can be toggled independently of mode for fine-grained control.
#'
#' @examples
#' \dontrun{
#' # Basic usage (Public app)
#' module_name_server(
#'   "my_module",
#'   data_input = reactive(my_data),
#'   mode = "basic",
#'   features = list(feature1 = FALSE, feature2 = FALSE)
#' )
#'
#' # Advanced usage (InHouse app)
#' module_name_server(
#'   "my_module",
#'   data_input = reactive(my_data),
#'   mode = "advanced",
#'   features = list(feature1 = TRUE, feature2 = TRUE)
#' )
#' }
module_name_server <- function(id,
                               data_input,
                               mode = "basic",
                               features = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Normalize mode to reactive
    mode_reactive <- reactive({
      if (is.reactive(mode)) mode() else mode
    })
    
    # Normalize features to reactive
    features_reactive <- reactive({
      if (is.reactive(features)) {
        f <- features()
      } else {
        f <- features
      }
      
      # Merge with defaults
      default_features <- list(
        feature1 = FALSE,
        feature2 = FALSE
      )
      
      for (name in names(default_features)) {
        if (is.null(f[[name]])) {
          f[[name]] <- default_features[[name]]
        }
      }
      
      return(f)
    })
    
    # Module logic here
    # Use mode_reactive() and features_reactive() to control behavior
    
  })
}
```

### Feature Flag Pattern
```r
# Define feature sets in global.R
features_advanced <- list(
  advanced_clustering = TRUE,
  download_plot = TRUE,
  interactive_selection = TRUE
)

features_basic <- list(
  advanced_clustering = FALSE,
  download_plot = TRUE,      # Can selectively enable
  interactive_selection = FALSE
)

# Use in UI rendering
output$dynamic_ui <- renderUI({
  feat <- features_reactive()
  mode <- mode_reactive()
  
  ui_elements <- list()
  
  if (mode == "advanced" || feat$advanced_clustering) {
    ui_elements <- append(ui_elements, list(
      # Advanced UI elements
    ))
  }
  
  # Basic UI elements always shown
  ui_elements <- append(ui_elements, list(
    # Basic UI
  ))
  
  do.call(tagList, ui_elements)
})
```

---

## 📁 Shiny App Structure (STANDARD)

```
app.R (optional - can launch directly from ui.R/server.R)
├── ui.R          # UI definition
├── server.R      # Server logic
├── global.R      # Global variables, feature flags, library loading
├── functions.R   # Helper functions (app-specific only)
├── modules/      # Only for app-specific modules not in package
├── repos_config.yaml (if needed)
├── copilot_context.md (this file)
└── developer_guide.md (human reference)
```

**DO NOT:**
- ❌ Put everything in one file
- ❌ Create complex directory structures without reason
- ❌ Duplicate modules that exist in iOmeAiFunctions package
- ❌ Put core analysis logic in app - belongs in package

---

## 📑 RStudio Code Navigation (REQUIRED STANDARD)

**All R files MUST use section headers for code organization.**

### Section Header Format:
```r
# Level 1: Main Section ====
# Use 4 equal signs for top-level sections

## Level 2: Subsection ----
# Use 2 hashes and 4 dashes for subsections

### Level 3: Sub-subsection ....
# Use 3 hashes and 4 dots for detailed sections
```

### Example Structure:
```r
# Data Loading ====

## Import Functions ----
load_data <- function() { }

## Validation ----
validate_data <- function() { }


# Processing ====

## Quality Control ----
run_qc <- function() { }

## Normalization ----
normalize <- function() { }
```

**Why:**
- ✅ Creates navigable outline in RStudio's document outline (Cmd+Shift+O)
- ✅ Makes large files easy to navigate
- ✅ Standard across all Sengenics code

**Required in:**
- All pipeline scripts
- server.R, ui.R, global.R
- functions.R files
- Package R files

---

## 📝 Markdown Rendering Pattern (CRITICAL - ALWAYS USE THIS)

**When displaying markdown files in Shiny apps:**

```r
output$content <- renderUI({
  # 1. Read entire file
  content <- readLines(filepath, warn = FALSE)
  markdown_text <- paste(content, collapse = "\n")
  
  # 2. Convert to HTML
  html_content <- markdown::markdownToHTML(
    text = markdown_text,
    fragment.only = TRUE
  )
  
  # 3. Render
  HTML(html_content)
})
```

**DO NOT:**
- ❌ Parse markdown into sections
- ❌ Extract specific headers with regex
- ❌ Reconstruct markdown from parsed data
- ❌ Add complexity when simple rendering works

**Why this pattern:**
- ✅ Preserves ALL formatting (code blocks, lists, tables, etc.)
- ✅ Simple and maintainable
- ✅ Consistent across the application

---

## 🐛 Debug Button Pattern (REQUIRED IN ALL SHINY APPS)

```r
# In global.R or top of server.R
debug <- !is.na(Sys.getenv("RSTUDIO", unset = NA))

# In server.R
output$debug_ui <- renderUI({
  if(debug == TRUE){
    actionButton('debug', 'Debug', class = "btn-warning btn-sm")
  }
})

observeEvent(input$debug, {
  browser()
})

# In ui.R (place in header or sidebar)
uiOutput("debug_ui")
```

**Why:**
- Button only appears when running in RStudio
- Click to trigger `browser()` for interactive debugging
- Essential for development workflow

---

## 📦 R Package Functions (iOmeAiFunctions)

### Standards (NO EXCEPTIONS):
- ✅ Roxygen2 documentation (complete with examples)
- ✅ No reactive dependencies in core functions (`values$`, `input$`, `output$`)
- ✅ Modules CAN use Shiny but must handle reactive/static inputs gracefully
- ✅ Accept data structures as parameters
- ✅ Include `@note` for original file location
- ✅ Export with `@export` or document why internal
- ✅ Handle edge cases (NULL, empty data, missing columns)
- ✅ Modules must document mode and feature parameters

### Example Function:
```r
#' Plot pseudo array for foreground intensity
#'
#' @param datCollate List containing data and parameters
#' @param sample_name Character, name of sample to plot
#' @return ggplot object
#' @export
#' @note Original location: server.R line 450
dat_GPR_FG_single_function <- function(datCollate, sample_name) {
  # Function body
}
```

### Example Module:
```r
#' Heatmap Module Server with Feature Flags
#'
#' @param id Character. Namespace identifier.
#' @param data_list Reactive or static. List with m, meta, anno_col, rows.
#' @param mode Character or reactive. "basic" or "advanced".
#' @param features List or reactive. Feature flags.
#'
#' @return Reactive expression with heatmap data.
#' @export
#' @note Original location: i-Ome-AI/server.R
#' @note Version 2.0 - Added mode and feature flag support
heatmap_module_server <- function(id, data_list, mode = "basic", features = list()) {
  # Module implementation
}
```

---

## 🔄 Code Migration Workflow

### Moving Code from App to Package:
1. Identify function/module in InHouse app
2. Refactor to remove hard-coded Shiny dependencies
3. Add mode and features parameters
4. Add roxygen documentation with examples for both modes
5. Add to iOmeAiFunctions package
6. Update InHouse app to use package version (advanced mode)
7. Update Public app to use package version (basic mode)
8. Test in all three contexts (InHouse, Public, Standalone)

**Module Migration Checklist:**
- [ ] Module accepts both reactive and static inputs
- [ ] Mode parameter implemented ("basic"/"advanced")
- [ ] Feature flags defined and documented
- [ ] Works in InHouse app (advanced mode)
- [ ] Works in Public app (basic mode)
- [ ] Roxygen documentation complete
- [ ] Examples provided for both modes

### Migrating Pipelines to Package:

**OLD:** Pipeline with `source("functions.R")` containing analysis functions  
**NEW:** Pipeline using `iOmeAiFunctions` package

**Migration steps:**
1. Copy base_app structure for new pipeline
2. Move analysis functions to iOmeAiFunctions package
3. Replace `source("functions.R")` with `library(iOmeAiFunctions)`
4. Use package functions: `iOmeAiFunctions::function_name()`
5. Validate: old output == new output

**Keep in pipeline (don't move to package):**
- File paths and configs
- One-off data transformations
- Project-specific business logic

---

## 🏗️ Active Projects (Brief Overview)

### 1. iOmeAiFunctions (R Package)
**Also called:** "iOmeAI Package" (same thing - different names used interchangeably)
- Central code repository for protein microarray analysis
- All reusable functions and modules
- Must be package-ready (proper documentation, no hard dependencies)
- **Installation:** `devtools::install_github("Sengenics/iOmeAiFunctions")`

### 2. i-Ome AI InHouse (Shiny App)
- Internal analysis platform
- Uses modules in **Advanced mode**
- Full functionality

### 3. i-Ome AI Public (Shiny App)
- Customer-facing platform
- Uses modules in **Basic mode**
- Simplified UI

### 4. KREX_App_Dashboard (Tracking Dashboard)
- Development tracking
- Storyboard system
- Repository monitoring

### 5. Base App (STARTING POINT FOR ALL NEW APPS)
- **Location:** `inst/ExampleApps/BaseApp` in iOmeAiFunctions package
- **Purpose:** Reference implementation and starting template
- **Use for:** Any new standalone app or pipeline interface

**How to use:**
1. Copy entire base_app structure
2. Rename to your app name
3. Replace module calls with your target modules
4. Update README with your app's purpose
5. Customize UI for your specific needs

**DO NOT:**
- ❌ Modify base_app directly for your project
- ❌ Deviate from the structure without documenting why
- ❌ Copy old app structures - always start from base_app

**Updating base_app:**
- When you discover a better pattern, update base_app first
- Document changes in base_app's README
- Gradually update existing apps to match

---

## 📚 Key Technologies

**Core Stack:**
- **R:** Primary language (version 4.x)
- **Shiny:** Web application framework
- **shinydashboard:** Dashboard framework
- **tidyverse:** Data manipulation (dplyr, ggplot2, tidyr, purrr)
- **Biobase:** ExpressionSet data structures
- **DT:** Interactive tables
- **plotly:** Interactive visualizations
- **roxygen2:** Documentation
- **httr:** GitHub API authentication
- **markdown:** Markdown to HTML conversion
- **Git/GitHub:** Version control

**Authentication:**
- Use `.Renviron` for GitHub tokens (see `developer_guide.md` for setup)
- Token automatically available via `Sys.getenv("GITHUB_PAT")`

---

## 🔑 GitHub API Authentication Pattern

### In Code (Pattern to Use):
```r
# Get token from environment (.Renviron)
github_token <- Sys.getenv("GITHUB_PAT", unset = NA)

# Make authenticated request
if(!is.na(github_token) && nchar(github_token) > 0) {
  response <- httr::GET(
    api_url,
    httr::add_headers(Authorization = paste("Bearer", github_token))
  )
  data <- httr::content(response, as = "parsed")
} else {
  # Fallback for unauthenticated (public repos only)
  warning("No GitHub token found. Only public repos accessible.")
  data <- jsonlite::fromJSON(api_url)
}
```

**Note:** Token setup instructions are in `developer_guide.md`

---

## 📞 Questions to Ask When Making Code

**For GitHub Copilot:**
1. Is there a documented pattern for this in the context file?
2. Does this request contradict any documented standard?
3. Am I overcomplicating this?
4. Have I added spaces where there shouldn't be any?
5. Is this module following the mode/features pattern?
6. Should this go in the package or the app?

---

**Note:** This context file contains only AI-relevant coding patterns. For developer setup, troubleshooting, and project management, see `developer_guide.md`.

**Last reviewed:** 2026-03-02  
**Major patterns:** Modular development with mode/feature switching, data structures, coding standards