# Shiny App Module Development Context

**Version:** 1.0  
**Last Updated:** 2025-12-09  
**Purpose:** Standard patterns for building Shiny app modules with consistent, minimal UI design

---

## 🎯 PURPOSE OF THIS DOCUMENT

This file contains **approved patterns and standards** for building Shiny modules.  When starting a new session or building new modules, provide this file to establish: 

1. **UI Design Standards** - How modules should look and behave
2. **Code Patterns** - Tested, reusable templates
3. **Common Pitfalls** - What NOT to do

**For AI Assistants:** Follow these patterns exactly.  Don't "improve" or "simplify" unless explicitly asked.

---

## ⚠️ CRITICAL RULES

### Before Writing Any Code: 

1. **Check if a pattern exists** - Is there already a documented way to do this?
2. **Use existing patterns** - Don't create new solutions when one is documented here
3. **Ask before deviating** - If user request contradicts a pattern, ask why
4. **Keep it simple** - Don't overcomplicate solutions
5. **Test parameters** - Ensure `show_*` and `debug` parameters work as expected

### Common Mistakes to Avoid: 

❌ Creating complex solutions when simple ones exist  
❌ Ignoring documented patterns  
❌ Adding spaces in variable names (`data.  frame` → `data.frame`)  
❌ Putting debug buttons outside expanded sections  
❌ Using fixed widths instead of conditional widths  
❌ Misaligning info icons with inputs  

---

## 📦 CORE DATA STRUCTURES

### ExpressionSet (Primary Data Structure)

```r
# From Biobase package - STANDARD for all omics data
eset <- ExpressionSet(
  assayData = exprs,           # Expression matrix (features × samples)
  phenoData = pData,           # Sample metadata
  featureData = fData,         # Feature annotations
  experimentData = experimentData,
  annotation = "platform_name"
)

# Access methods
Biobase::exprs(eset)                      # Expression data
Biobase::pData(eset)                      # Sample metadata
Biobase::fData(eset)                      # Feature metadata
Biobase::assayDataElementNames(eset)      # Available assays
Biobase::assayDataElement(eset, "name")   # Get specific assay
```

### ExpSet_list (Multi-Assay Container)

```r
# STANDARD for apps with multiple datasets/assays
ExpSet_list <- list(
  sample_data = <ExpressionSet>,
  clinical_data = <ExpressionSet>,
  reference_data = <ExpressionSet>
)

# Each ExpressionSet can contain multiple assays: 
# - exprs (default expression data)
# - normalized_data
# - batch_corrected_data
# - etc.
```

---

## 🎨 SHINY MODULE STANDARD PATTERN

### Basic Module Structure

```r
#' Module UI Function
#'
#' @param id Character.  Namespace identifier. 
#' @param debug Logical. Show debug button (default FALSE).
#' @return UI elements
#' @export
module_name_ui <- function(id, debug = FALSE) {
  ns <- NS(id)
  
  tagList(
    # UI elements using ns()
    
    # Debug button (if enabled)
    if (debug) {
      actionButton(
        ns("debug"),
        "Debug: module_name",
        icon = icon("bug"),
        class = "btn-warning btn-sm"
      )
    }
  )
}

#' Module Server Function
#'
#' @param id Character. Namespace identifier. 
#' @param eset Reactive or static ExpressionSet.
#' @param debug Logical. Enable debug mode (default FALSE).
#' @return List of reactive expressions
#' @export
module_name_server <- function(id, eset, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Debug observer
    if (debug) {
      observeEvent(input$debug, {
        message("🔍 DEBUG MODE - module_name")
        message("  Available objects:")
        message("  • eset()")
        message("  • input$[input_name]")
        browser()
      })
    }
    
    # Module logic here
    
    # Return values
    return(list(
      output_value = reactive({ ...  })
    ))
  })
}
```

---

## 🎛️ MINIMAL DROPDOWN MODULE PATTERN

### Design Philosophy

**Objective:** Create clean, minimal selectors that look like standard inputs with optional expandable information.

**Visual Structure:**
```
┌─────────────────────────────────────┬───┐
│ Select Item:             [Dropdown ▼] │ ⓘ │  ← Minimal, inline
└─────────────────────────────────────┴───┘
          │
          │ (Click ⓘ to expand)
          ▼
┌─────────────────────────────────────────┐
│ ┌─────────────────────────────────────┐ │
│ │ [Collapsible Box:  Summary]         │ │
│ │ [Collapsible Box: Details]         │ │
│ │ [Debug Button] (if debug=TRUE)     │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

### Core Principles

1. ✅ **Default state:** Just dropdown + info icon (no visible box)
2. ✅ **Info icon alignment:** Use `padding-top: 25px` to align with input label
3. ✅ **Full width control:** Dropdown takes 11 cols with info, 12 cols without
4. ✅ **Expandable content:** Wrapped in visible box when expanded
5. ✅ **Debug at bottom:** Debug button inside expanded section
6. ✅ **Consistent spacing:** Use `margin-top: 15px` for expanded content

---

### Template:  Minimal Selector Module

```r
#' Minimal Selector Module - UI
#'
#' Clean dropdown selector with optional info panel
#'
#' @param id Character. Module namespace ID.
#' @param label Character. Label for the selector (default "Select Item: ").
#' @param help_text Character. Optional help text below selector.
#' @param show_summary Logical. Show summary box in expanded view (default TRUE).
#' @param show_info Logical. Show info bubble (default TRUE).
#' @param debug Logical. Show debug button (default FALSE).
#' @export
mod_minimal_selector_ui <- function(id,
                                    label = "Select Item:",
                                    help_text = NULL,
                                    show_summary = TRUE,
                                    show_info = TRUE,
                                    debug = FALSE) {
  ns <- NS(id)
  
  tagList(
    # ✅ Minimal inline selector with optional info icon
    fluidRow(
      column(
        width = if (show_info) 11 else 12,  # Full width if no info
        uiOutput(ns("selector_ui"))
      ),
      if (show_info) {
        column(
          width = 1,
          style = "padding-top: 25px;",  # Align with input label
          actionLink(
            ns("toggle_details"),
            icon("info-circle", class = "fa-lg"),
            style = "color: #337ab7;"
          )
        )
      }
    ),
    
    # ✅ Collapsible details (only if show_info = TRUE)
    if (show_info) {
      conditionalPanel(
        condition = "input.toggle_details % 2 == 1",
        ns = ns,
        
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              
              # Summary section
              if (show_summary) {
                box(
                  title = "Selection Summary",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  verbatimTextOutput(ns("selection_summary"))
                )
              },
              
              # Debug button (at bottom of expanded section)
              if (debug) {
                fluidRow(
                  column(
                    width = 12,
                    style = "margin-top: 10px;",
                    actionButton(
                      ns("debug"),
                      "Debug: mod_minimal_selector",
                      icon = icon("bug"),
                      class = "btn-warning btn-sm",
                      style = "width:  100%;"
                    )
                  )
                )
              }
            )
          )
        )
      )
    }
  )
}

#' Minimal Selector Module - Server
#'
#' @param id Character. Module namespace ID.
#' @param choices Reactive or static vector of choices.
#' @param selected Reactive or static default selection.
#' @param multiple Logical. Allow multiple selection (default FALSE).
#' @param label Character. Label for the selector. 
#' @param help_text Character. Help text to display.
#' @param debug Logical. Enable debug mode (default FALSE).
#' @export
mod_minimal_selector_server <- function(id,
                                        choices,
                                        selected = NULL,
                                        multiple = FALSE,
                                        label = "Select Item:",
                                        help_text = NULL,
                                        debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ✅ Debug observer
    if (debug) {
      observeEvent(input$debug, {
        message("🔍 DEBUG MODE - mod_minimal_selector")
        message("  • Label: ", label)
        message("  • Multiple: ", multiple)
        message("  • Selected: ", paste(input$selected_item, collapse = ", "))
        browser()
      })
    }
    
    # Get choices (handle reactive or static)
    choices_reactive <- reactive({
      if (is.reactive(choices)) {
        choices()
      } else {
        choices
      }
    })
    
    # Get selected (handle reactive or static)
    selected_reactive <- reactive({
      if (is.reactive(selected)) {
        selected()
      } else {
        selected
      }
    })
    
    # Render selector
    output$selector_ui <- renderUI({
      avail_choices <- choices_reactive()
      default_sel <- selected_reactive()
      
      if (length(avail_choices) == 0) {
        return(
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            strong(" No items available")
          )
        )
      }
      
      tagList(
        selectInput(
          ns("selected_item"),
          label,
          choices = avail_choices,
          selected = default_sel,
          multiple = multiple,
          width = "100%"
        ),
        if (!is.null(help_text)) {
          helpText(help_text)
        }
      )
    })
    
    # Selection summary
    output$selection_summary <- renderPrint({
      req(input$selected_item)
      
      selected <- input$selected_item
      
      cat("═══════════════════════════════════════════════\n")
      cat("SELECTION", if (multiple) "S", "\n", sep = "")
      cat("═══════════════════════════════════════════════\n\n")
      
      if (multiple) {
        cat("Number selected:", length(selected), "\n\n")
      }
      
      for (item in selected) {
        cat("  • ", item, "\n", sep = "")
      }
      
      cat("\n═══════════════════════════════════════════════\n")
    })
    
    # Return selected item(s)
    return(list(
      selected = reactive({ input$selected_item })
    ))
  })
}
```

---

### Implementation Rules

#### 1. Info Icon Alignment

```r
# ✅ CORRECT - Aligns with input label
column(
  width = 1,
  style = "padding-top: 25px;",  # Magic number for standard selectInput
  actionLink(ns("toggle_details"), icon("info-circle", class = "fa-lg"))
)

# ❌ WRONG - Will be misaligned
column(width = 1, actionLink(ns("toggle_details"), icon("info-circle")))
```

#### 2. Width Control

```r
# ✅ CORRECT - Adjusts based on info icon presence
width = if (show_info) 11 else 12

# ❌ WRONG - Fixed width
width = 11
```

#### 3. Expanded Content Structure

```r
# ✅ CORRECT - Wrapped in box for visual clarity
conditionalPanel(
  condition = "input.toggle_details % 2 == 1",
  ns = ns,
  fluidRow(
    column(width = 12, box(width = NULL, ...))
  )
)

# ❌ WRONG - No visual container
conditionalPanel(
  condition = "input.toggle_details % 2 == 1",
  ns = ns,
  # Content directly here without box
)
```

#### 4. Debug Button Placement

```r
# ✅ CORRECT - At bottom, full width, inside expanded section
if (debug) {
  fluidRow(
    column(
      width = 12,
      style = "margin-top: 10px;",
      actionButton(
        ns("debug"),
        "Debug: [module_name]",
        icon = icon("bug"),
        class = "btn-warning btn-sm",
        style = "width: 100%;"
      )
    )
  )
}

# ❌ WRONG - Outside expanded section
if (debug) {
  actionButton(ns("debug"), "Debug")
}
```

---

## 📋 USAGE EXAMPLES

### Example 1: Column Selector

```r
# UI
mod_minimal_selector_ui(
  "column_select",
  label = "Select Column:",
  help_text = "Choose a metadata column",
  show_info = TRUE,
  debug = TRUE
)

# Server
column_selector <- mod_minimal_selector_server(
  "column_select",
  choices = reactive(colnames(Biobase::pData(eset()))),
  selected = "Labels",
  multiple = FALSE,
  label = "Select Column:",
  help_text = "Choose a metadata column",
  debug = TRUE
)

# Usage
selected_column <- column_selector$selected()
```

### Example 2: Assay Selector (Minimal - No Info)

```r
# UI - Clean, no info bubble
mod_minimal_selector_ui(
  "assay_select",
  label = "Select Assay:",
  show_info = FALSE,  # Just dropdown
  debug = FALSE
)

# Server
assay_selector <- mod_minimal_selector_server(
  "assay_select",
  choices = reactive(Biobase::assayDataElementNames(eset())),
  selected = "exprs",
  multiple = FALSE,
  label = "Select Assay:"
)

# Usage
selected_assay <- assay_selector$selected()
```

### Example 3: Multi-Select Batch Factors

```r
# UI
mod_minimal_selector_ui(
  "batch_select",
  label = "Batch Factors:",
  help_text = "Select one or more batch factors",
  show_summary = TRUE,
  show_info = TRUE,
  debug = TRUE
)

# Server
batch_selector <- mod_minimal_selector_server(
  "batch_select",
  choices = reactive(filtered_columns()),
  selected = reactive(c("Batch_ID", "Assay")),
  multiple = TRUE,
  label = "Batch Factors:",
  help_text = "Select one or more batch factors",
  debug = TRUE
)

# Usage
selected_batches <- batch_selector$selected()
```

---

## 🔧 CUSTOMIZATION GUIDE

When adapting the minimal selector pattern: 

### 1. Change Summary Output

```r
# Customize what shows in expanded summary
output$selection_summary <- renderPrint({
  req(input$selected_item)
  
  # Your custom summary logic
  cat("Custom Summary:\n")
  cat("Selected:", input$selected_item, "\n")
  
  # Add metadata, statistics, warnings, etc.
})
```

### 2. Add Additional Sections

```r
# Add more boxes in expanded view
box(
  width = NULL,
  
  # Summary
  if (show_summary) {
    box(title = "Summary", ...)
  },
  
  # ✅ NEW: Add custom section
  box(
    title = "Validation",
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    uiOutput(ns("validation_output"))
  ),
  
  # Debug button
  if (debug) { ... }
)
```

### 3. Use Different Input Types

```r
# Replace selectInput with other inputs
output$selector_ui <- renderUI({
  tagList(
    # Option 1: pickerInput (for enhanced multi-select)
    shinyWidgets::pickerInput(
      ns("selected_item"),
      label,
      choices = avail_choices,
      selected = default_sel,
      multiple = multiple,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE
      )
    ),
    
    # Option 2: radioButtons (for single selection with visible options)
    radioButtons(
      ns("selected_item"),
      label,
      choices = avail_choices,
      selected = default_sel
    ),
    
    # Option 3: checkboxGroupInput (for multiple selection with visible options)
    checkboxGroupInput(
      ns("selected_item"),
      label,
      choices = avail_choices,
      selected = default_sel
    )
  )
})
```

### 4. Add Validation

```r
# Add validation warnings in expanded view
output$validation_output <- renderUI({
  req(input$selected_item)
  
  # Check for issues
  if (length(input$selected_item) == 0) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      strong(" Please select at least one item")
    )
  } else if (length(input$selected_item) > 5) {
    div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(" Note: "), "Many items selected.  This may slow down processing."
    )
  } else {
    div(
      class = "alert alert-success",
      icon("check-circle"),
      strong(" Selection valid")
    )
  }
})
```

---

## 🚫 DO NOT

- ❌ Put info icon on separate row (breaks alignment)
- ❌ Use fixed pixel heights for alignment (use `padding-top`)
- ❌ Make debug button visible when `debug = FALSE`
- ❌ Forget to wrap expanded content in a box
- ❌ Hard-code `width = 11` (use conditional)
- ❌ Place debug button outside expanded section
- ❌ Use complex parsing when simple rendering works
- ❌ Create new patterns when documented ones exist

## ✅ DO

- ✅ Keep default state minimal (just dropdown + icon)
- ✅ Use `conditionalPanel` for expandable content
- ✅ Place debug button at bottom of expanded section
- ✅ Make info bubble optional via `show_info` parameter
- ✅ Test with and without info bubble enabled
- ✅ Handle both reactive and static inputs
- ✅ Provide clear, descriptive debug button labels
- ✅ Document all parameters in roxygen

---

## 📚 TECHNOLOGY STACK

**Core Packages:**
- `shiny` - Web framework
- `shinydashboard` - Dashboard layout
- `Biobase` - ExpressionSet data structure
- `shinyWidgets` - Enhanced inputs (pickerInput, etc.)
- `DT` - Interactive tables
- `ggplot2` - Static plots
- `plotly` - Interactive plots

**Standard Libraries:**
```r
library(shiny)
library(shinydashboard)
library(Biobase)
library(tidyverse)  # dplyr, ggplot2, tidyr, purrr
library(DT)
```

---

## 🔄 UPDATE HISTORY

| Date | Version | Changes |
|------|---------|---------|
| 2025-12-09 | 1.0 | Initial version with minimal dropdown pattern |

---

## 📞 QUESTIONS TO ASK WHEN CODING

**For AI Assistants:**

1. ✅ Does a documented pattern exist for this? 
2. ✅ Am I following the minimal dropdown pattern correctly?
3. ✅ Are info icon and debug button in the right places?
4. ✅ Have I tested both `show_info = TRUE` and `show_info = FALSE`?
5. ✅ Does the module handle both reactive and static inputs?
6. ✅ Is the debug observer implemented if `debug = TRUE`?
7. ✅ Am I overcomplicating this solution? 

**When to Update This Document:**

- ✅ A new UI pattern is approved
- ✅ A common mistake is identified
- ✅ A reusable module template is created
- ✅ An existing pattern needs refinement

---

**This is a living document. Update it as patterns evolve and new standards emerge.**
