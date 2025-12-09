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

## COPILOT INTERACTION

# Effective Code Collaboration Pattern for Copilot

## Context
This pattern was developed during iterative refinement of a Shiny module UI.  It proved highly effective for collaborative debugging and incremental improvements. The core principle is to present code changes as targeted snippets with clear find/replace patterns, rather than dumping entire files.  Only show complete functions when creating new code or performing major refactors (>50% changes).

## Standard Format for Code Edits

When making small changes (<50 lines), use this structure:  Start with "## ✅ [Brief description of what's being fixed]", then show "FIND THIS:" with exact code to locate (include 5-10 lines of context), followed by "REPLACE WITH:" showing new code with ✅ comments marking key changes. Include "Location:" with file path, function name, or line number reference.  End with "## 💡 Why this works:" with 1-2 sentence explanation of the problem and solution.  For large changes or new code, use "## ✅ [Description]" followed by "REPLACE the entire [function/section name] with:" and the complete new code, with location reference.

## Guidelines
Always include exact context by showing the literal code to find, not a paraphrase. Mark changes clearly using ✅ inline comments to highlight what's new or changed. Explain the why with a brief explanation of what problem this solves. Present one change at a time and don't bundle multiple unrelated fixes in one snippet. Use visual markers where emoji help scan quickly: ✅ for good changes, ❌ for wrong approaches, ⚠️ for cautions, 💡 for insights, and 🎯 for goals.  Show impact by including before/after structure diagrams for UI changes. Verify locations by referencing function names, line numbers, or unique code patterns. 

## Benefits and Anti-Patterns
This approach reduces cognitive load because users only see what changed. It prevents errors through clear find/replace patterns that eliminate ambiguity. It maintains context by not losing surrounding code and enables verification so it's easy to confirm changes applied correctly.  It supports iteration by building solutions incrementally and facilitates learning so users understand each change.  Avoid showing entire files when only 5 lines changed, using vague locations like "near the top" or "in the server function", making changes without explanation, assuming users remember previous code states, or bundling multiple unrelated changes together.

## Example Workflow and Visual Structure
Follow this workflow:  User reports specific issue, Copilot identifies root cause, show minimal snippet to fix it with clear find/replace, explain why this solves the problem, show impact (performance, UX, etc.), then move to next issue iteratively.  For UI reorganization, show layout diagrams using box-drawing characters to help users visualize the end result before implementing changes, with arrows and descriptions showing element hierarchy and states like collapsed or expanded sections.

## Session Consistency
When working on the same code across multiple interactions, reference previous changes (e.g., "building on the parallel processing we just added"), track state (e.g., "now that auto-run toggle is working"), maintain terminology consistency, and remember user preferences (e.g., "as you mentioned, put X in advanced options"). This pattern creates a collaborative debugging experience where the user and Copilot build solutions together incrementally, rather than a "replace your code with mine" exchange.  It respects the user's understanding of their codebase while providing clear, actionable guidance. 

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

# Shiny Module UI Design Pattern - Minimal with Expandable Advanced Options

## Pattern Overview
This is the standard UI pattern for analysis modules in the app. It prioritizes a clean, minimal interface with the main visualization always visible, while providing full functionality through expandable advanced options.  This pattern was developed for the batch confounding analysis module and should be replicated for similar analysis modules.

## Core Design Principles
1. **Visualization First** - The main plot/output is always visible at the top, taking priority in the layout
2. **Progressive Disclosure** - Advanced settings are hidden by default but easily accessible via a single button
3. **Hierarchical Organization** - Within advanced options, content is organized in collapsible boxes by purpose:  interpretation guide (collapsed), results table (collapsed), settings (always open), debug tools (if enabled)
4. **Overlay Controls** - Download buttons and similar controls overlay the visualization in the top-right corner to save space
5. **Self-Contained Boxes** - Each functional area (guide, table, settings) lives in its own shinydashboard box with appropriate status colors

## Standard Structure Template

The UI should follow this exact hierarchy:

```r
mod_[module_name]_ui <- function(id, show_auto_run_toggle = TRUE, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "[Module Title]",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				# SECTION 1: Main visualization with spinner
				shinycssloaders:: withSpinner(
					plotOutput(ns("main_plot"), height = "700px"),
					type = 4,
					color = "#337ab7"
				),
				
				# SECTION 2: Overlay download button (top-right, icon only)
				div(
					style = "position:  absolute; top: 45px; right: 5px; z-index: 1000;",
					downloadButton(
						ns("download_plot"),
						label = NULL,
						icon = icon("download"),
						class = "btn-primary btn-sm",
						style = "opacity: 0.9;",
						title = "Download plot as PNG"
					)
				),
				
				hr(),
				
				# SECTION 3: Advanced options toggle button
				fluidRow(
					column(
						width = 12,
						actionButton(
							ns("toggle_advanced"),
							"Advanced Options & Settings",
							icon = icon("cog"),
							class = "btn-default",
							style = "width:  100%; margin-bottom: 15px;"
						)
					)
				),
				
				# SECTION 4: Collapsible advanced panel
				conditionalPanel(
					condition = "input.toggle_advanced % 2 == 1",
					ns = ns,
					
					box(
						width = 12,
						collapsible = FALSE,
						
						# Subsection 4.1: Interpretation Guide (collapsed)
						box(
							title = "Interpretation Guide",
							width = 12,
							status = "info",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							# [Content explaining how to read the visualization]
						),
						
						# Subsection 4.2: Results Table (collapsed, with download)
						box(
							title = "Results Table",
							width = 12,
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							
							# Download button right-aligned
							fluidRow(
								column(
									width = 12,
									div(
										style = "text-align: right; margin-bottom: 10px;",
										downloadButton(
											ns("download_table"),
											"Download Table (TSV)",
											icon = icon("download"),
											class = "btn-primary btn-sm"
										)
									)
								)
							),
							
							# Status and help text
							uiOutput(ns("analysis_status")),
							helpText("[Method descriptions]"),
							
							hr(),
							
							# Data table
							DTOutput(ns("results_table"))
						),
						
						# Subsection 4.3: Settings (NOT collapsible, always visible when advanced is open)
						box(
							title = "Analysis Settings",
							width = 12,
							status = "primary",
							solidHeader = TRUE,
							collapsible = FALSE,
							
							# Row 1: Main analysis parameters (3-4 columns)
							fluidRow(
								column(width = 4, [selectInput or numericInput]),
								column(width = 4, [selectInput or numericInput]),
								column(width = 4, [selectInput or numericInput])
							),
							
							hr(),
							
							# Row 2: Toggles and secondary options
							fluidRow(
								column(width = 4, [selectInput]),
								column(
									width = 4,
									div(
										style = "padding-top: 5px;",
										shinyWidgets:: materialSwitch(
											inputId = ns("use_parallel"),
											label = "Enable Parallel Processing",
											value = TRUE,
											status = "success"
										)
									),
									helpText(icon("bolt"), tags$small("[Help text]"))
								),
								if (show_auto_run_toggle) {
									column(
										width = 4,
										div(
											style = "padding-top: 5px;",
											shinyWidgets::materialSwitch(
												inputId = ns("auto_run_analysis"),
												label = "Auto-Run Analysis",
												value = TRUE,
												status = "success"
											)
										),
										helpText(icon("sync"), tags$small("[Help text]"))
									)
								}
							),
							
							# Manual run button (conditional on auto-run being OFF)
							if (show_auto_run_toggle) {
								fluidRow(
									column(width = 12, uiOutput(ns("manual_run_ui")))
								)
							}
						),
						
						# Subsection 4.4: Debug button (if enabled)
						if (debug) {
							actionButton(
								ns("debug"),
								"Debug:  mod_[module_name]",
								icon = icon("bug"),
								class = "btn-warning",
								style = "width:  100%;"
							)
						}
					)
				)
			)
		)
	)
}


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
