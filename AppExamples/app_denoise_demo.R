# Updated Code for app_denoise_demo.R

# CSS Section (after line 82)
.ui {
    ...
    .upload-box {
        border: 2px solid #00a65a;
    }
}

# UI Data Selection Tab (insert after line 89)
# Create a collapsible box titled "Import New ExpSet Data"
ui <- fluidPage(
    ...
    collapsibleBox(title = "Import New ExpSet Data", open = TRUE,
        fileInput('expSetFile', 'Choose ExpSet File',
                  accept = c('.rds', '.RDS')),
        actionButton('loadExpSet', 'Load File'),
        uiOutput('uploadStatus'),
        tags$ul(
            tags$li('File must be an .rds or .RDS file'),
            tags$li('Ensure the file is formatted correctly')
        )
    ),
    ...
)

# Update text on line 98
textOutput('dataSourceText')
# Change to:
output$dataSourceText <- renderText({
    "The app will auto-load from data/ExpSet_list.rds or use your uploaded file."
})

# Server Function (insert after line 383)
server <- function(input, output, session) {
    uploaded_expset <- reactiveVal(NULL)
    upload_status <- reactiveVal('')
    upload_success <- reactiveVal(FALSE)

    observeEvent(input$loadExpSet, {
        req(input$expSetFile)
        tryCatch({
            validate(need(grepl('\.rds$', input$expSetFile$name), 'Please upload a valid .rds file.'))
            uploaded_expset(readRDS(input$expSetFile$datapath))
            upload_status('File uploaded successfully!')
            upload_success(TRUE)
        }, error = function(e) {
            upload_status(paste('Upload failed:', e$message))
            upload_success(FALSE)
        })
    })

    output$uploadStatus <- renderUI({
        status <- upload_status()
        if (upload_success()) {
            tags$div(style='color: green;', status)
        } else {
            tags$div(style='color: red;', status)
        }
    })

    # Modify ExpSet_list reactive (line 385)
    ExpSet_list <- reactive({
        if (!is.null(uploaded_expset())) {
            return(uploaded_expset())
        } else {
            # original logic to load from default paths
            ...
        }
    })

    # Update status boxes
    output$statusBox <- renderText({
        if (upload_success()) {
            "Uploaded ExpSets"
        } else {
            "ExpressionSets Loaded"
        }
    })

    # Update eset_summary output
    output$eset_summary <- renderPrint({
        if (!is.null(uploaded_expset())) {
            cat('Data Source: Uploaded')
        } else {
            cat('Data Source: Default')
        }
    })

    # Add uploaded_expset() to debug mode available objects
    debug_mode <- reactive({
        list(uploaded_expset = uploaded_expset())
    })

    # Update Help tab to include file upload instructions
    output$helpText <- renderText({
        "To upload a file, click the 'Upload New ExpSet Data' button and select your file."
    })
}

# Existing functionality remains intact

