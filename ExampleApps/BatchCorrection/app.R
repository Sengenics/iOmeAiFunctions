# Main app file for Batch Testing App

# Source global, ui, and server
source("global.R")
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)