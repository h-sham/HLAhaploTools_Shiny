## Clean-up the environment before a new run
gc()
rm(list = ls())

# Initialize global, server, and user interface
source("R/global.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

# Run app
shinyApp(ui = ui, server = server)
