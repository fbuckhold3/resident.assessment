# app.R - Clean version
library(shiny)

# Source files in correct order
source("R/global.R")
source("R/ui.R") 
source("R/server.R")

# Run the app
shinyApp(ui = ui, server = server)
 



