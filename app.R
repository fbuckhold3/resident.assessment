library(shiny)

source("R/global.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
