library(shiny)
require(shinyjs)
devtools::load_all()
ui <- fluidPage(
  shinyVirga::use_shinyVirga(),
  shinyVirga::use_driver.js(),
  shiny::actionButton("my-id", "Tooltip on this button")
)

server <- function(input, output, session) {
  js_callout('my-id', "Tooltip title", "Tooltip description", showButtons = "close")

}

shinyApp(ui, server)
