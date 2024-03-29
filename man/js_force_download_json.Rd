% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_js.R
\name{js_force_download_json}
\alias{js_force_download_json}
\title{Force the browser to download JSON, useful for saved session recovery}
\usage{
js_force_download_json(
  x,
  is_file = FALSE,
  filename = "recovery-save-session.json"
)
}
\arguments{
\item{x}{\code{chr/obj} The raw json as a character, a path to a file, or an R object in which case \code{\link[jsonlite]{toJSON}} will be used to convert the object to JSON.}

\item{is_file}{\code{lgl} If x is a file, must be TRUE}

\item{filename}{\code{chr} Name of file to be downlaoded}
}
\value{
\code{none} called for side effects
}
\description{
Useful in combination with \code{options(shiny.error = ...)} for saving a session on a break.
See Examples for details
}
\examples{
\dontrun{
library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  actionButton("break_me", "Break the app")
)

server <- function(input, output, session) {
  options(shiny.error = \() {
  js_force_download_json(jsonlite::toJSON(list(some = "important information")), filename = "test.json")
  })
  observeEvent(input$break_me, {
    rlang::abort("Done broke the app")
  })
}

shinyApp(ui, server)

}
}
