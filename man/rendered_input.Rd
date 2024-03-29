% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_ui.R
\name{rendered_input}
\alias{rendered_input}
\title{Set a Shiny input to \code{TRUE} when an element renders}
\usage{
rendered_input(x, id)
}
\arguments{
\item{x}{\code{shiny.tag} Must have an ID attribute or an ID must be supplied to \code{id}}

\item{id}{\code{chr} ID to appended to the element, must include \code{ns} if inside a module.}
}
\value{
\code{shiny.tag} with script appended.
}
\description{
\strong{Requires use_shinyVirga}. Adds a script to the tag that will observe when the element becomes visible in the DOM and set a Shiny input value with the id of the element plus \verb{_rendered}. If the id of \code{x} is \code{my-button}, the input will be \code{input$my-button_rendered}.
}
\examples{
\dontrun{
library(shiny)
library(shinyVirga)
ui <- fluidPage(
  use_shinyVirga(),
  rendered_input(
    actionButton("my-button", "My Button")
  )

)

server <- function(input, output, session) {
  observeEvent(input$`my-button_rendered`, {
    print("The button rendered")
  })
}

shinyApp(ui, server)
}
}
\seealso{
\code{\link[=use_shinyVirga]{use_shinyVirga()}}
}
