% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_js.R
\name{render_ran_input}
\alias{render_ran_input}
\title{Create an incrementing shiny input each time a render function runs}
\usage{
render_ran_input(
  id,
  htmlwidget,
  asis = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  debugger = FALSE
)
}
\arguments{
\item{id}{\code{chr} the id of the output for which a new input with the name \verb{[id]_ran} (without brackets) will be incremented each time it renders.}

\item{htmlwidget}{\code{htmlwidget} If the render function is rendering an htmlwidget, it can be passed. This obviates the need for shinyjs.}

\item{asis}{If \code{TRUE}, use the ID as-is even when inside a module
(instead of adding the namespace prefix to the ID).}

\item{debugger}{\code{lgl} whether to add a debugger statement to the callback.}
}
\value{
Runs code via shinyjs unless an htmlwidget is provided, in which case it modifies the htmlwidget with \link[htmlwidgets]{onRender} and returns it.
}
\description{
Requires \code{\link[shinyjs]{runjs}} if not using \code{htmlwidget}. Useful for firing an observer after a render function completes to update the page number for page number retention. \strong{Warning}: using the \code{htmlwidget} does not necessarily increment the input each time the R \code{render} statement runs since \code{htmlwidgets} has it's own internal heuristics for when to refresh an rendered UI element.
}
\examples{
\dontrun{
output[[outputId]] <- renderDT({
  ## DT CODE
  render_ran_input(outputId)
})
}

}
