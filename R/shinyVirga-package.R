#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL


#' Adds JS Dependencies to the DOM for shinyVirga to function
#'
#' @return \code{tagList}
#' @export
#'

useShinyVirga <- function() {
  shiny::tagList(
    htmltools::htmlDependency(
      name = "shinyVirga-funs",
      version = as.character(utils::packageVersion("shinyVirga")),
      package = "shinyVirga",
      src = "js",
      script = "shinyVirga-funs.js"
    )
  )
}
