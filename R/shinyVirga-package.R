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

use_shinyVirga <- function() {
  shiny::tagList(
    htmltools::htmlDependency(
      name = "shinyVirga-funs",
      version = utils::packageVersion("shinyVirga"),
      package = "shinyVirga",
      src = "srcjs",
      script = list(src = "shinyVirga-funs.js")
    ),
    shinyjs::useShinyjs()
  )
}
