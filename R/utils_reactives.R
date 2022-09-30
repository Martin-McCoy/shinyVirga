#' @inherit shiny::reactiveValues
#' @family reactives
#' @importFrom shiny reactiveValues
rv <- function(...) {
  shiny::reactiveValues(...)
}


#' Index into a reactiveValues object and return a list
#'
#' @param x \code{reactiveValues}
#' @param indices \code{chr/num} valid indexing object
#' @family reactives
#' @return \code{list}
#' @export

rv_index <- function(x, indices) {
  if (is.character(indices))
    indices <- rlang::set_names(indices)
  purrr::map(indices, ~x[[.x]])
}

#' Convert a reactiveValues object to a list
#' @description Convenience wrapper around \link[shiny]{reactiveValuesToList}
#' @inherit shiny::reactiveValuesToList
#' @importFrom shiny reactiveValuesToList
#' @export
#' @family reactives

rv_to_list <- function(x, all.names = FALSE) {
  shiny::reactiveValuesToList(x, all.names = all.names)
}

#' @rdname rv_to_list
rv2l <- rv_to_list
