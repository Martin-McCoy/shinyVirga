#' @inherit shiny::reactiveValues
#' @family reactives
#' @importFrom shiny reactiveValues
#' @export
rv <- function(...) {
  shiny::reactiveValues(...)
}


#' Index into a reactiveValues object and return a list
#'
#' @param x \code{reactiveValues}
#' @param indices \code{chr/num} valid indexing object
#' @param assign \code{lgl} whether to assign the values in `indices`to their respective names in `x`
#' @family reactives
#' @return \code{reactiveValues/list} depending on the value of `assign`
#' @export

rv_index <- function(x, indices, assign = FALSE) {
  if (is.character(indices) && is.null(names(indices)))
    indices <- rlang::set_names(indices)
  if (assign)
    out <- purrr::iwalk(indices, ~`<<-`(x[[.y]], .x))
  else
    out <- purrr::map(indices, ~x[[.x]])
  return(out)
}

#' Convert a reactiveValues object to a list
#' @description Convenience wrapper around \link[shiny]{reactiveValuesToList}
#' @inherit shiny::reactiveValuesToList
#' @importFrom shiny reactiveValuesToList
#' @export
#' @family reactives

rv_to_list <- function(x, all.names = FALSE) {
  if (!is.null(x))
    shiny::reactiveValuesToList(x, all.names = all.names)
  else
    x
}

#' @rdname rv_to_list
#' @export
rv2l <- rv_to_list
#' @export
#' @rdname rv_to_list
rvtl <- rv_to_list
