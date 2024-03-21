#' @inherit shiny::reactiveValues
#' @family reactives
#' @importFrom shiny reactiveValues
#' @export
rv <- function(...) {
  shiny::reactiveValues(...)
}

#' Create a \code{\link[shiny]{reactiveVal}} or a `nonreactiveVal` depending on whether shiny is running
#'
#' @param val \code{obj} any initial value
#'
#' @return \code{reactiveVal/nonreactiveVal}
#' @export
#'
#' @examples
#' new_rv(0)
new_rv <- function(val = NULL) {
  if (shiny::isRunning())
    reactiveVal(val)
  else
    nonreactiveVal(val)
}
#' Index into a reactiveValues object and return a list
#'
#' @param x \code{reactiveValues}
#' @param indices \code{chr/num} valid indexing object
#' @family reactives
#' @return \code{reactiveValues/list} depending on the value of `assign`
#' @export

rv_index <- function(x, indices, ...) {
  if (is.character(indices) && is.null(names(indices)))
    indices <- rlang::set_names(indices)
  out <- purrr::map(indices, \(.x) x[[.x]])
  return(out)
}

#' Add named objects to a `reactiveValues` object
#'
#' @inheritParams rv_index
#' @param ... \code{named objects} to assign when `assign = TRUE`
#' @param modify_in_place \code{lgl} modifies the reactiveValues object in place, making it such that assignment is not necessary. If `FALSE`, the default, a list copy of the reactiveValues are made and returned. Useful for checking expected output.
#' @return \code{none} Modifies in place, unless `modify_in_place = FALSE`
#' @export
#'

rv_modify <- function(x, ..., modify_in_place = TRUE, e = rlang::caller_env()) {

  if (modify_in_place) {
    out <- rlang::enexpr(x)
    quos <- rlang::enexprs(...)
    fn <- rlang::eval_tidy
  } else {
    out <- shinyVirga::rv2l(x)
    quos <- rlang::dots_list(...)
    ex <- rlang::expr({out[[nms[i]]] <- quos[[i]]})
    e <- environment()
    fn <- rlang::eval_bare
  }

  nms <- names(quos)
  for (i in seq_along(nms)) {
    if (modify_in_place)
      ex <- rlang::expr({(!!out)[[(!!nms[i])]] <- !!quos[[i]]})
    fn(ex, env = e)
  }
  return(out)
}


#' Convert a reactiveValues object to a list
#' @description Convenience wrapper around \link[shiny]{reactiveValuesToList}
#' @inherit shiny::reactiveValuesToList
#' @importFrom shiny reactiveValuesToList
#' @export
#' @family reactives

rv_to_list <- function(x, all.names = FALSE) {
  if (inherits(x, "reactivevalues"))
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

.nonreactiveVals <- new.env()

#' Create a non-reactiveVal
#' @description
#'  these are useful in conjunction with `req` to evaluate whether a reactive should run while preventing reactive flush or chaining when the internal value changes
#' @aliases nrV
#' @inheritParams shiny::reactiveVal
#'
#' @return \code{obj} Whatever object was last stored
#' @export
#'
#' @examples
#' my_nrv <- nonreactiveVal(5)
#' my_nrv()
nonreactiveVal <- function(value = NULL) {
  address <- paste0(".", rlang::hash(rnorm(3)))
  if (!is.null(value))
    assign(address, value, envir = .nonreactiveVals)
  out <- rlang::new_function(
    args = rlang::pairlist2(value = ),
    body = rlang::expr({
      if (!missing(value))
        assign(!!address, value, envir = .nonreactiveVals)
      get0(!!address, envir = .nonreactiveVals)
    })
  )
  reg.finalizer(rlang::fn_env(out), rlang::new_function(args = rlang::pairlist2(x=), body = rlang::expr({
    suppressWarnings(rm(!!address, envir = .nonreactiveVals))
  })), onexit = TRUE)

  out
}
nrV <- nonreactiveVal
