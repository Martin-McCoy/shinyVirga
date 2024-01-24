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
#'
#' @return \code{none} Modifies in place
#' @export
#'

rv_modify <- function(x, ...) {
  .dots <- rlang::dots_list(...)
  nms <- names(.dots)
  for (i in seq_along(.dots)) {
    x[[nms[i]]] <- .dots[[i]]
  }
}


#' Convert a reactiveValues object to a list
#' @description Convenience wrapper around \link[shiny]{reactiveValuesToList}
#' @inherit shiny::reactiveValuesToList
#' @importFrom shiny reactiveValuesToList
#' @export
#' @family reactives

rv_to_list <- function(x, all.names = FALSE) {
  if (inherits(x, "reactiveValues"))
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
