#' R List to CSS declarations as a string
#' @param ... \code{chr} Named css properties, IE characters in the from `property = value`
#' @param declarations \code{list} CSS Declarations as a named list ie  `list(property = value)`
#'
#' @return \code{chr} properties as CSS character with newlines
#' @export
#' @family ui
#' @family css
#' @examples
#' css_props(display = "none", declarations = list(margin = "0px"))

css_props <- function(..., declarations) {
  .dots <- rlang::dots_list(..., .named = TRUE)
  declarations <- rlang::list2(!!!.dots, !!!declarations)
  glue::glue_collapse(purrr::imap_chr(declarations, ~glue::glue("{.y}: {.x};")), sep = "\n")
}


#' Translate inline Sass to CSS style tag
#'
#' @param sass \code{chr} with Sass
#' @inheritParams glue::glue
#'
#' @return \code{shiny-tag}
#' @export
#' @family ui
#' @family css
#' @examples
#' ns <- function(x) paste0("blah-ble-",x)
#' add_sass("#|ns('myID')| > button {
#'   height: 10px;
#'   & option {
#'     text-align: center;
#'   }
#' }")
add_sass <- function(sass,
                     .open = "|",
                     .close = "|"
                     , .envir  = rlang::caller_env()) {
  tags$style(type = "text/css",
             HTML(sass::sass(
               glue::glue(
                 sass,
                 .envir = .envir,
                 .open = .open,
                 .close = .close
               )
             )))
}
