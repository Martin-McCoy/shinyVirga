#' R List to CSS declarations as a string
#' @param ... \code{chr} Named css properties, IE characters in the from `property = value`
#' @param declarations \code{list} CSS Declarations as a named list ie  `list(property = value)`
#'@param inline \code{lgl} Should CSS Properties be in inline format?
#' @return \code{chr} properties as CSS character with newlines
#' @export
#' @family ui
#' @family css
#' @examples
#' css_props(display = "none", declarations = list(margin = "0px"))

css_props <- function(..., declarations = NULL, inline = FALSE) {
  .dots <- rlang::dots_list(..., .named = TRUE)
  declarations <- rlang::list2(!!!.dots, !!!declarations)
  paste0(purrr::imap_chr(declarations, ~glue::glue("{.y}: {.x};")), collapse = ifelse(inline, "","\n"))
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

#' Conver a list or vector to Sass variables
#'
#' @param x \code{list/chr}
#' @param pre \code{chr} prefix to prepend
#' @param suf \code{chr} suffix to append
#' @family css
#' @return \code{chr}
#' @export
#'
#' @examples
#' list2sass(pre = "dark-", list(
#'   lightblue = "rgba(18,180,211,1)",
#'   darkblue = "rgba(2,120,170,1)",
#'   navyblue = "rgba(0,57,73,1)",
#'   brown = "rgba(72,36,18,1)",
#'   lightbrown = "rgb(132,96,78)",
#'   purple = "rgba(111,96,140,1)",
#'   lightpurple = "rgba(165,150,194,1)",
#'   darkcyan = "rgba(0,166,212,1)",
#'   darkturquoise = "rgba(9,119,168,1)"
#' )) |> cat(sep = '\n')
list2sass <- function(x, pre = "", suf = "") {
  paste0("$", pre, names(x), suf, ": ", x,";")
}
