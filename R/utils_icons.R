#' Wrapper for \link[shiny]{icon} that supports svg images
#'
#' @param name \code{chr} FontAwesome name or image path
#' @inherit shiny::icon params return examples
#'
#' @export

icon_sb <- function(name, class = NULL, lib = "font-awesome", ...) {
  if (!missing(name) && UU::`%|try|%`(file.exists(name), FALSE)) {
    shiny::img(src = path_strip_to(name, "www"),
               class = class,
               ...)
  } else if (missing(name)) {
    shiny::img(class = class, ...)
  } else
    shiny::icon(
      name = name,
      class = class,
      lib = lib,
      ...,
      verify_fa = FALSE
    )
}

#' FontAwesome Arrow Icons
#'
#' @param direction \code{chr} One of \code{`r glue::glue_collapse(eval(rlang::fn_fmls(fa_arrow_icon)$direction), ", ")`}
#' @inheritParams shiny::icon
#' @return \code{shiny.tag}
#' @export
#'
#' @examples
#' fa_arrow_icon("up-down")
fa_arrow_icon <- function(direction = c("up", "down", "left-right", "up-down", "up-down-left-right"), class = NULL, lib = 'font-awesome', ...) {
  direction <- direction[1]
  dashes <- stringr::str_count(direction, "-")
  arrow <- if (dashes > 0)
    "arrows"
  else
    "arrow"
  shiny::icon(glue::glue_collapse(c(arrow, direction), sep = "-"), class = class, lib = lib, ...)
}
