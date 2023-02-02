#' Wrapper for \link[shiny]{icon} that supports svg images
#'
#' @param name \code{chr} FontAwesome name or image path
#' @inherit shiny::icon params return
#' @param inactive_path \code{chr} path to the icon image file in it's inactive (unselected) state, starting with `www`.
#' @param active_path \code{chr} path to the icon image file in it's active (selected) state, starting with `www`.
#' @family ui
#' @export

icon_sb <- function(name, class = NULL, lib = "font-awesome", inactive_path = NULL, active_path = NULL, inactive_props = NULL, active_props = inactive_props, ...) {
  msg_nm <- missing(name)
  if (msg_nm && !is.null(class) && is.null(inactive_path)) {
    UU::gwarn("{.code class} supplied but no {.code inactive_path} supplied. If these are img icons, please supply {.code inactive_path} with the path to the image.")
  }
  out <- if (!msg_nm && UU::`%|try|%`(file.exists(name), FALSE)) {
    shiny::img(src = path_strip_to(name, "www"),
               class = class,
               ...)
  } else if (msg_nm) {
    shiny::img(class = class, ...)
  } else
    shiny::icon(
      name = name,
      class = class,
      lib = lib,
      ...,
      verify_fa = FALSE
    )
  # Make CSS when icons are images
  css = c()
  if (!is.null(inactive_path)) {
    # These wierd paths (../..) for content:url are necessary to make the images work on shinyapps.io
    css <- UU::glue_js(
      "
   .*{class}* {
     content:url('*{fs::path('../..', inactive_path)}*');
    *{css_props(declarations = inactive_props)}*
   }"
   )
  }

  if (!is.null(active_path)) {
    css <- c(css, UU::glue_js(
      "
   .active .*{class}* {
     content:url('*{fs::path('../..', active_path)}*');
     *{css_props(declarations = active_props)}*
     }"
    ))
  }
  if (UU::is_legit(css)) {
    out <- htmltools::tagAppendChild(
      out,
      htmltools::tags$style(
        type = "text/css",
        glue::glue_collapse(css, sep = "\n")
      )
    )
  }
  return(out)
}

#' FontAwesome Arrow Icons
#'
#' @param direction \code{chr} One of \code{`r glue::glue_collapse(eval(rlang::fn_fmls(fa_arrow_icon)$direction), ", ")`}
#' @inheritParams shiny::icon
#' @return \code{shiny.tag}
#' @export
#' @family ui
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


#' A small info icon with a tooltip
#' @description Requires \link[tippy]{use_tippy} to be placed in the `head` of the app. See the [tippy docs]{https://github.com/JohnCoene/tippy} for details
#' @param ... \code{shiny.tag.list/shiny.tag}s to be rendered as the tooltip
#' @inheritParams shiny::icon
#' @family ui
#' @return \code{shiny.tag} tippy enabled info icon
#' @export
#'
#' @examples
#' infoIcon(tags$p("Help text here"))
infoIcon <- function(..., name = "info-circle", class = NULL, lib = "font-awesome") {
  .dots <- rlang::dots_list(...)
  tippy::tippy(shiny::icon(name, class = class, lib = lib, verify_fa = FALSE), htmltools::doRenderTags(
    rlang::exec(
      tagList,
      !!!.dots
    )
  ), interactive = TRUE, allowHTML = TRUE)
}
