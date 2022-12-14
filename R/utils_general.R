
#' Retrieve the `ns` function
#' @description Designed to find the `ns` function from any level of nesting within a UI or Server module. `r lifecycle::badge("experimental")`
#' @family general
#' @param e \code{env}
#' @return \code{fun}
#' @export


ns_find <- function(e = rlang::caller_env()) {
  tries <- rlang::exprs(
    get0("ns", inherits = FALSE),
    {
      i <- 1
      need_mod = TRUE
      while (need_mod) {
        # Whats the call associated with each frame
        exp <- rlang::expr_deparse(rlang::caller_call(i))
        # Stop at the first frame called by a module
        need_mod <- !stringr::str_detect(exp[1], "^mod")
        if (need_mod)
          i <- i + 1
        # If we get all the way to the handler when searching from the UI side, give up. If we're searching on the server side, valueFunc comes first in the call stack, so break and return NULL to go onto the next method.
        if (stringr::str_detect(exp[1], UU::regex_or(prefix = "^", c("valueFunc", "handler"))) || i > 10)
          break

      }
      # Get the ns from the first module environment
      get0("ns", envir = rlang::caller_env(i), inherits = FALSE)
    },
    shiny::getDefaultReactiveDomain()$ns,
    {
      UU::gwarn("Could not find ns function. Using {.code function(x) x}")
      out <- function(x) x
    }
  )
  out <- NULL
  i <- 1
  while (!rlang::is_function(out)) {
    out <- rlang::eval_bare(tries[[i]], env = e)
    i <- i + 1
  }

  return(out)
}

#' Create a custom namespace string.
#' @description Useful for referencing objects nested in adjacent modules
#' @details No `-` can be used in user supplied namespace names as this is the separator shiny uses to separate namespace sections and is how this function splits apart sections.
#' @param levels \code{num} indicating what segments of the namespace to keep.
#' @param add \code{chr} segments to append to the result
#' @param .ns \code{fun} `ns` function
#'
#' @return \code{chr} The custom namespace string
#' @export
#'
#' @examples
#' .ns <- function(x) paste0("body-vulnerability-vuln_num_summary-vuln_summary_nums", "-", x)
#' ns_custom(-2, add = "blah", .ns = .ns)
ns_custom <- function(levels, add = NULL, .ns = ns_find()) {
  segs <- stringr::str_split(.ns(""), "\\-")[[1]]
  retain <- if (sign(levels) == -1) {
    1:(length(segs) + levels)
  } else {
    1:levels
  }
  glue::glue_collapse(c(segs[retain], add), sep = "-")
}

#' Extract the tabname to which this module instance corresponds from the ns
#' @family general
#' @param ns_fun \code{fun} `ns` found within the module
#'
#' @return \code{chr} the namespace as string
#' @export
#'

tab_ns_extract <- function(extract, which = c("first", "last", "all")[1], ns_fun = ns_find(e), e = rlang::caller_env()) {
  if (!missing(extract)) {
    out <- stringr::str_extract_all(ns_fun(""), UU::regex_or(extract, prefix = "(?<=-)", suffix = "(?=-?)"))[[1]]
  } else {
     out <- stringr::str_split(ns_fun(""), "\\-")[[1]]
     out <- out[nzchar(out)]
  }

  switch(which,
         first = dplyr::first(out),
         last = dplyr::last(out),
         all = out)
}


#' Add the \link[shiny]{observeEvent} call for a hidden ui browser button.
#' @seealso browser_ui, golem::browser_button
#' @family general
#' @param e The environment from which this function is called. **Default** \link[rlang]{caller_env} which typically works as is.
#' @export

browser_server <- function(e = rlang::caller_env()) {
  observeEvent(input$browser, {
    browser()
  }, event.env = e, handler.env = e)
}

#' Add the invisible browser button ui component
#'
#' @param ns \code{(function)} the `ns` function. Typically called from the parent environment, but in some cases where this function is heavily nested it may need to be provided directly.
#' @family general
#' @return \code{(shiny.tag.list)} with the browser button and the script that hides it.
#' @export
browser_ui <- function(.ns = ns_find()) {
  force(.ns)
  id <- .ns('browser')
  UU::gmsg("Use $('#{id}').show() in JS console for browser button")
  tagList(
    bs4Dash::actionButton(.ns("browser"), "browser"),
    tags$script(type = "text/javascript", shiny::HTML(glue::glue("$('#{id}').hide();"))),
  )
}

#' Display a warning in a DOM element of choice
#'
#' @param warn_text \code{chr/shiny.tags} HTML or shiny.tags to render to HTML to display in the DOM
#' @param id \code{chr} ID of the DOM element in which to display
#' @inheritDotParams shinyjs::html
#' @inheritParams shinyjs::delay
#' @family general
#' @return A warning on the specified DOM element
#' @export

warn_id <- function(warn_html, id, ..., ms = 10000) {
  warn_text <- htmltools::doRenderTags(warn_html)
  if (shiny::isRunning()) {
    shinyjs::html(id = id, html = warn_text, ...)
    shinyjs::delay(ms, shinyjs::html(id, html = NULL))
  } else
    UU::gwarn(warn_text)
  FALSE
}

#' Strip a file path to everything after resourcepath
#' @description Useful for linking to internal files, such as with image source attributes `<img src="[path]">`
#' @param path \code{chr}
#' @param resourcepath \code{chr} A resource path specified in _app_ui.R_
#'
#' @return \code{chr} without stripped directories
#' @export
#' @family general
#' @examples
#' path_strip_to("inst/app/www/img/myimage.svg", "www")
path_strip_to <- function(path, resourcepath = "www", leading_slash = FALSE) {
  out <- stringr::str_replace(path, paste0(".*(?=\\",paste0(.Platform$file.sep, resourcepath),")"), "")
  if (!leading_slash)
    out <- stringr::str_sub(out, start = 2)
  return(out)
}


#' Strip a file path to everything after resourcepath if shiny is running
#'
#' @inherit path_strip_to params return description
#' @family general
#' @export
#' @examples
#' path_strip_shiny("inst/app/www/img/image.jpg")

path_strip_shiny <- function(path, resourcepath = "www", leading_slash = FALSE) {
  if (stringr::str_detect(path, resourcepath) && (golem::is_running() || shiny::isRunning()))
    path_strip_to(path, resourcepath, leading_slash = leading_slash)
  else
    path

}



#' @title Display the name of the parent module
#' @description Stack traces often aren't available in Shiny. Use this function inside of modules to know where errors occur
#' @return Message with the name of the running module and it's environment
#' @export

msg_mod_fun <- function(.call = rlang::trace_back(bottom = 5), e = rlang::caller_env()) {
  if (getOption("use_debug", FALSE)) {
    cli::cli_text(cli::col_br_cyan("NS: ", e$ns("|")),cli::col_br_blue("call: {.code {rlang::expr_deparse(.call$call[[length(.call$call)]])}}|"),cli::col_br_black("env:{.code {rlang::env_label(e)}}"))
  }

}

#' Insert module debugging statements throughout module files.
#' @param pattern \code{chr} The pattern to search for in files that will be modified.
#' @return Modifies all files beginning with `pattern` insert \link[shinyVirga]{msg_mod_fun}
#' @export
#'

use_msg_mod_fun <- function(pattern = "^mod") {
  list.files("R", pattern = pattern, full.names = TRUE) |>
    purrr::walk(~{
      file <- readLines(.x)
      s_line <- stringr::str_which(file, "ns \\<\\- session\\$ns")
      lines_to_check <- file[s_line:(s_line+3)]
      if (!any(stringr::str_detect(lines_to_check, "shinyVirga\\:\\:msg\\_mod\\_fun"))) {
        blanks <- UU::zchar(trimws(lines_to_check))
        if(any(blanks)) {
          file[s_line + which(blanks)[1] - 1] <- "   shinyVirga::msg_mod_fun()"
        } else {
          file <- append(file, "    shinyVirga::msg_mod_fun()", after = s_line)
        }

        write(file, .x)
      }

    })

}
