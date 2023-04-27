
#' Retrieve the `ns` function
#' @description Designed to find the `ns` function from any level of nesting within a UI or Server module. `r lifecycle::badge("experimental")`
#' @family general
#' @param e \code{env}
#' @return \code{fun}
#' @export


ns_find <- function(e = rlang::caller_env(2)) {
  tb <- rlang::trace_back(bottom = 2)
  calls <- rev(tb$call)
  is_ui <- purrr::some(calls, \(.x) {stringr::str_detect(rlang::call_name(.x), "^mod.*ui" ) %|0|% FALSE})
  tries <- rlang::exprs(
    get0("ns", inherits = FALSE),
    shiny::getDefaultReactiveDomain()$ns,
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
    {
      UU::gwarn("Could not find ns function. Using {.code function(x) x}")
      out <- function(x) x
    }
  )
  out <- NULL
  i <- 1
  if (is_ui) {
    # The call stack search should be in front of the getDefaultReactiveDomain as getDefaultReactiveDomain will typically not return the appropriate ns in the UI
    tries <- tries[c(1,3,2,4)]
  }
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
#' @inheritParams js_after
#' @family general
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
#' @family debugging
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
#' @family debugging
#' @return \code{(shiny.tag.list)} with the browser button and the script that hides it.
#' @export
browser_ui <- function(.ns = ns_find()) {
  force(.ns)
  id <- .ns('browser')
  dbg_msg("Use $('#{id}').show() in JS console for browser button")
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
#' @family debugging
#' @return Message with the name of the running module and it's environment
#' @export

msg_mod_fun <- function(.call = rlang::trace_back(bottom = 5), e = rlang::caller_env()) {
  if (getOption("use_debug", FALSE)) {
    cli::cli_text(cli::col_br_cyan("NS: ", e$ns("|")),cli::col_br_blue("call: {.code {rlang::expr_deparse(.call$call[[length(.call$call)]])}}|"),cli::col_br_black("env:{.code {rlang::env_label(e)}}"))
  }

}



#' Add a pseudo-module, one which maintains the namespace of it's parent module
#'
#' @param name \code{chr} name of the module
#' @param path \code{chr} directory in which to create it
#' @param export \code{lgl} whether it should be exported in the package
#' @param ph_name \code{chr} The name of file
#' @param ph_ui \code{chr} The name of the module ui function
#' @param ph_server \code{chr} The name of the module server function
#' @return \code{file} The new file opens
#' @export

add_pseudo_module <- function (name,
                               path = "R",
                               export = FALSE,
                               ph_name = sprintf("pmod_%s", name),
                               ph_ui = sprintf("pmod_%s_ui", name),
                               ph_server = sprintf("pmod_%s_server", name),
                               open = TRUE)
{
  file_path <- fs::path(path, ph_name, ext = "R")
  write_there <- function(..., path = file_path) {
    write(..., file = path, append = TRUE)
  }
  write_there(sprintf("#' %s UI Function", name))
  write_there("#' @description A shiny Module.")
  write_there("#' @param .ns \\code{fun} ns function. Typically found automatically.")
  if (export) {
    write_there(sprintf("#' @rdname %s", ph_ui))
    write_there("#' @export ")
  } else {
    write_there("#' @noRd ")
  }
  write_there("#' @importFrom shiny tagList ")
  write_there(sprintf("%s <- function(.ns = shinyVirga::ns_find()){", ph_ui))
  write_there("  ns <- force(.ns)")
  write_there("  tagList(")
  write_there("    ")
  write_there("  )")
  write_there("}")
  write_there("    ")
  if (utils::packageVersion("shiny") < "1.5") {
    write_there(sprintf("#' %s Server Function", name))
    if (export) {
      write_there(sprintf("#' @rdname %s", ph_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(){", ph_server))
    write_there("  session <- shiny::getDefaultReactiveDomain()")
    write_there("  ns <- session$ns")
    write_there("  input <- session$input")
    write_there("  output <- session$output")
    write_there("}")
    write_there("    ")
    write_there("## To be copied in the UI")
    write_there(sprintf("# %s()", ph_ui))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf("# %s())",
                        ph_server))
  } else {
    write_there(sprintf("#' %s Server Functions", name))
    if (export) {
      write_there(sprintf("#' @rdname %s", ph_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(){",
                        ph_server))

    write_there("  session <- shiny::getDefaultReactiveDomain()")
    write_there("  ns <- session$ns")
    write_there("  input <- session$input")
    write_there("  output <- session$output")
    write_there("  ")
    write_there("}")
    write_there("    ")
    write_there("## To be copied in the UI")
    write_there(sprintf("# %s()", ph_ui))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf("# %s()", ph_server))
  }
  if (open)
    rstudioapi::documentOpen(file_path)
}

#' Insert module debugging statements throughout module files.
#' @param pattern \code{chr} The pattern to search for in files that will be modified.
#' @return Modifies all files beginning with `pattern` insert \link[shinyVirga]{msg_mod_fun}
#' @family debugging
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

#' Print a debug message to the R and Javascript Console
#'
#' @param x \code{chr} message formatted for \link[cli]{format_inline}
#' @param e \code{env} Environment
#'
#' @return \code{msg}
#' @export
#'

dbg_msg <- function(x, e = rlang::caller_env()) {
  if (getOption("use_debug", FALSE)) {
    msg <- cli::format_inline(x, .envir = e)
    cli::cli_text(cli::col_br_magenta(msg))
    if (shiny::isRunning()) {
      shinyjs::runjs(UU::glue_js("console.log('Debug: *{msg}*')"))
    }
  }
}
