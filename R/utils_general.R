
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
    if (!is.null(ms)) {
      shinyjs::delay(ms, shinyjs::html(id, html = NULL))
    }
  } else
    UU::gwarn(warn_text)
  FALSE
}



shiny.tag_map <- function(x, name = NULL, attribs = NULL, previous) {
  fns <- purrr::compact(list(
    name = name,
    attribs = attribs
  ))

  if (!rlang::is_empty(x$attribs)) {

  }
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
#' @description
#' The usage of a pseudo-module is nearly identical to the module, but inputs & outputs in the pseudo-module are shared between the parent and the pseudo-module.  It's usefulness shines when developing numerous interacting inputs, say for a complex plot, and those inputs need be modular but also must interact with one another in a confluence fashion to determine how the plot renders.
#' The pseudo-module allows developers to functionalize input components whose values must necessarily must be considered in concert with one another. Using modules, the developer must keep track of shared inputs explicitly between modules using \code{\link[shiny]{reactiveVal}} and/or \code{\link[shiny]{reactiveValues}} and pass them as arguments to any nested modules. As a codebase changes, and inputs are added and removed, keeping track of where confluences of interactions are stored and updating arguments accordingly is cumulative technical debt. With pseudo-modules shared reactive inputs allow the developer to logically segment interactivity involving confluences of inputs as they see fit without incurring the technical debt of passing \code{\link[shiny]{reactiveVal}} and/or \code{\link[shiny]{reactiveValues}} around when interactions are removed or added.
#'
#' @param name \code{chr} name of the module
#' @param path \code{chr} directory in which to create it
#' @param export \code{lgl} whether it should be exported in the package
#' @param pm_name \code{chr} The name of file
#' @param pm_ui \code{chr} The name of the module ui function
#' @param pm_server \code{chr} The name of the module server function
#' @param use_ud \code{lgl} Whether to include the `session$userData` environment as a shorthand variable `ud` in the module for ease of access to a user's session state variables.
#' @return \code{file} The new file opens
#' @export


add_pseudo_module <- function (name,
                               path = "R",
                               export = FALSE,
                               pm_name = sprintf("pmod_%s", name),
                               pm_ui = sprintf("pmod_%s_ui", name),
                               pm_server = sprintf("pmod_%s_server", name),
                               use_ud = TRUE,
                               open = TRUE)
{
  file_path <- fs::path(path, pm_name, ext = "R")
  write_there <- function(..., path = file_path) {
    write(..., file = path, append = TRUE)
  }
  session_arg <- if (use_ud)
    ", session = shiny::getDefaultReactiveDomain()"
  else
    ""
  write_there(sprintf("#' %s UI Function", name))
  write_there("#' @description A shiny Module.")
  write_there("#' @param .ns \\code{fun} ns function. Typically found automatically.")
  if (export) {
    write_there(sprintf("#' @rdname %s", pm_ui))
    write_there("#' @export ")
  } else {
    write_there("#' @noRd ")
  }
  write_there("#' @importFrom shiny tagList ")
  write_there(sprintf("%s <- function(.ns = shinyVirga::ns_find()%s){", pm_ui, session_arg))
  write_there("  ns <- force(.ns)")
  if (use_ud)
    write_there("  ud <- session$userData")
  write_there("  tagList(")
  write_there("    ")
  write_there("  )")
  write_there("}")
  write_there("    ")
  if (utils::packageVersion("shiny") < "1.5") {
    write_there(sprintf("#' %s Server Function", name))
    if (export) {
      write_there(sprintf("#' @rdname %s", pm_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(parent_env = rlang::caller_env()){", pm_server))
    write_there("  session <- shiny::getDefaultReactiveDomain()")
    write_there("  ns <- session$ns")
    write_there("  input <- session$input")
    write_there("  output <- session$output")
    if (use_ud)
      write_there("    ud <- session$userData")
    write_there("}")
    write_there("    ")
    write_there("## To be copied in the UI")
    write_there(sprintf("# %s()", pm_ui))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf("# %s())",
                        pm_server))
  } else {
    write_there(sprintf("#' %s Server Functions", name))
    if (export) {
      write_there(sprintf("#' @rdname %s", pm_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(){",
                        pm_server))

    write_there("  session <- shiny::getDefaultReactiveDomain()")
    write_there("  ns <- session$ns")
    write_there("  input <- session$input")
    write_there("  output <- session$output")
    if (use_ud)
      write_there("    ud <- session$userData")
    write_there("  ")
    write_there("}")
    write_there("    ")
    write_there("## To be copied in the UI")
    write_there(sprintf("# %s()", pm_ui))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf("# %s()", pm_server))
  }
  if (open)
    rstudioapi::documentOpen(file_path)
}


#' Add a module file to the `path` specified
#' @param m_name \code{chr} The name of file
#' @param m_ui \code{chr} The name of the module ui function
#' @param m_server \code{chr} The name of the module server function
#' @inheritParams add_pseudo_module
#' @export
add_module <- function(
    name,
    path = "R",
    export = FALSE,
    m_name = sprintf("mod_%s", name),
    m_ui = sprintf("mod_%s_ui", name),
    m_server = sprintf("mod_%s_server", name),
    use_ud = TRUE,
    open = TRUE,
    ...
) {
  file_path <- fs::path(path, m_name, ext = "R")
  write_there <- function(..., path = file_path) {
    write(..., file = path, append = TRUE)
  }
  session_arg <- if (use_ud)
    ", session = shiny::getDefaultReactiveDomain()"
  else
    ""
  write_there(sprintf("#' %s UI Function", name))
  write_there("#'")
  write_there("#' @description A shiny Module.")
  write_there("#'")
  write_there("#' @param id,input,output,session Internal parameters for {shiny}.")
  write_there("#'")
  if (export) {
    write_there(sprintf("#' @rdname %s", m_name))
    write_there("#' @export ")
  } else {
    write_there("#' @noRd ")
  }
  write_there("#'")
  write_there("#' @importFrom shiny NS tagList ")
  write_there(sprintf("mod_%s_ui <- function(id%s){", name, session_arg))
  write_there("  ns <- NS(id)")
  if (use_ud)
    write_there("  ud <- session$userData")
  write_there("  tagList(")
  write_there("    ")
  write_there("  )")
  write_there("}")
  write_there("    ")

  if (utils::packageVersion("shiny") < "1.5") {
    write_there(sprintf("#' %s Server Function", m_server))
    write_there("#'")
    if (export) {
      write_there(sprintf("#' @rdname %s", m_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(input, output, session){", m_server))
    write_there("  ns <- session$ns")
    if (use_ud)
      write_there("  ud <- session$userData")
    write_there("  ")
    write_there("}")
    write_there("    ")

    write_there("## To be copied in the UI")
    write_there(sprintf('# mod_%s_ui("%s_1")', name, name))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf('# callModule(mod_%s_server, "%s_1")', name, name))
  } else {
    write_there(sprintf("#' %s Server Functions", name))
    write_there("#'")
    if (export) {
      write_there(sprintf("#' @rdname ", m_server))
      write_there("#' @export ")
    } else {
      write_there("#' @noRd ")
    }
    write_there(sprintf("%s <- function(id){", m_server))
    write_there("  moduleServer( id, function(input, output, session){")
    write_there("    ns <- session$ns")
    if (use_ud)
      write_there("    ud <- session$userData")
    write_there("    ")
    write_there("  })")
    write_there("}")
    write_there("    ")

    write_there("## To be copied in the UI")
    write_there(sprintf('# mod_%s_ui("%s_1")', name, name))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf('# mod_%s_server("%s_1")', name, name))
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
#' @inheritParams shiny::updateActionButton
#' @return \code{msg}
#' @export
#'

dbg_msg <- function(x, e = rlang::caller_env(), session = shiny::getDefaultReactiveDomain()) {
  if (getOption("use_debug", FALSE)) {
    msg <- cli::format_inline(x, .envir = e)
    cli::cli_text(cli::col_br_magenta(msg))
    if (shiny::isRunning() && inherits(session, "ShinySession")) {
      UU::need_pkg("shinyjs","runjs")(UU::glue_js("console.log('Debug: *{msg}*')"))
    }
  }
}
