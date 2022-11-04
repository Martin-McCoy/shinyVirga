#' Add a temporary message after an element
#' Message is added directly below element and persists for three seconds
#' @param id \code{(chr)} Automatically namespaced with `ns` if inside a module
#' @param content \code{chr/shiny.tag/shiny.tag.list} Content to add beneath the element
#' @param properties \code{list} of css rules to apply to the parent element of the content. Defaults to jQuery validation styled messages.
#' @param delay \code{dbl} duration in ms before removing the content from the element. Set to 0 to allow the element to persist.
#' @inheritParams shinyjs::html
#' @param .ns \code{fun} ns function. Typically found automatically
#' @family JS
#' @return Content added to element
#' @export
#'

js_after <- function(id,
                     content,
                     status = NULL,
                     properties = list(`font-size` = "12px",
                                       display = "block",
                                       position = "relative"),
                     delay = 3000,
                     asis = FALSE,
                     .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  to_append <- tags$span(
    class = if (!is.null(status))
      paste0("text-", status),
    class = 'js-after',
    style = css_props(declarations = properties),
    content
  )
  html <- htmltools::doRenderTags(to_append)
  the_js <- UU::glue_js("
    $('#*{id}*').after(() => {
      return `*{htmltools::HTML(html)}*`
    })
    ")
  shinyjs::runjs(the_js)
  if (delay)
    shinyjs::delay(delay, shinyjs::hide(selector = '.js-after', asis = TRUE))

}


#' Create an anonymous JS function to monitor an event and bind it to a shiny input
#' @inheritParams js_after
#' @param js \code{(chr)} path to js file with just the anonymous function, or js code to interpolate into a generalized anonymous javascript function that uses glue insertions demarcated by `{{}}`:
#' \preformatted{
#' (e) => {
#'          console.log('{{input}} event');
#'          {{js}}
#'Shiny.setInputValue('{{input}}', {{value}}, {priority: 'event'});
#'};}
#' @param value \code{(js)} arbitrary js code that defines the value to bind. Can also be a variable name if declared in the `js` argument
#' @param ... \code{(additional named arguments)} corresponding to any arguments that need be interpolated with \link[glue]{glue}
#' @inheritParams js_after
#' @return \code{(chr)} with javascript interpolated with \link[glue]{glue} arguments
#' @export
#' @family JS
#' @examples js_callback("widget", "var myValue = e.someProperty;", "myValue")
js_callback <- function(id,
                        js = "",
                        value,
                        ...,
                        asis = FALSE,
                        .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)


  rlang::env_bind(environment(), ...)
  if (fs::is_file(js))
    js_fn <- paste0(readLines(js), collapse = "\n")
  else {
    if (stringr::str_detect(js, ";$", negate = TRUE))
      js <- paste0(js, ";")

    js_fn <- "(e) => {
                        console.log('*{id}* event');
                        *{js}*
                        Shiny.setInputValue('*{id}*', *{value}*, {priority: 'event'});
                                           };"
  }


  UU::as_js(UU::glue_js(js_fn))
}


#' Create a javascript callback that runs when shiny connects.
#'
#' @inheritParams js_callback
#' @param add_tag \code{lgl} whether to enclose in a script tag
#' @return \code{shiny.tag} script tag with javascript
#' @export
#' @family JS
js_set_input_val <- function(id,
                             value,
                             js = NULL,
                             ...,
                             asis = FALSE,
                             as_callback = FALSE,
                             on = NULL,
                             as_tag = FALSE,
                             .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  rlang::env_bind(environment(), ...)

  to_glue <- c("Shiny.setInputValue('*{id}*', *{value}*, {priority: 'event'});")

  if (length(value) > 1)
    value <- jsonlite::toJSON(value)

  if (!is.null(js)) {
    if (stringr::str_detect(js, ";$", negate = TRUE))
      js <- paste0(js, ";")
    to_glue <- c(js, to_glue)
  }


  if (as_callback) {
    to_glue <- c("(e) => {",
      to_glue,
      "}")
  }

  if (!is.null(on))
    to_glue <-
    c("$(document).on('*{on}*', (e) => {",
      to_glue,
      "})")

  out <- UU::glue_js(
    to_glue
  )
  if (as_tag)
    out <- shiny::tags$script(type = "text/javascript", out)
  return(out)
}
#' Make a shiny input with named ID with a logical TRUE value when the element is moused over.
#' @inheritParams js_set_input_val
#' @export
#' @family JS
js_mouseover_once <- function(id,
                              asis = FALSE,
                              .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  # create an onmouseover event that runs once for the scatter plot that binds input$scatter_select as TRUE to start receiving the event_data below. Silences warning: The 'plotly_relayout' event tied a source ID warnings
  UU::glue_js(
    "
  $('#*{id}*').one('mouseover', (event) => {
      Shiny.setInputValue('*{id}*', true);
  })
  "
  )

}
