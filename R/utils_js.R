#' Add a temporary message after an element
#' Message is added directly below element and persists for three seconds
#' @param id \code{(chr)} Automatically namespaced with `ns` if inside a module
#' @param content \code{chr/shiny.tag/shiny.tag.list} Content to add beneath the element
#' @param properties \code{list} of css rules to apply to the parent element of the content. Defaults to jQuery validation styled messages.
#' @param delay \code{dbl} duration in ms before removing the content from the element. Set to 0 to allow the element to persist.
#' @inheritParams shinyjs::html
#' @param .ns \code{fun} ns function. Typically found automatically.
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
    $('*{make_id(id)}*').after(() => {
      return `*{htmltools::HTML(html)}*`
    })
    ")
  shinyjs::runjs(the_js)
  if (delay)
    shinyjs::delay(delay, shinyjs::hide(selector = '.js-after', asis = TRUE))

}
#' Add driver.js dependency
#' @family JS
#' @export
use_driver.js <- function() {
  htmltools::htmlDependency(
    name = "driver.js",
    version = "1",
    src = c(href = "https://cdn.jsdelivr.net/npm/driver.js@1.0.1/dist/"),
    script = "driver.js.iife.js",
    stylesheet = "driver.css"
  )
}

make_id <- function(x) {
  if (grepl("^#", x))
    x
  else
    paste0("#", x)
}
make_id_ <- Vectorize(make_id)

#' Create a driver.js callout
#' Must include `shinyVirga::use_driver.js()` in the head of the page.
#' @family JS
#' @param className className to wrap driver.js popover
#' @param animate Whether to animate or not
#' @param opacity Background opacity (0 means only popovers and without overlay)
#' @param padding Distance of element from around the edges
#' @param allowClose Whether the click on overlay should close or not
#' @param overlayClickNext Whether the click on overlay should move next
#' @param doneBtnText Text on the final button
#' @param closeBtnText Text on the close button for this step
#' @param stageBackground Background color for the staged behind highlighted element
#' @param nextBtnText Next button text for this step
#' @param prevBtnText Previous button text for this step
#' @param showButtons Do not show control buttons in footer
#' @param keyboardControl Allow controlling through keyboard (escape to close, arrow keys to move)
#' @param scrollIntoViewOptions We use `scrollIntoView()` when possible, pass here the options for it if you want any
#' @param onHighlightStarted Called when element is about to be highlighted
#' @param onHighlighted Called when element is fully highlighted
#' @param onDeselected Called when element has been deselected
#' @param onReset Called when overlay is about to be cleared
#' @param onNext Called when moving to next step on any step
#' @param onPrevious Called when moving to previous step on any step
#' @inheritParams js_after
#' @inheritParams js_callback
#'
#' @export
#'

js_callout <- function(id,
                       title,
                       description,
                       position = "bottom-center",
                       className = 'scoped-class',
                       animate = TRUE,
                       opacity = 0.75,
                       padding = 10,
                       allowClose = TRUE,
                       overlayClickNext = FALSE,
                       doneBtnText = 'Done',
                       closeBtnText = 'Close',
                       stageBackground = '#ffffff',
                       nextBtnText = 'Next',
                       prevBtnText = 'Previous',
                       showButtons = TRUE,
                       keyboardControl = TRUE,
                       scrollIntoViewOptions = list(),
                       onHighlightStarted = NULL,
                       onHighlighted = NULL,
                       onDeselected = NULL,
                       onReset = NULL,
                       onNext = NULL,
                       onPrevious = NULL,
                       asis = FALSE,
                       animate_el = TRUE,
                       .ns = ns_find()) {


    if (!asis)
      id <- .ns(id)

    html <- htmltools::doRenderTags(description)
    driver_settings <- list(
      args = list(
        element = make_id(id),
        popover = purrr::map(list(
        title = title,
        description = description,
        position = position
      ), htmltools::doRenderTags)),
      opts = purrr::compact(list(
        className = className,
        animate = animate,
        opacity = opacity,
        padding = padding,
        allowClose = allowClose,
        overlayClickNext = overlayClickNext,
        doneBtnText = doneBtnText,
        closeBtnText = closeBtnText,
        stageBackground = stageBackground,
        nextBtnText = nextBtnText,
        prevBtnText = prevBtnText,
        showButtons = showButtons,
        keyboardControl = keyboardControl,
        scrollIntoViewOptions = scrollIntoViewOptions,
        onHighlightStarted = onHighlightStarted,
        onHighlighted = onHighlighted,
        onDeselected = onDeselected,
        onReset = onReset,
        onNext = onNext,
        onPrevious = onPrevious
      ))
    ) |>
      purrr::map(jsonlite::toJSON, auto_unbox = TRUE, pretty = TRUE) |>
      purrr::map(stringr::str_replace_all, pattern = '"(\\w+)"\\s*:', replacement = '\\1:')

    to_glue <- c(
      c("const newDriver = new driver(*{driver_settings$opts}*);",
        "driver.popover(*{driver_settings$args}*)")
    )

    if (animate_el) {



    }
    the_js <- UU::glue_js(to_glue)
    shinyjs::runjs(the_js)
}


#' Add an animated glow to an element
#' @family JS
#' @inheritParams js_after
#' @param color \code{chr} A hexadecimal color to animate with
#'
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' devtools::load_all(pkgload::pkg_path())
#' # Define UI for application that draws a histogram
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   div(id = "test", "blah", style = "width:50px"),
#' )
#'
#'
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'
#'   shinyVirga::js_glow(
#'     id = "test"
#'   )
#'
#' }
#' }
#'
#' shinyApp(ui = ui, server = server)

js_glow <- function(id,
                    color = "deepskyblue",
                    asis = FALSE,
                    delay = NULL,
                    .ns = ns_find()) {
  color_alpha <-
    colorspace::adjust_transparency(color, c(rep(.8, 5), rep(.5, 3), rep(.3, 2)))


  if (!asis)
    id <- .ns(id)


  color_alpha[7:10] <- colorspace::darken(color_alpha[7:10], .3)
  color_alpha[1:2] <- colorspace::lighten(color_alpha[1:2], .6)
  color_alpha[3:4] <- colorspace::lighten(color_alpha[3:4], .3)
  frames <- list(
    text = list(a = tibble::tibble(a = 0,
                                   b = 0,
                                   width = paste0(seq(10, 200, by = 20), "px"),
                                   color_alpha),
                z = tibble::tibble(a = 0,
                                   b = 0,
                                   width = paste0(seq(2, 80, length.out = 10) |> round(), "px"),
                                   color_alpha)),
    box = list(a = tibble::tibble(a = 0,
                                  b = 0,
                                  width = paste0(seq(5, 50, by = 5), "px"),
                                  color_alpha),
               z = tibble::tibble(a = 0,
                                  b = 0,
                                  width = paste0(seq(5, 50, by = 5), "px"),
                                  color_alpha))
  ) |>
    purrr::map_depth(2, \(.x) {
      purrr::pmap_chr(.x, \(...) {
        .x <- list(...)
        paste(.x, collapse = " ")
      }) |> glue::glue_collapse(sep = ",\n\t\t")
    })

  style <- UU::glue_js(
    "// only append if not already used
    if ($('#animated-css').length != 1) {
      var style = document.createElement('style');
        style.innerHTML = `

          @keyframes neonGlow {
            0% {
              text-shadow: *{frames$text$a}*;
              box-shadow:  *{frames$box$a}*;
            }
            100% {
              text-shadow: *{frames$text$z}*;
              box-shadow:  *{frames$box$z}*;
            }
          }

          .animated {
            animation: neonGlow 1s infinite alternate cubic-bezier(0.455, 0.030, 0.515, 0.955);
          }
      `
      $(style).attr('id', 'animated-css').appendTo('head');
    }
      "
  )
  shinyjs::runjs(style)
  shinyjs::addClass(id = id, asis = asis, class = "animated")
  if (!is.null(delay))
    shinyjs::delay(delay,
                   shinyjs::removeClass(id = id, asis = asis, class = "animated"))
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
#' @param value_to \code{chr} One of:
#' \itemize{
#'   \item{\code{code}}{ The object will be treated as raw javascript code **Default**}
#'   \item{\code{chr}}{ The object will be treated as as JS character string}
#'   \item{\code{json}}{ The object will be coerced with \code{\link[jsonlite]{toJSON}}}
#' }
#' @param as_callback \code{lgl} Should the resulting code be wrapped in a callback function?
#' @param on \code{chr} A javascript event name, must be specified with a selector to argument `on_dom` that indicates the jQuery selector for a DOM element to which the event will pertain.
#' @param as_tag \code{lgl} Whether the output should be a script tag.
#' @param priority_event \code{lgl} Is the priority event level. See [Communicating with JS](https://shiny.posit.co/r/articles/build/communicating-with-js/) for details.
#' @return \code{chr/shiny.tag} script tag with javascript
#' @export
#' @family JS
js_set_input_val <- function(id,
                             value,
                             js = NULL,
                             ...,
                             value_to = c("code", "chr", "json")[1],
                             asis = FALSE,
                             as_callback = FALSE,
                             on = NULL,
                             as_tag = FALSE,
                             priority_event = TRUE,
                             .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  rlang::env_bind(environment(), ...)

  priority <- if (isTRUE(priority_event))
    ", {priority: 'event'}"
  else
    ""
  to_glue <- c("Shiny.setInputValue('*{id}*', *{value}**{priority}*);")

  value <- switch(
    value_to,
    code = value,
    chr = paste0("'",value,"'"),
    json = jsonlite::toJSON(value)
  )


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
    c("$(*{on_dom}*).on('*{on}*', (e) => {",
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

#' Open/Close a bs4Card with ID
#'
#' @param id \code{chr} ID of the bs4Card
#' @param action \code{chr} One of:
#' \itemize{
#'   \item{\code{toggle}}{ Toggle Open/Closed}
#'   \item{\code{open}}{ If closed, open, otherwise do nothing}
#'   \item{\code{close}}{ If open, close, otherwise do nothing}
#' }
#'
#' @return \code{None}
#' @export
#'

js_bs4Card_action <- function(id, action = 'toggle') {
  shinyjs::runjs(
    UU::glue_js("
           function openCard(id, toggle = *{ifelse(action == 'toggle', 'true', 'false')}*) {
             let sel = `#${id} > .card-header > .card-tools > .btn-tool`
             if (toggle) {
               $(sel).click()
             } else if ($(sel + ' > .fas').hasClass('fa-*{switch(action, open = 'plus', close =  'minus')}*')) {
               $(sel).click()
             }
           }
           openCard('*{id}*')
           ")
  )
}


#' Create an incrementing shiny input each time a render function runs
#' @description
#' Requires \code{\link[shinyjs]{runjs}}. Useful for creating a callback once a DT is rerendered to update the page number for page number retention.
#'
#' @param outputId \code{chr} The outputId value
#' @inheritParams shiny::updateActionButton
#'
#' @return Called for side effects. Runs code via shinyjs.
#' @export
#'
#' @examples
#' \dontrun{
#' output[[outputId]] <- renderDT({
#'   ## DT CODE
#'   render_ran_input(outputId)
#' })
#' }
#'
render_ran_input <- function(outputId, session = shiny::getDefaultReactiveDomain()) {
  shinyjs::runjs(shinyVirga::js_set_input_val(paste0(session$ns(outputId), "_ran"), session$input[[outputId]] %|0|% 0 + 1, asis = TRUE))
}
