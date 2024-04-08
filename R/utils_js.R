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
    version = "1.0.1",
    src = c(href = "https://cdn.jsdelivr.net/npm/driver.js@1.0.1/dist/"),
    script = list(src = "driver.js.iife.js", defer = NA),
    stylesheet = "driver.css"
  )
}

make_id <- function(x) {
  if (grepl("^#|\\.", x))
    x
  else
    paste0("#", x)
}
make_id_ <- Vectorize(make_id)


#' Set attributes of a DOM element
#' @description
#' This will replace existing attribute if it exists.
#'
#' @inheritParams js_after
#' @param ... \code{named args} attributes to set
#'
#' @return \code{None} Updates the element
#' @export
#'

js_set_attrs <-  function(id,
                         ...,
                         asis = FALSE,
                         .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)

  .dots <- rlang::dots_list(...)
  purrr::imap_chr(.dots, \(.x, .y) {
    UU::glue_js("$('#*{id}*').css('*{ .y}*', '*{ .x}*');")
  }) |>
    glue::glue_collapse() |>
    shinyjs::runjs()

}

#' Create a driver.js callout
#' Must include `shinyVirga::use_driver.js()` & \code{\link[shinyjs]{useShinyJS}} in the head of the page.
#' @family JS

#' @param el \code{chr} ID or class of element. If a character not prefixed by `.` for a class or `#` for an ID, it will be assumed and ID and `#` will be added.
#' @param title \code{chr} Title of popover
#' @param description \code{chr} Description for it
#' @param side \code{chr}  The position of the popover relative to the target element. One of "top", "right", "bottom", "left".
#' @param align \code{chr} The alignment of the popover relative to the target element. One of "start", 'center', "end".
#' @param animate \code{lgl} Whether to animate the product tour. (default: true)
#' @param overlayColor \code{chr} Overlay color. (default: black) This is useful when you have a dark background and want to highlight elements with a light background color.
#' @param smoothScroll \code{lgl}  Whether to smooth scroll to the highlighted element. (default: false)
#' @param allowClose \code{lgl}  Whether to allow closing the popover by clicking on the backdrop. (default: true)
#' @param overlayOpacity \code{num}  Opacity of the backdrop. (default: 0.5)
#' @param stagePadding \code{num} Distance between the highlighted element and the cutout. (default: 10)
#' @param stageRadius  \code{num}  Radius of the cutout around the highlighted element. (default: 5)
#' @param allowKeyboardControl \code{lgl} Whether to allow keyboard navigation. (default: true)
#' @param disableActiveInteraction \code{lgl} Whether to disable interaction with the highlighted element. (default: false)
#' @param popoverClass \code{chr} If you want to add custom class to the popover
#' @param popoverOffset \code{num} Distance between the popover and the highlighted element. (default: 10)
#' @param showButtons \code{chr}  Array of buttons to show in the popover. Defaults to ["next", "previous", "close"] for product tours and [] for single element highlighting.
#' @param disableButtons \code{chr}  Array of buttons to disable. This is useful when you want to show some of the buttons, but disable some of them.
#' @param showProgress \code{lgl}  Whether to show the progress text in popover. (default: false)
#' @param progressText \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param doneBtnText  \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param closeBtnText \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param nextBtnText \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param prevBtnText \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param doneBtnText \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onPopoverRender \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onHighlightStarted \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onHighlighted \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onDeselected \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onDestroyStarted \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onDestroyed \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onNextClick \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onPrevClick \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @param onCloseClick \code{chr} See \code{\href{https://driverjs.com/docs/configuration}{docs}} for details
#' @inheritParams js_after
#' @examples
#'
#' js_callout('my_id', "Tooltip title", "Tooltip description")
#' @export

js_callout <- function(el,
                       title,
                       description,
                       side = NULL,
                       align = NULL,
                       animate = TRUE,
                       overlayColor = "black",
                       smoothScroll = FALSE,
                       allowClose = TRUE,
                       overlayOpacity = 0.5,
                       stagePadding = 10,
                       stageRadius = 5,
                       allowKeyboardControl = TRUE,
                       disableActiveInteraction = FALSE,
                       popoverClass = 'scoped-class',
                       popoverOffset = 10,
                       showButtons = NULL,
                       disableButtons = NULL,
                       showProgress = FALSE,
                       progressText = FALSE,
                       nextBtnText = 'Next',
                       prevBtnText = 'Previous',
                       doneBtnText = 'Done',
                       onPopoverRender = NULL,
                       onHighlightStarted = NULL,
                       onHighlighted = NULL,
                       onDeselected = NULL,
                       onDestroyStarted = NULL,
                       onDestroyed = NULL,
                       onNextClick = NULL,
                       onPrevClick = NULL,
                       onCloseClick = NULL,
                       asis = FALSE,
                       .ns = ns_find()) {


    if (!asis)
      el <- .ns(el)


    driver_settings <- list(
      highlight = list(
        element = make_id(el),
        popover = purrr::map_at(purrr::compact(list(
        title = title,
        description = description,
        side = side,
        align = align,
        showButtons = showButtons,
        disableButtons = disableButtons,
        popoverClass = popoverClass,
        showProgress = showProgress,
        progressText = progressText,
        disableButtons = disableButtons,
        nextBtnText = nextBtnText,
        prevBtnText = prevBtnText,
        doneBtnText = doneBtnText
      )), \(.x) .x %in% c("title", "description"), htmltools::doRenderTags)),
      driver = purrr::compact(
        list(
          animate = animate,
          overlayColor = overlayColor,
          smoothScroll = smoothScroll,
          allowClose = allowClose,
          overlayOpacity = overlayOpacity,
          stagePadding = stagePadding,
          stageRadius = stageRadius,
          allowKeyboardControl = allowKeyboardControl,
          disableActiveInteraction = disableActiveInteraction,
          popoverClass = popoverClass,
          popoverOffset = popoverOffset,
          showProgress = showProgress,
          progressText = progressText,
          showButtons = showButtons,
          disableButtons = disableButtons,
          nextBtnText = nextBtnText,
          prevBtnText = prevBtnText,
          doneBtnText = doneBtnText
        )
      )
    )
    driver_callbacks <- list(
      highlight = list(
        onPopoverRender = onPopoverRender,
        onNextClick = onNextClick,
        onPrevClick = onPrevClick,
        onCloseClick = onCloseClick
      ),
      driver = list(onPopoverRender = onPopoverRender,
                       onHighlightStarted = onHighlightStarted,
                       onHighlighted = onHighlighted,
                       onDeselected = onDeselected,
                       onDestroyStarted = onDestroyStarted,
                       onDestroyed = onDestroyed,
                       onNextClick = onNextClick,
                       onPrevClick = onPrevClick,
                       onCloseClick = onCloseClick
                       )
    ) |>
      purrr::map(purrr::compact)

    driver_settings_json <- purrr::map(driver_settings, jsonlite::toJSON, pretty = TRUE, auto_unbox = TRUE) |>
      purrr::map(stringr::str_replace_all, pattern = '"(\\w+)"\\s*:', replacement = '\\1:')

    has_callbacks <- !purrr::map_lgl(driver_callbacks, rlang::is_empty)
    if (any(has_callbacks)) {
      nms <- names(driver_settings)
      # toJSON escapes function calls, JS functions must be inserted asis
      for (i in which(has_callbacks)) {
        json <- stringr::str_split(driver_settings_json[[i]], "\\n")[[1]]
        # The popover settings are double nested in the JSON, so they must be inserted before the two closing brackets
        idx_insert <- length(json) - ifelse(identical(nms[i], "driver"), 1, 2)
        # Add comma since were adding arguments to the json
        json[idx_insert] <- paste0(json[idx_insert], ",")
        callbacks <- purrr::imap_chr(driver_callbacks[[i]], \(.x, .y) glue::glue("  { .y}: { .x}")) |>
          glue::glue_collapse(sep = ",\n")
        json <- append(json, after = idx_insert, callbacks)
        driver_settings_json[[i]] <- glue::glue_collapse(json, sep = "\n")
      }
    }



    to_glue <- c(
      c("(() => {
        const driver = window.driver.js.driver;
        const newDriver = new driver(*{driver_settings_json$driver}*);
        newDriver.highlight(*{driver_settings_json$highlight}*);
        })()")
    )

    the_js <- UU::glue_js(to_glue)
    shinyjs::runjs(the_js)
}


#' Add a "Toggle" style to an actionButton
#' @description
#' Wrap the actionbuttion in this function and once clicked, the button will appear to be in a "pressed" state until the next click.
#' Useful for toggling a floating panel which is hidden/shown on button click
#' @param btn \code{shiny.tag} The actionButton to style
#'
#' @return \code{shiny.tag.list} With the button
#' @export
#' @examples
#' library(shiny)
#' devtools::load_all(pkgload::pkg_path())
#' ui <- fluidPage(
#'   shinyVirga::use_shinyVirga(),
#'   actionButton_toggle_style(
#'     shiny::actionButton(
#'       'modal',
#'       "Open Floating Panel",
#'       icon = shiny::icon("redo")
#'     )
#'   )
#' )
#'
#'
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'
#' }
#' shinyApp(ui = ui, server = server)

actionButton_toggle_style <- function(btn) {
  if (!rlang::inherits_any(btn, c("shiny.tag", "shiny.tag.list")))
    UU::gbort("{.code btn} must be an `actionButton`")
  id <- if (shinyVirga::is_shiny.tag(btn, explicit = TRUE))
    btn$attribs$id
  else
    btn[[1]]$attribs$id



  htmltools::attachDependencies(
    tagList(
      btn,
      htmltools::tags$script(
        type = 'text/javascript',
        UU::glue_js(
          "
        document.getElementById('*{id}*').addEventListener('click', buttonClose)
        "
        )
      )
    ),
    htmltools::htmlDependency(
      name = "buttonClose",
      version = packageVersion('shinyVirga'),
      src = system.file("srcjs", package = "shinyVirga"),
      script = 'buttonClose.js'
    )
  )


}

#' Add a "Toggle" style to an actionButton
#' @description
#' Call this button on the `inputId` of the action button with a reactive observer
#'
#' @inheritParams shiny::actionButton
#'
#' @return \code{null} Runs JS
#' @export
#'
#' @examples
#' library(shiny)
#' devtools::load_all(pkgload::pkg_path())
#' ui <- fluidPage(
#'   shinyVirga::use_shinyVirga(),
#'   shiny::actionButton(
#'     'modal',
#'     "Open Modal",
#'     icon = shiny::icon("redo")
#'   )
#' )
#'
#'
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'   observeEvent(input$modal,
#'                {
#'                  actionButton_close("modal")
#'                  shiny::showModal(modalDialog(
#'                    title = "Hi, Modal",
#'                    footer = actionButton('close_modal',
#'                                          "Close")
#'                  ))
#'                })
#'   observeEvent(input$close_modal, {
#'     actionButton_close("modal")
#'     removeModal()
#'   })
#'
#' }
#' shinyApp(ui = ui, server = server)

actionButton_toggle <- function(inputId) {
  shinyjs::runjs(UU::glue_js("buttonClose('*{inputId}*')"))
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
#' shinyApp(ui = ui, server = server)
#' }
#'

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
#' @description
#' Requires `shinyVirga::use_shinyVirga()` in the head of the DOM of the app (typically in ui file).
#' @param id \code{chr} ID of the bs4Card
#' @inheritParams js_set_input_val
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

js_bs4Card_action <- function(id, action = 'toggle', asis = FALSE, .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  shinyjs::runjs(
    UU::glue_js("cardOpen('*{id}*', action = '*{action}*')")
  )
}

picker_js <- function(enable = TRUE) {
  status <- ifelse(enable, "true", 'false')
  sprintf("((id) => {
      var picker = $(id)
      if (picker) {
        picker.prop('disabled', %s);
        picker.selectpicker('refresh');
      }

      })('#*{id}*')
      ", status)
}

#' Enable a bootstrap pickerInput
#'
#' @inheritParams js_mouseover_once
#' @author [Victor Perrier](https://github.com/pvictor), Maria Sevillano, Stephen Holsenbeck
#' @return \code{none} called for side-effects
#' @export
#'
#' @family JS
js_picker_enable <- function(id,
                             asis = FALSE,
                             .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  shinyjs::runjs(
    UU::glue_js(picker_js(enable = TRUE))
  )


}

#' Disable a bootstrap pickerInput
#'
#' @inherit js_picker_enable params author
#' @return \code{none} called for side-effects
#' @export
#'
#' @family JS
js_picker_disable <- function(id,
                             asis = FALSE,
                             .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)
  shinyjs::runjs(
    UU::glue_js(picker_js(enable = FALSE))
  )

}

#' Check to ensure all plotlys are visible within an element
#'
#' @inheritParams js_picker_disable
#' @inherit js_bs4Card_action description
#' @return \code{none} called for side affects
#' @export
#'

js_all_plotly_visible <- function(id,
                                  asis = FALSE,
                                  .ns = ns_find()) {
  the_ns <- .ns('')
  if (!asis)
    id <- .ns(id)

  shinyjs::runjs(
    UU::glue_js(
      "
      $('#*{id}*').find('.plotly.html-widget').each((i, e) => {
        var ns = '*{the_ns}*';
        var svg = $(e).find('.svg-container')[0];
        var v = isVisible(svg);
        Shiny.setInputValue('*{the_ns}*' + e.id.match(/\\w+$/)[0] + '_visible', v, {priority: 'event'})
      })
      "
    )
  )

}

#' Remove or show sliderInput tick marks
#'
#' @inheritParams js_accordion_open
#' @param hide \code{lgl} Whether to hide the tick marks
#' @return \code{None} Removes tick marks from slider
#' @export
#'

js_sliderInput_ticks <- function(id,
                                 hide = TRUE,
                              asis = FALSE,
                              .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)

  shinyjs::runjs(
    UU::glue_js(
      '$("#*{id}*").siblings("span.irs").find(".irs-grid").attr("hidden", *{tolower(hide)}*)'
    )
  )
}



#' Opens all accordions of provided `id`
#'
#' @inheritParams js_picker_enable
#'
#' @return \code{none} Opens accordions
#' @export
#'

js_accordion_open <- function(id,
                               asis = FALSE,
                               .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)

  shinyjs::runjs(
    UU::glue_js(
      "
      $('#*{id}*').find('a').each((i, e) => {
        var el = $(e);
        if (el.hasClass('collapsed')) {
          el.click()
        }

      })
      "
    )
  )
}

#' Opens all accordions of provided `id`
#'
#' @inheritParams js_picker_enable
#'
#' @return \code{none} Opens accordions
#' @export
#'

js_accordion_close <- function(id,
                                     asis = FALSE,
                                     .ns = ns_find()) {
  if (!asis)
    id <- .ns(id)

  shinyjs::runjs(
    UU::glue_js(
      "
      $('#*{id}*').find('a').each((i, e) => {
        var el = $(e);
        if (!el.hasClass('collapsed')) {
          el.click()
        }

      })
      "
    )
  )
}

#' Add functionality that closes a floating panel when clicking outside of the panel.
#'
#' @param x \code{shiny.tag/chr} of actionButton to click to close the panel or the `inputId` thereof.
#' @param panel_id \code{chr} ID of the panel to close/open when clicking outside
#' @return \code{shiny.tag} with script that adds event listener
#' @export
#'
js_click_to_close <- function(x, panel_id, debugger = FALSE) {
  if (missing(x) || missing(panel_id)) {
    UU::gbort("{.code x} must be a {.code shiny.tag} or the from {.code shiny::actionButton} or the {.code inputId} thereof, and the {.code panel_id} must be provided.")
  }
  id_provided <- is.character(x)
  id <- if (id_provided)
     x
  else if (shinyVirga::is_shiny.tag(x, explicit = TRUE))
    x$attribs$id
  else
    x[[1]]$attribs$id

  fn_nm <- paste0('close_', nm_to_id(panel_id))
  tagList(
    if (!id_provided) x,
    htmltools::tags$script(
      type = "text/javascript",
      UU::glue_js(
        "
        document.*{fn_nm}* = (event) => {
        *{if (debugger) 'debugger' else ''}*
          let el = {
            panel: $('#*{panel_id}*'),
            button: $('#*{id}*')
          };
        let t = isVisible(event.target) ? event.target : document.elementFromPoint(event.x, event.y)
          if (isVisible(el.panel[0]) && !el.panel[0].contains(t) && !el.button[0].contains(t)) $('#*{id}*').click();
        };
        document.addEventListener('click', document.*{fn_nm}*);
      "
      )
    )
  )
}
#' Force the browser to download JSON, useful for saved session recovery
#' @description
#' Useful in combination with `options(shiny.error = ...)` for saving a session on a break.
#' See Examples for details
#' @param x \code{chr/obj} The raw json as a character, a path to a file, or an R object in which case \code{\link[jsonlite]{toJSON}} will be used to convert the object to JSON.
#' @param is_file \code{lgl} If x is a file, must be TRUE
#' @param filename \code{chr} Name of file to be downlaoded
#'
#' @return \code{none} called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   actionButton("break_me", "Break the app")
#' )
#'
#' server <- function(input, output, session) {
#'   options(shiny.error = \() {
#'   js_force_download_json(jsonlite::toJSON(list(some = "important information")), filename = "test.json")
#'   })
#'   observeEvent(input$break_me, {
#'     rlang::abort("Done broke the app")
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }

js_force_download_json <- function(x, is_file = FALSE, filename = "recovery-save-session.json") {
  no_brackets <- !any(stringr::str_detect(x, "\\{"))
  if (!is.character(x) && no_brackets) {
    x <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
  }
  if (is_file) {
    x <- readLines(x)
  }
  x <- glue::glue_collapse(x, sep = "\n")
  stopifnot(`Must be run in a shiny session` = shiny::isRunning())
 UU::need_pkg("shinyjs", "runjs")(UU::glue_js("(() => {
  var json = `*{x}*`;
  var data = \"text/json;charset=utf-8,\" + encodeURIComponent(json);
  var a = document.createElement('a');
  a.href = 'data:' + data;
  a.download = '*{filename}*';
  a.innerHTML = 'download JSON';
  var container = document.getElementsByTagName('body')[0];
  container.appendChild(a);
  a.click();
  a.remove();
  })()"))
}


#' Create an incrementing shiny input each time a render function runs
#' @description
#' Requires \code{\link[shinyjs]{runjs}} if not using `htmlwidget`. Useful for firing an observer after a render function completes to update the page number for page number retention.
#'
#' @param id \code{chr} the id of the output for which a new input with the name `[id]_ran` (without brackets) will be incremented each time it renders.
#' @inheritParams js_picker_enable
#' @param htmlwidget \code{htmlwidget} If the render function is rendering an htmlwidget, it can be passed. This obviates the need for shinyjs.
#' @param debugger \code{lgl} whether to add a debugger statement to the callback.
#' @return Runs code via shinyjs unless an htmlwidget is provided, in which case it modifies the htmlwidget with \link[htmlwidgets]{onRender} and returns it.
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
render_ran_input <- function(id, htmlwidget, asis = FALSE, session = shiny::getDefaultReactiveDomain(), debugger = FALSE) {

  if (!missing(htmlwidget)) {
    htmlwidgets::onRender(
      htmlwidget,
      UU::glue_js(
        c("(e, x) => {",
          "var id = e.id + '_ran';",
          "var val = window[id] || 0;",
          if (debugger) 'debugger;',
          "val += 1;",
          "window[id] = val;",
          "Shiny.setInputValue(id, val, {priority: 'event'});",
        "}")
      )
    )
  } else {
    id_ran <- paste0(id, "_ran")
    val <- shiny::isolate(session$input[[id_ran]] %||% 0)
    if (!asis) {
      id_ran <- session$ns(id_ran)
    }
    shinyjs::runjs(shinyVirga::js_set_input_val(id_ran,  val + 1, asis = TRUE))
  }

}
