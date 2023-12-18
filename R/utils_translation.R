#' Add Transifex Language selector
#' @description
#' Must set up an environment variable entitled `TRANSIFEX_API_KEY` with your API Key created [here](https://app.transifex.com/user/settings/api/) and run `transifex_select_server` at least once in your app.
#' @details
#' Implementation adapted from the [Transifex docs](https://help.transifex.com/en/articles/6259014-technical-instructions#h_aa6dc907c4)
#'
#'
#' @inheritParams shinyWidgets::pickerInput
#' @inheritParams htmltools::tagAppendAttributes

#' @inheritDotParams mod_transifex_deps transifex_options
#' @family translation
#' @return \code{shiny.tag}
#' @export
#'

mod_transifex_select <-
  function(inputId,
           label,
           choices = c("English" = "en", "Spanish" = "es"),
           selected = c(English = "en"),
           multiple = FALSE,
           options = list(),
           choicesOpt = NULL,
           width = NULL,
           class = NULL,
           inline = FALSE,
           include_deps = FALSE,
           key = Sys.getenv("TRANSIFEX_API_KEY", ''),
           ...) {


  tagList(
    if (include_deps)
      mod_transifex_deps(key = key, ...),
    do.call(
      shinyWidgets::pickerInput,
      list(
        inputId = inputId,
        label = label,
        choices = choices,
        selected = selected,
        multiple = multiple,
        options = options,
        choicesOpt = choicesOpt,
        width = width,
        inline = inline
      )
    )
  )
}

#' Add Transifex API Key to Javascript environment
#' @description
#' Must set up an environment variable entitled `TRANSIFEX_API_KEY` with your API Key created [here](https://app.transifex.com/user/settings/api/) and run `transifex_select_server` at least once in your app. **Requires shinyjs**
#' @param key \code{chr} Your Transifex API Key. Will be derived from an environment variable entitled `TRANSIFEX_API_KEY` if not specified
#' @return \code{shiny.tag}
#' @family translation
#' @export
#'

mod_transifex_select_server <- function(inputId, session = shiny::getDefaultReactiveDomain()) {
  input <- session$input
  observeEvent(input[[inputId]], {
    shinyjs::runjs(UU::glue_js("Transifex.live.translateTo('*{input[[inputId]]}*');"))
  }, priority = 2)

}

#' Include transifex dependencies
#'
#' @param key \code{chr} The Transifex API Key associated with the Project > Resource.
#' @param transifex_options \code{list} List of options to be passed to the \href{https://help.transifex.com/en/articles/6370718-javascript-api}{liveSettings object}.  The option `manual_init` must be `TRUE` when including the dependencies anywhere other than in the `head` of the application.
#' @return \code{shiny.tag.list} containing a \code{\link[htmltools]{singleton}} with the Transifex live.js and liveSettings options scripts.
#' @export
#'

mod_transifex_deps <- function(key = Sys.getenv("TRANSIFEX_API_KEY", ''), transifex_options = list(version = "latest", manual_init = TRUE)) {
  transifex_options <- rlang::list2(
    api_key = key,
    !!!transifex_options
  )

  tfex_options <- jsonlite::toJSON(transifex_options, auto_unbox = TRUE) |>
    stringr::str_replace_all('"(\\w+)"\\s*:', '\\1:')
  shiny::singleton(
    rlang::exec(shiny::tagList,
                purrr::map(
                  list(
                    UU::glue_js('window.liveSettings=*{tfex_options}*'),
                    list(src = "//cdn.transifex.com/live.js"),
                    if (isTRUE(transifex_options$manual_init))
                      list("Transifex.live.init()")
                  ),
                  \(.x) rlang::exec(htmltools::tags$script, type = "text/javascript", !!!.x)
                )
    )
  )
}
