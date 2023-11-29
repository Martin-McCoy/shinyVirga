#' Add Transifex Language selector
#' @description
#' Must set up an environment variable entitled `TRANSIFEX_API_KEY` with your API Key created [here](https://app.transifex.com/user/settings/api/) and run `transifex_select_server` at least once in your app.
#' @details
#' Implementation adapted from the [Transifex docs](https://help.transifex.com/en/articles/6259014-technical-instructions#h_aa6dc907c4)
#'
#'
#' @inheritParams shinyWidgets::pickerInput
#' @inheritParams htmltools::tagAppendAttributes
#' @family translation
#' @return \code{shiny.tag}
#' @export
#'

mod_transifex_select <- function(inputId, label, choices = c("English" = "en", "Spanish"= "es"), selected = c(English = "en"), multiple = FALSE, options = list(), choicesOpt = NULL, width = NULL, class = NULL, inline = FALSE, key = Sys.getenv("TRANSIFEX_API_KEY", ''), ...) {
  tagList(
    shiny::singleton(
      shiny::tags$head(
        tags$script(type="text/javascript", src="//cdn.transifex.com/live.js"),
        tags$script(type="text/javascript", UU::glue_js('window.liveSettings={api_key:"*{Sys.getenv("TRANSIFEX_API_KEY", "")}*"}'))
      )
    ),
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
    ) |>
      tagAppendAttributes(...)
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
