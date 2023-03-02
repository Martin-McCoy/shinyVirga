#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param .ns \code{fun} ns function. Typically found automatically.
#' @inheritParams shiny::callModule
#'
#' @noRd 
#'
#' @importFrom shiny tagList 
pmod_test_ui <- function(.ns = shinyVirga::ns_find()){
  ns <- force(.ns)
  tagList(
    
  )
}
    
#' test Server Functions
#'
#' @noRd 
pmod_test_server <- function(session = shiny::getDefaultReactiveDomain()){
    ns <- session$ns
    input <- session$input
    output <- session$output
    
}
    
## To be copied in the UI
# pmod_test_ui()
    
## To be copied in the server
# pmod_test_server()
