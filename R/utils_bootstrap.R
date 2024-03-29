#' Valid Bootstrap Statuses
#' @family ui
#' @export
bs_statuses <- c("Primary",
                 "Secondary",
                 "Success",
                 "Danger",
                 "Warning",
                 "Question",
                 "Info",
                 "Light",
                 "Dark")

#' Extract the bootstrap status of a Bootstrap tag that uses the status in the class
#'
#' @param x \code{shiny.tag}
#' @family ui
#' @return \code{chr}
#' @export
#'

bs_extract_status <- function(x) {
  stringr::str_extract(x$attribs$class, paste0("(?<=\\-)",UU::regex_or(tolower(bs_statuses))))[1]
}
