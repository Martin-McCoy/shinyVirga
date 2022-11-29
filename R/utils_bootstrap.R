#' Valid Bootstrap Statuses
#' @export
bs_statuses <- c("Primary",
                 "Secondary",
                 "Success",
                 "Danger",
                 "Warning",
                 "Info",
                 "Light",
                 "Dark")

#' Extract the bootstrap status of a Bootstrap tag that uses status
#'
#' @param x \code{shiny.tag}
#'
#' @return \code{chr}
#' @export
#'

bs_extract_status <- function(x) {
  stringr::str_extract(x$attribs$class, UU::regex_or(prefix = "(?=\\-)", tolower(bs_statuses)))
}
