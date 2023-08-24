
#' @title make a nested category list for \code{\link[shiny]{selectInput}}
#' @description Takes a "metrics-styled" `data.frame` with
#' @family inputs
#' \itemize{
#'   \item{A column that indicates the category}
#'   \item{A column with the values}
#'   \item{A column with the names displayed to the user, if different from the values (_optional_)}
#' }
#'  and returns a nested list appropriate for the `choices` argument to a \code{\link[shiny]{selectInput}}.
#' @param metrics_df \code{tbl} See description
#' @param metric_col_values \code{chr} column name with values
#' @param category_col \code{chr} column name with categories
#' @param metric_col_names \code{chr} column name with names (if there are names)
#'
#' @return \code{chr/list} list if category_col supplied, otherwise character vector
#' @export

select_choices <- function(metrics_df, metric_col_values, category_col, metric_col_names) {

  metric_col_values <- rlang::ensym(metric_col_values)
  category_col <- rlang::ensym(category_col)
  if (!missing(metric_col_names)) {
    metric_col_names <- rlang::ensym(metric_col_names)
    metrics_df <- dplyr::mutate(metrics_df,
                                !!metric_col_values := rlang::set_names(!!metric_col_values, !!metric_col_names))
  }

  if (!missing(category_col)) {
    metrics_df |>
      dplyr::select(!!category_col, !!metric_col_values) |>
      dplyr::group_by(!!category_col) |>
      {\(.x) {
        rlang::set_names(dplyr::group_split(.x, .keep = FALSE), dplyr::group_keys(.x)[[1]])
      }}() |>
      purrr::map(\(.x) {
        as.list(.x[[1]])
      })
  } else {
    metrics_df |>
      dplyr::pull(!!metric_col_values)
  }


}