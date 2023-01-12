

#' Create a row with columns that dynamically resize
#' Must use `div(class = 'column', ...)` or `col_auto()` for internal columns to function properly.
#' @inherit shiny::fluidRow params return
#'
#' @export
#' @family ui
#' @examples
#' dynamic_row(col_auto(bs4Dash::box(width = 12)), col_auto(bs4Dash::box(width = 12)))
dynamic_row <- function(...) {
  out <- shiny::fluidRow(...)
  l <- 1:length(out$children)
  htmltools::tagAppendAttributes(
    out,
    style = UU::glue_js('
    display: grid; grid-template-columns: *{paste0(collapse = " ", purrr::map_chr(l, ~"1 fr"))}*; grid-template-areas: "*{paste0(collapse = " ", purrr::map_chr(l, ~"a"))}*";')
  )

}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @param box \code{lgl} Whether to box the contents or just put them in row. **Default TRUE**
#' @param row \code{lgl} Whether to wrap the output in a row. **Default TRUE**
#' @param class \code{chr} A class to add to the top level div.row
#' @param add_attribs \code{list} of attributes to append to arbitrary tags in the format `list([jQuery selector] = list([attribute] = [value]))`
#' @return A full-width \link[bs4Dash]{box} nested in a row
#' @export
#' @family ui
#' @examples
#' ui_row(tags$p("Hi"))
ui_row <- function(...,
                   title = NULL,
                   footer = NULL,
                   status = NULL,
                   solidHeader = FALSE,
                   background = NULL,
                   width = 12,
                   height = NULL,
                   collapsible = TRUE,
                   collapsed = FALSE,
                   closable = FALSE,
                   maximizable = FALSE,
                   icon = NULL,
                   gradient = FALSE,
                   boxToolSize = "sm",
                   elevation = NULL,
                   headerBorder = TRUE,
                   label = NULL,
                   dropdownMenu = NULL,
                   sidebar = NULL,
                   id = NULL,
                   box = TRUE,
                   row = TRUE,
                   class = NULL,
                   add_attribs = NULL
) {
  .dots <- rlang::dots_list(...)
  .args <- list(title = title,
                footer = footer,
                status = status,
                solidHeader = solidHeader,
                background = background,
                width = width,
                height = height,
                collapsible = collapsible,
                collapsed = collapsed,
                closable = closable,
                maximizable = maximizable,
                icon = icon,
                gradient = gradient,
                boxToolSize = "sm",
                elevation = elevation,
                headerBorder = headerBorder,
                label = label,
                dropdownMenu = dropdownMenu,
                sidebar = sidebar,
                id = id)

  if (UU::is_legit(.dots)) {
    out <- eval(
      rlang::call2(
        purrr::when(box, . ~ bs4Dash::box, ~ shiny::tagList),
        !!!purrr::when(box,. && UU::is_legit(.dots) ~ append(.args, .dots), . ~ .args,  ~ .dots)
      )
    )
  } else
    out <- NULL
  if (row)
    out <- shiny::fluidRow(class = trimws(paste("ui_row", class)),
                           id = purrr::when(box, isTRUE(.) ~ NULL,  ~ .args$id),
                           out)

  if (box && !is.null(add_attribs)) {
    o <- htmltools::tagQuery(out)
    for (selector in names(add_attribs)) {
      do.call(o$find(selector)$addAttrs, add_attribs[[selector]])
      out <- o$allTags()
    }
  }
  out
}

#' @title Create a bootstrap 4 Alert box
#'
#' @param style \code{(character)} Inline style parameters to add
#' @inherit bs4Dash::bs4Card params return
#' @family ui
#' @export

bs4Alert <- function(..., status = "primary", style = NULL, id = NULL, width = 6) {
  bs4Dash:::validateStatus(status)
  status <- UU::match_letters(status, n = 2, bs4Dash:::validStatuses)

  shiny::tags$div(class = paste0("alert alert-",status), role = "alert", ..., style = paste0("margin: 6px 5px 6px 15px;", ifelse(grepl(";$", style), style, paste0(style, ";"))), id = id)
}

#' @title Make columns from assorted shiny.tag elements
#' @description Sorts shiny.tags into columns based on the maximum number of columns (`max_cols`) per row. `r lifecycle::badge("experimental")`
#' @param x \code{(shiny.tags)}
#' @param max_cols \code{(logical/integer)} Either `TRUE` **Default** for a default of 4 columns per row, `FALSE` for no columns, or an integer indicating the max number of columns.
#' @param fn \code{fun} with which to wrap each column's content. Typically \link[bs4Dash]{box} or \link[bs4Dash]{column}
#' @family ui
#' @return \code{(list(s))}
#' @export

make_columns <- function(x, max_cols = TRUE, fn = list(bs4Dash::box, bs4Dash::column)[[1]]) {
  max_cols <- purrr::when(isTRUE(max_cols),
                          . ~ 4,
                          ~ max_cols)

  if (max_cols) {
    ld <- nrow(x)
    rows <- x |>
      dplyr::mutate(.g = rep(1:ld, each = max_cols, length.out = ld)) |>
      dplyr::group_by(.g) |>
      dplyr::group_split(.keep = FALSE)

    out <- purrr::map(rows, ~{
      .cols <- .x
      .width = 12 %/% max_cols
      do.call(shiny::fluidRow,
              purrr::pmap(.cols, ~ {
                .args <- rlang::dots_list(..., .named = TRUE)
                .lgl <- names(.args) %in% rlang::fn_fmls_names(fn)
                .args <- append(.args[.lgl], unname(.args[!.lgl]))

                rlang::exec(fn,
                            !!!.args,
                            width = .width
                )
              })
      )
    })
  } else
    out <- x
  out
}

#' Create boxes around a list of shiny.tags
#'
#' @param x \code{list} of shiny.tags to box
#' @inheritDotParams ui_row
#' @return \code{shiny.tag.list} with \link[bs4Dash]{box}'s around them.
#' @export
#' @family ui
#' @examples
#' box_list(purrr::map(c("Lorem", "Ipsum"), htmltools::HTML))
box_list <- function(x, ...) {
  do.call(shiny::tagList, purrr::imap(x, ~ ui_row(
    .x,
    title = .y,
    ...
  )))
}

#' Create an accordion from a list of items
#'
#' @param x \code{chr/shiny.tag.list} chr vector or shiny.tag.list to wrap in accordions
#' @inheritParams bs4Dash::accordion
#' @param collapse_all \code{lgl} Whether to keep all accordions collapsed except for the open one, or keep them open after opening.
#' @inheritDotParams bs4Dash::accordion
#'
#' @return \code{shiny.tag.list}
#' @export
#' @family ui
#' @examples
#' acc_list(shiny::tagList(shiny::p("a"), shiny::p("b")))
#' # purrr::map is used internally so the first layer of list nesting determines what resides in each accordion
#' acc_list(list("a" , shiny::tagList(shiny::p("a"), shiny::p("b"))))

acc_list <-
  function(x,
           id = "accordion",
           width = 12,
           collapsed = TRUE,
           collapse_all = FALSE,
           ...) {
    .dots <- rlang::dots_list(...)
    rlang::exec(
      bs4Dash::accordion,
      id = id,
      width = width,
      collapse_all = collapse_all,
      !!!.dots,
      !!!purrr::imap(
        x,
        ~ bs4Dash::bs4AccordionItem(.x, title = .y, collapsed = collapsed))
    )

  }



#' Create a \code{shiny.tag.list} of \link[bs4Dash]{bs4SidebarMenuItem}s with an input \link[tibble]{tibble}
#'
#' @param tabs \code{tbl} similar to the following:
#' \preformatted{
#' tabs <- tibble::tribble(
#'   ~ text, ~ icon, ~ dots,
#'   "Welcome", "house", list(),
#'   "Education", "chalkboard", list(),
#'   "Performance", "flag-checkered", list(),
#'   "Robustness", "circle", list(),
#'   "Vulnerabilty", "bomb", list()
#'   ) |>
#'     dplyr::mutate(tabName = snakecase::to_snake_case(text))
#' }
#' where `dots` is a list of additional arguments to \link[bs4Dash]{bs4SidebarMenuItem}
#' @return \code{shiny.tag.list}
#' @seealso tabs
#' @family ui
#' @export

ui_tabs <- function(tabs) {
  purrr::pmap(tabs, ~{
    x <- rlang::dots_list(...)
    if ("dots" %in% names(x))
      x <- rlang::list2(
        !!!purrr::keep(x, !names(x) %in% "dots"),
        !!!x$dots
      )
    # Exclude all values but those intended for sideBarMenuITem
    sb_item_arg_nms <- rlang::fn_fmls_names(bs4Dash::bs4SidebarMenuItem)
    sb_args <- purrr::compact(x[stringr::str_subset(sb_item_arg_nms, "icon", negate = TRUE)])

    rlang::exec(
      bs4Dash::bs4SidebarMenuItem,
      !!!purrr::list_modify(
        sb_args,
        icon = purrr::when(
          x$icon,
          inherits(., c("shiny.tag", "shiny.tag.list")) ~ x$icon,
          is.character(.) ~ icon_sb(x$icon),
          ~ NULL
        )
      )
    )
  })
}




#' Write wrappers
#' @family ui
#' Writes convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' named `col_1`, `col_2` etc.
#' @aliases col_1 col_2 col_3 col_4 col_5 col_6 col_7 col_8 col_9 col_10 col_11 col_12

write_col_fns <- function(file = "R/utils_col_fns.R") {
  UU::mkpath(file, mkfile = TRUE)
  purrr::imap_chr(rlang::set_names(glue::glue("col_{1:12}")), ~{
    width <- as.numeric(stringr::str_extract(.x, "\\d+$"))
    paste(
      glue::glue("#' @title Create a column of width {width}"),
      "#' @inherit shiny::column params return",
      "#' @family ui",
      "#' @export",
      glue::glue_collapse(c(.y, " <- ", capture.output(
        dput(
          rlang::new_function(
            args = rlang::pairlist2(... =),
            body = rlang::expr(shiny::column(!!width, ...))
          )
        )
      ))),
      sep = "\n"
    )
   }) |>
    write(file)
}


#' Render an auto-sized column
#'
#' @inherit htmltools::builder params return
#' @family ui
#' @return \code{shiny.tag}
#' @export
#'
#' @examples
#' col_auto()
col_auto <- function(..., .noWS = NULL, .renderHook = NULL) {
  shiny::div(class = "column", ...)
}


#' Update the Virga Labs glossary
#'
#' @inheritParams googlesheets4::read_sheet
#' @inheritParams base::dump
#'
#' @return \code{chr} file path invisibly


glossary_sync <-
  function(ss = "163ArY3cL67Vp-gzqjKSw_4r2kl-pCqMRsKDrK_zgbM0",
           sheet = "Main",
           file = "R/glossary.R")  {
    virgaUtils::google_auth()
    .glossary <- googlesheets4::read_sheet(ss, sheet = sheet)

    # Force remove duplicates
    glossary <- dplyr::distinct(.glossary, tolower(Acronym), .keep_all = TRUE)
    dif <- setdiff(.glossary$Acronym, glossary$Acronym)
    if (!rlang::is_empty(dif))
      UU::gwarn("The glossary has duplicates. Please remove these duplicate entries: {dif}")

    dump("glossary", file)
    invisible(file)
  }
#' Add definitions to acronyms
#' @description Uses the [glossary](https://docs.google.com/spreadsheets/d/163ArY3cL67Vp-gzqjKSw_4r2kl-pCqMRsKDrK_zgbM0/edit#gid=0) to make tooltip definitions
#' @param x \code{(chr)} to add tooltips too
#' @param as_text \code{(lgl)} if x is for a tooltip or otherwise needs to be plain text - a definition will be inserted as follows: `ACRONYM (DEFINITION)` since nested tooltips do not render
#' @param .glossary \code{tbl} of glossary items with the following structure
#' \itemize{
#'   \item{\code{Acronym}}{ Column with acronym abbreviations}
#'   \item{\code{Definition}}{ Column with acronym definitions}
#' }
#' @return \code{(list)} with shiny.tags that will render the acronyms with definitions
#' @export
#'
#' @examples
#' glossarize("A DMDU Example")
glossarize <- function(x, as_text = FALSE, .glossary = glossary) {
  acronyms <- stringr::str_extract_all(as.character(x), stringr::regex(UU::regex_or(.glossary$Acronym, pre = "(?<![A-Za-z\\-\\'0-9])", suf = "(?![A-Za-z\\-\\'0-9])"), ignore_case = TRUE))
  acr_legit <- purrr::map_lgl(acronyms, UU::is_legit)
  acr_idx <- which(acr_legit)
  acronyms <- acronyms[acr_idx]
  if (UU::is_legit(acronyms)) {
    need_definitions <- purrr::keep(stringr::str_split(x[acr_idx], UU::regex_or(do.call(c, acronyms), pre = "(?<![A-Za-z\\-\\'0-9])", suf = "(?![A-Za-z\\-\\'0-9])")), ~length(.x) > 1)

    replacements <- purrr::map2(need_definitions, acronyms, ~{
      out <- .x
      # Create the defined glossary terms
      insertions <- purrr::map(.y, ~{
        # Format the acronym for lookup in the glossary here
        .acronym <- trimws(.x)
        # Consider all case combinations
        .acronym <- c(.acronym, toupper(.acronym), tolower(.acronym), snakecase::to_sentence_case(.acronym))
        def_row <- .glossary[.glossary$Acronym %in% .acronym, ]
        if (as_text)
          glue::glue("{.x} ({def_row$Definition})")
        else {
          tooltip <- tagList(def_row$Definition)
          if (UU::is_legit(def_row$Link)) {
            tooltip <- rlang::exec(htmltools::tagAppendChildren, tooltip, !!!purrr::map(stringr::str_split(def_row$Link, "\\s")[[1]], ~htmltools::tags$a(href = .x, "[Link]", target = "_blank")))
            .interactive <- TRUE
          } else
            .interactive <- FALSE

          # Use the acronym as it's formatted in the document
          tippy::tippy(tags$a(.x, class = "tippy"), content = htmltools::doRenderTags(tooltip), allowHTML = TRUE, interactive = .interactive)
        }
      })
      # Tracks the index of the tip to insert
      out <- append(purrr::flatten(sapply(seq_along(insertions), function(i) append(out[i], insertions[i], i), simplify = FALSE)), dplyr::last(.x))
      glue::glue_collapse(purrr::map(out, ~htmltools::doRenderTags(purrr::when(inherits(.x, c("htmlwidget", "shiny.tag.list")), isTRUE(.) ~ htmltools::tags$span(.x), ~shiny::HTML(.x)))))
    })

    x[acr_idx] <- replacements
  }
  if (length(x) == 1)
    x[[1]]
  else
    x
}
