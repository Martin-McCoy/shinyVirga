

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

#' Creates an always up to date italicized copyright element.
#' @family ui
#' @param copyright_holder \code{shiny.tag} to display for the copyright
#' @param class \code{chr} additional classes for the parent div
#' @param logo \code{shiny.tag} A logo to display after the copyright message. Set to NULL to turn off.
#' @return \code{shiny.tag}
#' @export
#'

copyright <- function(copyright_holder = shiny::a("Virga Labs ", href = "https://www.virgalabs.io/", target = "_blank"), class = "", logo = img_base64(css_props = list(`max-width` = "3em", `max-height` = "1em", display = "inline-block;"))) {

  htmltools::withTags(
    div(
      class = paste("inline-block", class), "All Rights Reserved", copyright_holder, " Ⓒ ", span(id = "c_year")
,      script(type = "text/javascript", "
           document.getElementById('c_year').innerHTML = `${new Date().getFullYear()}`;
           "),
      if (UU::is_legit(logo))
        logo

    )
  )
}

img_base64 <- function(path = system.file("img/virga_logo.png", package = "shinyVirga"), css_props) {

  shiny::img(
    src = paste0("data:image/png;base64,", base64enc::base64encode(path)),
    style = if (!missing(css_props))
      css_props(declarations = css_props)
  )
}

#' Construct a simple Bootstrap card
#'
#' @param ... \code{shiny.tag}s
#' @param width \code{num} Width of the encapsulating column
#' @param class \code{chr} any additional classes added to the div.card
#' @param style \code{chr} of the div.card element
#' @param id \code{chr} added to the div.card
#'
#' @return \code{shiny.tag}
#' @export
#'
simpleCard <- function(...,
                       width = 12,
                       class = NULL,
                       id = NULL,
                       style = NULL) {
  shiny::column(
    width = width,
    tags$div(
      style = style,
      class = trimws(paste("card", class)),
      id = id,
      tags$div(
        class = "card-body",
        ...
      )
    )
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
                   tip_icon = NULL,
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
                tip_icon = tip_icon,
                gradient = gradient,
                boxToolSize = "sm",
                elevation = elevation,
                headerBorder = headerBorder,
                label = label,
                dropdownMenu = dropdownMenu,
                sidebar = sidebar,
                id = id)

  if (box || UU::is_legit(.dots)) {
    out <- eval(
      rlang::call2(
        if (box)
          bs4Dash::box
        else
          shiny::tagList,
        !!!if (UU::is_legit(.dots) && box) {
          append(.args, .dots)
        } else if (box) {
          .args
        } else {
          .dots
        }
        )
      )

  } else
    out <- NULL

  if (row)
    out <- shiny::fluidRow(class = trimws(paste("ui_row", class)), id = id, out)

  has_class <- !is.null(class)
  has_attr <- !is.null(add_attribs)
  if (has_class || has_attr) {

    if (has_class) {
      out <- tagAppendAttributes(out, class = class, .cssSelector = ".card")
    }

    if (has_attr) {
      o <- htmltools::tagQuery(out)
      for (selector in names(add_attribs)) {
        do.call(o$find(selector)$addAttrs, add_attribs[[selector]])
        out <- o$allTags()
      }
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
  if (!is.null(status)) {
    status <- UU::match_letters(status, n = 2, bs4Dash:::validStatuses)
    bs4Dash:::validateStatus(status)
    status <- paste0("alert-",status)
  } else {
    status <- ""
  }

  shiny::tags$div(class = paste0("alert ",status), role = "alert", ..., style = paste0("margin: 6px 5px 6px 15px;", ifelse(grepl(";$", style), style, paste0(style, ";"))), id = id)
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
        icon = if (inherits(x$icon, c("shiny.tag", "shiny.tag.list"))) {
          x$icon
        } else if (is.character(x$icon)) {
          icon_sb(x$icon)
        } else {
          NULL
        }
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
  e <- rlang::env(matches = "A-Za-z\\-\\'\\/0-9\\.")
  reg <- purrr::map_vec(.ptype = character(), c(pre = "(?<![{matches}])", suf = "(?![{matches}])"), glue::glue, .envir = e)
  acronyms <- stringr::str_extract_all(as.character(x), stringr::regex(UU::regex_or(.glossary$Acronym, pre = reg["pre"], suf = reg["suf"]), ignore_case = TRUE))
  acr_legit <- purrr::map_lgl(acronyms, UU::is_legit)
  acr_idx <- which(acr_legit)
  acronyms <- acronyms[acr_idx]
  if (UU::is_legit(acronyms)) {
    need_definitions <- purrr::keep(stringr::str_split(x[acr_idx], UU::regex_or(do.call(c, acronyms), pre = reg["pre"], suf = reg["suf"])), ~length(.x) > 1)

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

#' Is a shiny.tag or shiny.tag.list a Bootstrap card?
#'
#' @param x \code{shiny.tag/shiny.tag.list}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' is_card(bs4Dash::bs4Card())

is_card <- function(x) {
  any(purrr::pluck(x, "children",1,"attribs",1) == "card") || any(purrr::pluck(x, "children",1,"children",1,"attribs",1) == "card")
}

#' Is a shiny.tag or shiny.tag.list have a Bootstrap card in the nesting structure?
#'
#' @param x \code{shiny.tag/shiny.tag.list}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' has_card(shiny::tagList(shiny::div(bs4Dash::bs4Card())))

has_card <- function(x) {
  any(rapply(bs4Dash::bs4Card(), classes = "character", \(.x) {
    stringr::str_detect(.x, "card")
  }))
}

#' Convert a string to a valid HTML ID
#'
#' @param x \code{chr}
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' nm_to_id("A random string")
nm_to_id <- function(x) {
  paste0(stringr::str_extract_all(tolower(x), "[[:alnum:]]+")[[1]], collapse = "_")
}

#' @title Wrap in a \link[bs4Dash]{bs4Card} with default options
#' @description Wraps in a width 12 card, converts the title to an ID by replacing spaces with \code{_}. Adds a \code{tip_icon} if a tip named with the title exists in \code{tips}.
#' @inherit bs4Dash::bs4Card params
#' @inheritDotParams bs4Dash::bs4Card
#' @export

grid_card <- function(..., title = NULL, id = NULL, class = NULL, tip_icon = NULL, width = 12, maximizable = TRUE, elevation = 2, more_info = NULL, collapsible = FALSE) {
  if (is.null(id) && !is.null(title))
    id <- nm_to_id(title)

  if (!is.null(title) && !is.null(tips[[title]]) && is.null(tip_icon))
    tip_icon <- tippy::tippy(shiny::icon("info-circle", verify_fa = FALSE), htmltools::doRenderTags(tips[[title]]), interactive = TRUE, allowHTML = TRUE)

  if (exists("acc_info") && !is.null(title) && !is.null(acc_info[[title]]) && is.null(more_info))
    more_info <- bs4Dash::accordion(
      id = paste0(id, "_info"),
      class =
      bs4Dash::accordionItem(
        tags$p(HTML(acc_info[[title]])),
        title = tagList("More Info", shiny::icon("chevron-down", class = "float-right"))
      )
    )

  .args <- rlang::list2(
    ...,
    class = class,
    footer = more_info,
    title = title,
    id = id,
    width = width,
    tip_icon = tip_icon,
    maximizable = maximizable,
    elevation = elevation,
    collapsible = collapsible
  )
  do.call(bs4Dash::bs4Card, .args) |>
    tagAppendAttributes(
      class = "grid_card"
    )
}

row_wrap <- function(x, nm, icons) {
  UseMethod("row_wrap")
}
#' @title S3 Method for row_wrap shiny.tag.list
#' @export
row_wrap.shiny.tag.list <- function(x, nm, icons) {
  if (!missing(nm) && !is_card(x)) {
    tl <- grid_card(x, title = nm)
  } else {
    tl <- x
  }
  shiny::fluidRow(tl)
}
#' @title S3 Method for row_wrap shiny.tag
#' @export
row_wrap.shiny.tag <- row_wrap.shiny.tag.list

#' @title S3 Method for row_wrap list
#' @export
row_wrap.list <- function(x, nm, icons) {
  tl <- do.call(shiny::tagList, purrr::imap(x, ~ {
    grid_card(
      .x,
      title = .y,
      width = 12 %/% length(x),
    )
  }))
  if (!missing(nm) && !is_card(x)) {
    tl <- grid_card(tl, nm)
  }
  shiny::fluidRow(tl)
}

#' @title Create a gridded card layout
#' @description Creates a grid layout informed by the [design spec](https://miro.com/app/board/o9J_l90QVck=/?moveToWidget=3074457361986467451&cot=14)
#' @param ... Any number of \link[shiny]{tag}s or \link[shiny]{tagList}s or `list`s (each treated as a separate entity to be housed in a row.). `shiny.tag`s and `shiny.tag.list`s, if named, will be inside of a \link[bs4Dash]{bs4Card} with the name as the title on it's own row. A `list` with named objects will be on a row with individual \link[bs4Dash]{bs4Card}s.
#' @param description A shiny tag or tagList to be added to the description box
#' @param infoboxes \code{(named list/tagList)} with length divisible by 12. Will be added to the right of the description
#' \itemize{
#'   \item{\code{named_list}}{ A named list of values to be put into infoboxes. Infobox will be titled by the name of the list item.}
#'   \item{\code{tag/tagList}}{ shiny tag or tagList of items to include. These will individually be added to appropriate sized columns.}
#' }
#' @param graphs \code{shiny.tag/shiny.tag.list} Graphics to be placed in the further right column of the top summary area
#' @param top_row \code{shiny.tag/shiny.tag.list} All elements belonging to the top row
#' @return A `shiny::fluidPage` with the grid layout
#' @export

card_grid <- function(..., icons = FALSE, top_row = NULL, description, infoboxes, graphs) {

  # Top Row Summary ----
  # Wed Sep 08 20:04:03 2021

  has <- c(
    description = !missing(description),
    infobox = !missing(infoboxes),
    graphs = !missing(graphs)
  )
  if (is.null(top_row) && any(has))
    top_row <- list()
  e <- environment()
  purrr::iwalk(has, \(.x, .y) {
    if (.x) {
      top_row[[.y]] <<- get0(.y, envir = e, inherits = FALSE)
    }
  })



  if (UU::is_legit(top_row)) {
    col_width <- 12 %/% length(top_row)
    top_row <- shinyVirga::ui_row(
      rlang::exec(fluidRow, purrr::imap(top_row, \(.x, .y) {
        shinyVirga::ui_row(
          row = FALSE,
          width = col_width,
          .x,
          class = "grid-summary",
          id = paste0("grid-summary-", .y)
        )
      })) ,
      title = "Summary",
      width = 12,
      collapsible = TRUE,
      elevation = 2,
      id = "summary"
    )
  }




  # Row Viz ----
  # Wed Sep 08 20:03:52 2021
  viz <- rlang::dots_list(..., .named = TRUE)
  if (UU::is_legit(viz)) {
    .viz <- list()
    for (r in seq_along(viz)) {
      if (!names(viz[r]) %in% c("list(...)", "fluidRow(...)", "<shiny.tg>") && UU::is_legit(names(viz[r]))) {
        # Case where a named entity as provided
        .viz[[r]] <- row_wrap(viz[[r]], names(viz[r]), icons = icons)
      }  else if (!inherits(viz[[r]], c("shiny.tag", "shiny.tag.list"))) {
        # Case where a list is provided
        .viz[[r]] = row_wrap(viz[[r]], icons = icons)
      } else {
        # Case where shiny.tag is provided as is
        .viz[[r]] <- viz[[r]]
      }
    }
    .viz <- do.call(shiny::tagList, .viz)
  } else {
    .viz <- viz
  }



  shiny::fluidPage(
    top_row,
    .viz,
    tags$script( type = "text/javascript",
                 UU::as_js("
     $(document).ready(() => {
       $(function(){
          let card = $('.card');
          let find = ['[data-card-widget=\"maximize\"]','.accordion a.collapsed'];
          let controls = find.map((e, i) => {
            return card.find(e);
          });
          function doResize(e) {
            // it may take some time for the resizing of the card to happen
            setTimeout(function(){
              let target = $(e.target);
              let charts = target
              .closest('.grid_card')
              .find('.echarts4r')
              let charts_n = charts.length;
              charts.each(function(index){
                let chart = charts[index];

                if(!chart)
                  return;

                let id = $(chart).attr('id');

                let $parent = $(chart).parent();
                let w = $parent.width();
                let h = $parent.height() / charts_n;

                console.log(`Resizing ${id}: w: ${w}px h: ${h}px`)
                get_e_charts(id).resize({width: w, height: h});
              })
            }, 250);
          }
          controls.forEach((e, i) => {
            return e.on('click', doResize);
          })
        });
     })
     "))
  )

}
