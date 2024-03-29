% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_js.R
\name{js_click_to_close}
\alias{js_click_to_close}
\title{Title}
\usage{
js_click_to_close(
  id,
  panel_id,
  asis = FALSE,
  asis_panel = FALSE,
  .ns = ns_find()
)
}
\arguments{
\item{id}{\code{(chr)} Automatically namespaced with \code{ns} if inside a module}

\item{panel_id}{ID of the panel to close/open when clicking outside. \code{asis} applies to this argument as well}

\item{asis}{If \code{TRUE}, use the ID as-is even when inside a module
(instead of adding the namespace prefix to the ID).}

\item{asis_panel}{If the panel_id should be left asis \code{TRUE} or namespaced \code{FALSE}}

\item{.ns}{\code{fun} ns function. Typically found automatically.}
}
\value{
\code{shiny.tag} with script that adds event listener
}
\description{
Title
}
