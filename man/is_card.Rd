% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_ui.R
\name{is_card}
\alias{is_card}
\title{Is a shiny.tag or shiny.tag.list a Bootstrap card?}
\usage{
is_card(x)
}
\arguments{
\item{x}{\code{shiny.tag/shiny.tag.list}}
}
\value{
\code{lgl}
}
\description{
Is a shiny.tag or shiny.tag.list a Bootstrap card?
}
\examples{
is_card(bs4Dash::bs4Card())
}
