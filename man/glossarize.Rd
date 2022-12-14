% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_ui.R
\name{glossarize}
\alias{glossarize}
\title{Add definitions to acronyms}
\usage{
glossarize(x, as_text = FALSE, .glossary = glossary)
}
\arguments{
\item{x}{\code{(chr)} to add tooltips too}

\item{as_text}{\code{(lgl)} if x is for a tooltip or otherwise needs to be plain text - a definition will be inserted as follows: \code{ACRONYM (DEFINITION)} since nested tooltips do not render}

\item{.glossary}{\code{tbl} of glossary items with the following structure
\itemize{
\item{\code{Acronym}}{ Column with acronym abbreviations}
\item{\code{Definition}}{ Column with acronym definitions}
}}
}
\value{
\code{(list)} with shiny.tags that will render the acronyms with definitions
}
\description{
Uses the \href{https://docs.google.com/spreadsheets/d/163ArY3cL67Vp-gzqjKSw_4r2kl-pCqMRsKDrK_zgbM0/edit#gid=0}{glossary} to make tooltip definitions
}
\examples{
glossarize("A DMDU Example")
}
