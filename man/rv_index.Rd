% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_reactives.R
\name{rv_index}
\alias{rv_index}
\title{Index into a reactiveValues object and return a list}
\usage{
rv_index(x, indices, assign = FALSE)
}
\arguments{
\item{x}{\code{reactiveValues}}

\item{indices}{\code{chr/num} valid indexing object}

\item{assign}{\code{lgl} whether to assign the values in \code{indices}to their respective names in \code{x}}
}
\value{
\code{reactiveValues/list} depending on the value of \code{assign}
}
\description{
Index into a reactiveValues object and return a list
}
\seealso{
Other reactives: 
\code{\link{rv_to_list}()},
\code{\link{rv}()}
}
\concept{reactives}
