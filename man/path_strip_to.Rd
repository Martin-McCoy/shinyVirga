% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_general.R
\name{path_strip_to}
\alias{path_strip_to}
\title{Strip a file path to everything after resourcepath}
\usage{
path_strip_to(path, resourcepath = "www", leading_slash = FALSE)
}
\arguments{
\item{path}{\code{chr}}

\item{resourcepath}{\code{chr} A resource path specified in \emph{app_ui.R}}
}
\value{
\code{chr} without stripped directories
}
\description{
Useful for linking to internal files, such as with image source attributes \verb{<img src="[path]">}
}
\examples{
path_strip_to("inst/app/www/img/myimage.svg", "www")
}
\seealso{
Other general: 
\code{\link{browser_server}()},
\code{\link{browser_ui}()},
\code{\link{is_shiny.tag}()},
\code{\link{ns_find}()},
\code{\link{path_strip_shiny}()},
\code{\link{tab_ns_extract}()},
\code{\link{warn_id}()}
}
\concept{general}
