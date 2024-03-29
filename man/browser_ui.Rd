% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_general.R
\name{browser_ui}
\alias{browser_ui}
\title{Add the invisible browser button ui component}
\usage{
browser_ui(.ns = ns_find())
}
\arguments{
\item{ns}{\code{(function)} the \code{ns} function. Typically called from the parent environment, but in some cases where this function is heavily nested it may need to be provided directly.}
}
\value{
\code{(shiny.tag.list)} with the browser button and the script that hides it.
}
\description{
Add the invisible browser button ui component
}
\seealso{
Other general: 
\code{\link{browser_server}()},
\code{\link{is_shiny.tag}()},
\code{\link{ns_custom}()},
\code{\link{ns_find}()},
\code{\link{tab_ns_extract}()},
\code{\link{warn_id}()}

Other debugging: 
\code{\link{browser_server}()},
\code{\link{msg_mod_fun}()},
\code{\link{use_msg_mod_fun}()}
}
\concept{debugging}
\concept{general}
