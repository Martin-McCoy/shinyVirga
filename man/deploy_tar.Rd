% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_deploy.R
\name{deploy_tar}
\alias{deploy_tar}
\title{Create a staging directory for deployment files}
\usage{
deploy_tar(
  deploy_path = "deploy",
  use_renv = TRUE,
  copy_r_environ = TRUE,
  copy_r_profile = TRUE,
  copy_renv_lock = TRUE,
  copy_dockerfile = TRUE,
  lockfile_path = "renv.lock"
)
}
\arguments{
\item{deploy_path}{\code{chr} directory in which to stage deployment files}

\item{use_renv}{\code{lgl} whether to use the renv.lock & \code{golem::add_dockerfile_with_renv} (\code{TRUE}) or the DESCRIPTION \code{golem::add_dockerfile}}

\item{copy_r_environ}{\code{lgl} Whether to include the \emph{.Renviron} file}

\item{copy_r_profile}{\code{lgl} Whether to include the \emph{.Rprofile} file}

\item{copy_renv_lock}{\code{lgl} Whether to include the \emph{renv.lock} file}

\item{copy_dockerfile}{\code{lgl} Whether to include the \emph{Dockerfile}}
}
\value{
\code{dir} a directory with the tar.gz package executable, and requisite files for docker image building.
}
\description{
Create a staging directory for deployment files
}
