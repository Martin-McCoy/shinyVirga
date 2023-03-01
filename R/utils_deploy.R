
#' Create a staging directory for deployment files
#' @family deploy
#' @description `r lifecycle::badge("experimental")`
#' @param deploy_path \code{chr} directory in which to stage deployment files
#' @param use_renv \code{lgl} whether to use the renv.lock & `golem::add_dockerfile_with_renv` (`TRUE`) or the DESCRIPTION `golem::add_dockerfile`
#' @param copy_r_environ \code{lgl} Whether to include the _.Renviron_ file
#' @param copy_r_profile \code{lgl} Whether to include the _.Rprofile_ file
#' @param copy_renv_lock \code{lgl} Whether to include the _renv.lock_ file
#' @param copy_files \code{chr} vector of files to copy in addition to default.
#' @param rebuild \code{lgl} Whether to rebuild the tar.gz
#' @param remove_previous_builds \code{lgl} Whether to remove previous build files from `deploy_path` folder
#' @return \code{msg} All files are updated based on other arguments and an informative message indicates files and
#' @export
deploy_tar <- function(deploy_path = "deploy",
                       use_renv = TRUE,
                       copy_r_environ = TRUE,
                       copy_r_profile = TRUE,
                       copy_renv_lock = TRUE,
                       copy_files = NULL,
                       rebuild = TRUE,
                       remove_previous_builds = TRUE,
                       lockfile_path = "renv.lock") {
  # make the deploy dir
  UU::mkpath(deploy_path)

  # False is there is a file called Dockerfile in deploy dir
  made_dockerfile <- !file.exists(fs::path(deploy_path, "Dockerfile")) && !copy_dockerfile

  # If there isn't a Dockerfile in deploy dir and you don't want to include the
  # root dir Dockerfile, then make the split-dockerfiles with the golem func

  if (made_dockerfile) {
    if (use_renv) {
      golem::add_dockerfile_with_renv(output_dir = deploy_path, lockfile = lockfile_path)
    } else {
      golem::add_dockerfile()
    }
  }


  copy_files <- c(
    # c auto removes nulls
    Renviron = if (copy_r_environ)
      ".Renviron",
    Rprofile = if (copy_r_profile)
      ".Rprofile",
    renv_lock = if (copy_renv_lock)
      "renv.lock"
  )


  if (UU::is_legit(copy_files)) {
    #only copy if newer than ones in deploy
    copy_files <- purrr::keep(copy_files, ~rlang::`%|%`(UU::last_updated(.x), Sys.Date()) > rlang::`%|%`(UU::last_updated(fs::path(deploy_path,.x)), lubridate::make_date()))
    purrr::walk(copy_files, ~ file.copy(.x, fs::path(deploy_path, .x), overwrite = TRUE))
  }

  devtools::document(roclets = c('rd', 'collate', 'namespace'))
  if (remove_previous_builds) {
    old_builds <- UU::list.files2(deploy_path, pattern = "tar.gz$")
    if (UU::is_legit(old_builds))
      purrr::walk(old_builds, file.remove)
  }
  copy_files <- fs::path(deploy_path, copy_files)
  if (rebuild)
    copy_files <- c(copy_files, devtools::build(path = deploy_path))

  if (UU::is_legit(copy_files)) {
    cli::cli_alert_success("The following files were updated:")
    purrr::iwalk(copy_files, \(.x, .y) {
      cli::cli_inform(cli::format_inline("{.path {.y}}: {.x}") )
    })
  }


}



#' Run Deployment as a background task
#' @inheritParams deploy_tar
#' @description `r lifecycle::badge("experimental")`
#' @family deploy
#' @param GITHUB_PAT \code{chr} Github Personal Access Token. ** Default remotes:::github_pat() **. Includes the Github Personal Access Token in _.Renviron_ copied to Docker image for fetching Github repos during package installation. Set to `NULL` to turn off.
#' @param launch_job \code{lgl} whether to launch the deployment script _jobs.R_ in the `deploy_path` directory after writing it.
#' @return \code{job} Background job that deploys the dmdu app (not working locally)
#' @export
deploy_stage <- function(deploy_path = "deploy",
                         use_renv = TRUE,
                         copy_r_environ = TRUE,
                         copy_r_profile = TRUE,
                         copy_renv_lock = TRUE,
                         copy_files = NULL,
                         rebuild = TRUE,
                         remove_previous_builds = TRUE,
                         launch_job = TRUE,
                         lockfile_path = "renv.lock",
                         GITHUB_PAT = remotes:::github_pat(),
                         docker_image_tag = tolower(golem::get_golem_name())) {

  UU::mkpath(deploy_path)
  lockfile_path <- fs::path_abs(lockfile_path)


  opts <- list(
    deploy_path = deploy_path,
    include_github_pat = is.character(GITHUB_PAT),
    use_renv = use_renv,
    copy_r_environ = copy_r_environ,
    copy_r_profile = copy_r_profile,
    copy_renv_lock = copy_renv_lock,
    copy_files = copy_files,
    rebuild = rebuild,
    remove_previous_builds = remove_previous_builds,
    lockfile_path = rlang::expr(!!fs::path_abs(lockfile_path)),
    docker_image_tags = rlang::parse_expr(capture.output(dput(rlang::set_names(sprintf("%s%s:%s", docker_image_tag, c("", "_base"),"latest"), c("main", "base")))))
  )
  job <- rlang::expr({

    list2env(!!opts, envir = environment())



    copy_files <- c(
      copy_files,
      # c auto removes nulls
      Renviron = !!fs::path_abs(".Renviron"),
      Rprofile = if (copy_r_profile)
        !!fs::path_abs(".Rprofile"),
      renv_lock = if (copy_renv_lock)
        lockfile_path
    )


    if (UU::is_legit(copy_files)) {
      #only copy if newer than ones in deploy
      copy_files <- purrr::keep(copy_files, \(.x) rlang::`%|%`(UU::last_updated(.x), Sys.Date()) > rlang::`%|%`(UU::last_updated(basename(.x)), lubridate::make_date()))
      purrr::walk(copy_files, \(.x) file.copy(.x, basename(.x), overwrite = TRUE))

    }

    if (include_github_pat && file.exists(".Renviron")) {
      write(glue::glue("GITHUB_PAT = {remotes:::github_pat()}"), ".Renviron", append = copy_r_environ)
    } else if (include_github_pat && !file.exists(".Renviron")) {
      UU::gmsg(".Renviron file must be copied temporarily to deploy folder & Docker image for {.code GITHUB_PAT} to be used in Github dependency installation. It will be removed after installation is complete.")
    }

    made_dockerfile <- !file.exists("Dockerfile")
    dockerfiles <- purrr::map(rlang::set_names(paste0("Dockerfile", c("", "_base"))), readLines)
    if (made_dockerfile) {
      # IF use_renv was selected when calling `deploy_stage`
      if (use_renv) {
        golem::add_dockerfile_with_renv(
          source_folder = "../",
          output_dir = ".",
          lockfile = lockfile_path,
          from = "rocker/r-ver:4.2.1@sha256:3e3f21d75482c5c66e122188ae88ad5c89ca24f5202dd07f69c623b3c8af7e80"
        )
      } else {
        golem::add_dockerfile()
      }

    }


    devtools::document(pkgload::pkg_path(), roclets = c('rd', 'collate', 'namespace'))
    if (remove_previous_builds) {
      old_builds <- UU::list.files2(pattern = "tar.gz$")
      if (UU::is_legit(old_builds))
        purrr::walk(old_builds, file.remove)
    }
    if (rebuild)
      copy_files <- c(copy_files, devtools::build())


    # Combine dockerfiles since the two-step method doesn't work
    dockerfile <- c(
      dockerfiles$Dockerfile_base,
      # Everything after the renv::restore, since the last call in Dockerfile_base is renv::restore
      dockerfiles$Dockerfile[(stringr::str_which(dockerfiles$Dockerfile, "RUN R -e 'renv::restore\\(\\)'") + 1):length(dockerfiles$Dockerfile)]
    )
    dockerfile_restore_line_idx <- stringr::str_which(dockerfile, "RUN R -e 'renv::restore\\(\\)'")
    write(dockerfile, "Dockerfile")
    # IF copy_r_environ OR copy_r_profile was set to true when deploy_stage was run
    if (copy_r_environ || copy_r_profile) {
      add_lines <- c(
        # If copy_r_environ
        if (copy_r_environ)
          "COPY .Renviron .Renviron",
        if (copy_r_profile)
          "COPY .Rprofile .Rprofile"

      )
      # Add the .Renviron & .Rprofile right before restoring such that options are available while packages are installing.
      UU::write_lines("Dockerfile", add_lines, after = dockerfile_restore_line_idx - 1)
    }

    if (!copy_r_environ && file.exists(".Renviron")) {
      # If the user did not opt to copy the .Renviron file, delete it from the Docker container and the deploy directory after renv::restore is run
      UU::write_lines("Dockerfile", "RUN rm .Renviron", after = dockerfile_restore_line_idx)
      file.remove(".Renviron")
    }

    # build latest dockerfile (either from base above or from a single Dockerfile)
    system(glue::glue("docker build -f Dockerfile --progress=plain -t {docker_image_tags['main']} ."))
  })
  job_path <- fs::path(deploy_path, "job.R")
  write(deparse(job), job_path)
  if (launch_job)
    rstudioapi::jobRunScript(job_path, name = glue::glue("Build {docker_image_tag} Docker images"))
  UU::ignore_files("deploy/")
}
