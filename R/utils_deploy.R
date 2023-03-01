
#' Create a staging directory for deployment files
#' @family deploy
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
#' @return \code{job} Background job that deploys the dmdu app (not working locally)
#' @export
deploy_stage <- function(deploy_path = "deploy",
                         use_renv = TRUE,
                         copy_r_environ = TRUE,
                         copy_r_profile = TRUE,
                         copy_renv_lock = TRUE,
                         copy_files = NULL,
                         lockfile_path = "renv.lock",
                         copy_dockerfile = TRUE,
                         GITHUB_PAT = remotes:::github_pat(),
                         docker_image_tag = tolower(golem::get_golem_name())) {

  UU::mkpath(deploy_path)
  lockfile_path <- fs::path_abs(lockfile_path)

  images <- rlang::parse_expr(capture.output(dput(rlang::set_names(sprintf("%s%s:%s", docker_image_tag, c("", "_base"),"latest"), c("main", "base")))))

  job <- rlang::expr({

    docker_image_tags <- !!images

    GITHUB_PAT <- !!GITHUB_PAT

    copy_files <- c(
      copy_files,
      # c auto removes nulls
      Renviron = if (!!copy_r_environ)
        !!fs::path_abs(".Renviron"),
      Rprofile = if (!!copy_r_profile)
        !!fs::path_abs(".Rprofile"),
      renv_lock = if (!!copy_renv_lock)
        !!lockfile_path,
      Dockerfile = if (!!copy_dockerfile)
        !!fs::path_abs("Dockerfile")
    )


    if (UU::is_legit(copy_files)) {
      #only copy if newer than ones in deploy
      copy_files <- purrr::keep(copy_files, ~rlang::`%|%`(UU::last_updated(.x), Sys.Date()) > rlang::`%|%`(UU::last_updated(basename(.x)), lubridate::make_date()))
      purrr::walk(copy_files, ~ file.copy(.x, basename(.x), overwrite = TRUE))
    }

    made_dockerfile <- !file.exists("Dockerfile")

    if (made_dockerfile) {
      # this needed to be negated to match logic above
      if (!!use_renv) {
        golem::add_dockerfile_with_renv(source_folder = "../", output_dir = ".", lockfile = !!lockfile_path)
      } else {
        golem::add_dockerfile()
      }

      write(glue::glue("GITHUB_PAT = {GITHUB_PAT}"), ".Renviron", append = !!copy_r_environ)

      # Need to build dmdu_base first if you just created it with the
      # split-dockerfile methodology employed by add_dockerfile_with_renv above

      # same negation logic as earlier
      # All of these unix sed commands modify the dockerfiles built by add_dockerfile_with_renv
      # above to match our methodology used to pull github pats into dockerfiles
      # to download the needed private repositories
      system("sed -i '' -e '1s/^//p; 1s/^.*/COPY .Renviron .Renviron/' Dockerfile_base")
      system("sed -i '' -e '1s/^//p; 1s/^.*/COPY .Renviron .Renviron/' Dockerfile")
      system("sed -i '' -e '4s/^//p; 4s/^.*/COPY .Rprofile .Rprofile/' Dockerfile")
      # system("sed -i '' 's/renv.lock.prod/renv.lock/g' Dockerfile") # gets rid of pesky renv.lock.prod
      if (!(!!copy_r_environ)) {
        # If the user did not opt to copy the .Renviron file, delete it
        write("RUN rm .Renviron", file = "Dockerfile")
        file.remove(".Renviron")
      }


    }


    if (!made_dockerfile) {
      devtools::document(roclets = c('rd', 'collate', 'namespace'))
      devtools::build(path = ".")
    }

    # Combine dockerfiles since the two-step method doesn't work
    dockerfiles <- purrr::map(rlang::set_names(paste0("Dockerfile", c("", "_base"))), readLines)

    dockerfile <- c(
      dockerfiles$Dockerfile_base,
      # Everything after the renv::restore, since the last call in Dockerfile_base is renv::restore
      dockerfiles$Dockerfile[(stringr::str_which(dockerfiles$Dockerfile, "RUN R -e 'renv::restore\\(\\)'") + 1):length(dockerfiles$Dockerfile)]
    )
    write(dockerfile, "Dockerfile")

    # build latest dockerfile (either from base above or from a single Dockerfile)
    system(glue::glue("docker build -f Dockerfile --progress=plain -t {docker_image_tags['main']} ."))
  })
  job_path <- fs::path(deploy_path, "job.R")
  write(deparse(job), job_path)
  rstudioapi::jobRunScript(job_path, name = glue::glue("Build {docker_image_tag} Docker images"))
  UU::ignore_files("deploy/")
}
