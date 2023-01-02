
#' Create a staging directory for deployment files
#'
#' @param deploy_path \code{chr} directory in which to stage deployment files
#' @param use_renv \code{lgl} whether to use the renv.lock & `golem::add_dockerfile_with_renv` (`TRUE`) or the DESCRIPTION `golem::add_dockerfile`
#' @param copy_r_environ \code{lgl} Whether to include the _.Renviron_ file
#' @param copy_r_profile \code{lgl} Whether to include the _.Rprofile_ file
#' @param copy_renv_lock \code{lgl} Whether to include the _renv.lock_ file
#' @param copy_dockerfile \code{lgl} Whether to include the _Dockerfile_
#' @return \code{dir} a directory with the tar.gz package executable, and requisite files for docker image building.
#' @export
deploy_tar <- function(deploy_path = "deploy",
                       use_renv = TRUE,
                       copy_r_environ = TRUE,
                       copy_r_profile = TRUE,
                       copy_renv_lock = TRUE,
                       copy_dockerfile = TRUE,
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
      "renv.lock",
    Dockerfile = if (copy_dockerfile)
      "Dockerfile"
  )


  if (UU::is_legit(copy_files)) {
    #only copy if newer than ones in deploy
    copy_files <- purrr::keep(copy_files, ~rlang::`%|%`(UU::last_updated(.x), Sys.Date()) > rlang::`%|%`(UU::last_updated(fs::path(deploy_path,.x)), lubridate::make_date()))
    purrr::walk(copy_files, ~ file.copy(.x, fs::path(deploy_path, .x), overwrite = TRUE))
  }

  if (!made_dockerfile) {
    devtools::document(roclets = c('rd', 'collate', 'namespace'))
    devtools::build(path = deploy_path)
  }
}
#' Run DMDU Deployment as a background task
#' @inheritParams deploy_tar
#' @return \code{job} Background job that deploys the dmdu app (not working locally)
#' @export
deploy_stage <- function(deploy_path = "deploy",
                         use_renv = TRUE,
                         copy_r_environ = TRUE,
                         copy_r_profile = TRUE,
                         copy_renv_lock = TRUE,
                         lockfile_path = "renv.lock",
                         copy_dockerfile = TRUE,
                         GITHUB_PAT = remotes:::github_pat(),
                         docker_image_tag = paste0(pkgload::pkg_name(), ":latest")) {

  UU::mkpath(deploy_path)
  lockfile_path <- fs::path_abs(lockfile_path)

  job <- rlang::expr({

    docker_image_tag <- !!docker_image_tag
    GITHUB_PAT <- !!GITHUB_PAT

    copy_files <- c(
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
        golem::add_dockerfile_with_renv(output_dir = ".", lockfile = !!lockfile_path)
      } else {
        golem::add_dockerfile(output = file.path(deploy_path, "Dockerfile"))
      }

      # Need to build dmdu_base first if you just created it with the
      # split-dockerfile methodology employed by add_dockerfile_with_renv above

      # same negation logic as earlier
      # All of these unix sed commands modify the dockerfiles built by add_dockerfile_with_renv
      # above to match our methodology used to pull github pats into dockerfiles
      # to download the needed private repositories
      system("sed -i '' -e '1s/^//p; 1s/^.*/RUN GITHUB_PAT=$GITHUB_PAT/' Dockerfile_base") # puts RUN GITHUB_PAT command in line 2
      system("sed -i '' -e '1s/^//p; 1s/^.*/RUN GITHUB_PAT=$GITHUB_PAT/' Dockerfile")
      system("sed -i '' -e '1s/^//p; 1s/^.*/ARG GITHUB_PAT/' Dockerfile_base") # puts ARG GITHUB_PAT in line 2
      system("sed -i '' -e '1s/^//p; 1s/^.*/ARG GITHUB_PAT/' Dockerfile")
      system("sed -i '' -e '4s/^//p; 4s/^.*/COPY .Rprofile .Rprofile/' Dockerfile") # Adds line for copying .Rprofile and .Renviron
      system("sed -i '' -e '4s/^//p; 4s/^.*/COPY .Renviron .Renviron/' Dockerfile")
      system("sed -i '' 's/RUN R -e/RUN GITHUB_PAT=$GITHUB_PAT R -e/g' Dockerfile_base") # adds in necessary GITHUB_PAT args to each R RUn commands
      system("sed -i '' 's/RUN R -e/RUN GITHUB_PAT=$GITHUB_PAT R -e/g' Dockerfile")
      system("sed -i '' 's/renv.lock.prod/renv.lock/g' Dockerfile") # gets rid of pesky renv.lock.prod
      system(glue::glue("docker build --build-arg GITHUB_PAT={GITHUB_PAT} -f Dockerfile_base --progress=plain -t dmdu_base .")) # build

    }


    if (!made_dockerfile) {
      devtools::document(roclets = c('rd', 'collate', 'namespace'))
      devtools::build(path = ".")
    }



    # build latest dockerfile (either from base above or from a single Dockerfile)
    system(glue::glue("docker build --build-arg GITHUB_PAT={GITHUB_PAT} -f Dockerfile --progress=plain -t {docker_image_tag} ."))
  })
  job_path <- fs::path(deploy_path, "job.R")
  write(deparse(job), job_path)
  rstudioapi::jobRunScript(job_path, name = "Deploy dmdu")
  UU::ignore_files("deploy/")
}
