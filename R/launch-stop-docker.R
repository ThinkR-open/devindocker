#' Launch and stop Docker
#'
#' @param project_path Path to project to launch.
#' @param container Docker container to download from docker hub.
#' @param with_mysql Logical. Use MySQL database or not.
#' @param mysql_docker Mysql docker container to download from docker hub.
#' @param with_chrome Logical. Use Chrome in the network or not.
#' @param chrome_docker Chrome docker container to download from docker hub.
#' @param chrome_path Path to local chromium installation.
#' Help with \code{\link[pagedown:find_chrome]{pagedown::find_chrome()}}, but use the real path, not a symlink
#' @param port Local port to which to launch Rstudio Server
#' @param renv_inst Logical. Whether to add a R script with {renv} instructions in the project.
#' @param renv_cache Path to renv cache on your computer. Set to FALSE to not use renv.
#' @param renv_out Whether to set {renv} libraries out of the project.
#' @param renv_out_dir Where to store project libraries. Default to a ".renv" folder
#' in the parent directory of "project_path"
#' @param update_docker Logical. Whether to update Docker container with DockerHub.
#'
#' @importFrom utils browseURL
#'
#' @details
#' renv_out : It is recommended to set {renv library} out of the project to avoid copy of entire library
#' during package checks. See \code{vignette("packages", package = "renv")}.
#'
#' @export
#' @examples
#' \dontrun{
#' tempdir <- tempdir()
#' project_path <- file.path(tempdir, "myproject")
#' usethis::create_package(project_path, open = FALSE)
#'
#' # Launch Docker
#'
#' # Which Rstudio container ? ----
#' container <- c("thinkr/rstudio3_5_2",
#'                "thinkr/rstudio3_5_2_geo",
#'                "thinkr/rstudio3_6_1_geo")[3]
#'
#' # Which port ? ----
#' # _Useful if multiple Rstudio Server to launch
#' port <- 8788
#'
#' # Start Docker project
#' launch_proj_docker(project_path = project_path,
#'                    container = container,
#'                    port = port)
#'
#' # Stop Docker properly
#' stop_proj_docker(project_path = project_path, sleep = 5)
#'
#' # With renv cache shared with host
#' dir.create(file.path(tempdir, "cache"))
#' tempcache <- file.path(tempdir, "cache")
#' # Start Docker project
#' launch_proj_docker(project_path = project_path,
#'                    container = container,
#'                    port = port,
#'                    renv_cache = tempcache)
#'
#' # Stop Docker properly
#' stop_proj_docker(project_path = project_path, sleep = 5)
#' }

launch_proj_docker <- function(project_path = ".",
                               container = "thinkr/rstudio3_6_1_geo",
                               with_mysql = FALSE,
                               mysql_docker = "mysql:8.0.16",
                               with_chrome = FALSE,
                               chrome_docker = "tkp1n/chromium",
                               chrome_path = pagedown::find_chrome(),
                               port = 8787,
                               renv_inst = FALSE,
                               renv_cache = FALSE,
                               renv_out = TRUE,
                               renv_out_dir,
                               update_docker = TRUE
                               # vbox = FALSE
) {
  # @param vbox Logical. If Docker run on windows in a virtual box, paths need to be changed

  project_path <- normalizePath(project_path)

  if (missing(renv_cache) | renv_cache == FALSE) {
    renv_cache <- NULL
  }

  # First time ----
  if (!dir.exists(file.path(project_path, "rstudio_dotrstudio"))) {
    # Hide this file
    # rstudio last files and others
    dir.create(normalizePath(file.path(project_path, "rstudio_dotrstudio"), mustWork = FALSE))
  }
  if (!dir.exists(file.path(project_path, "rstudio_dotconfig"))) {
    # Hide this file
    # rstudio appearance preferences
    dir.create(normalizePath(file.path(project_path, "rstudio_dotconfig"), mustWork = FALSE))
  }
  if (isTRUE(renv_out)) {
    if (missing(renv_out_dir)) {
      renv_out_dir <- normalizePath(file.path(dirname(project_path), ".renv"), mustWork = FALSE)
    }
    if (!dir.exists(renv_out_dir)) {dir.create(renv_out_dir)}
  }

  # .gitignore
  # Files to ignore
  lines <- stats::na.omit(
    c("rstudio_dotrstudio", "rstudio_dotconfig",
      ifelse(renv_inst, "renv_instructions.Rmd", NA),
      ifelse(with_mysql, "database", NA)
    )
  )

  gitfile <- normalizePath(file.path(project_path, ".gitignore"), mustWork = FALSE)
  if (!file.exists(gitfile)) {
    existing_lines <- ""
  } else {
    existing_lines <- readLines(gitfile, warn = FALSE, encoding = "UTF-8")
  }
  new <- setdiff(lines, existing_lines)
  if (length(new) != 0) {
    all <- c(existing_lines, new)
    cat(all, file = gitfile, sep = "\n")
  }

  # .Rbuildignore
  # Files to ignore
  lines <- stats::na.omit(c("rstudio\\_dotconfig",
                            "rstudio\\_dotrstudio",
                            ifelse(renv_inst, "renv\\_instructions\\.Rmd", NA),
                            ifelse(with_mysql, "database", NA)
  ))

  buildfile <- normalizePath(file.path(project_path, ".Rbuildignore"), mustWork = FALSE)
  if (!file.exists(buildfile)) {
    existing_lines <- ""
  } else {
    existing_lines <- readLines(buildfile, warn = FALSE, encoding = "UTF-8")
  }
  new <- setdiff(lines, existing_lines)
  if (length(new) != 0) {
    all <- c(existing_lines, new)
    cat(all, file = buildfile, sep = "\n")
  }

  # Pull container if needed
  if (isTRUE(update_docker)) {
    system(paste("docker pull", container))
  }

  # Future ----
  ## Allow us to lanch the system command in a new R session
  # library(future)
  # Requires at least 2 workers otherwise does not work
  future::plan(future::multisession(workers = 2))

  # Databases container ----
  if (isTRUE(with_mysql)) {
    if (isTRUE(update_docker)) {
      system(paste("docker pull", mysql_docker))
    }

    path <- normalizePath(file.path(project_path, "database"), mustWork = FALSE)
    if (!dir.exists(path)) {
      dir.create(path)
    }

    system("docker network create r-db")

    future::future({
      system(
        paste0(
          'docker run --net r-db',
          ' --name mysql ',
          ' -v ', normalizePath(file.path(project_path, "database"), mustWork = TRUE), ':/var/lib/mysql',
          ' -e MYSQL_ROOT_PASSWORD=coucou -d ', mysql_docker,
          ' --secure-file-priv=""',
          ' --default-authentication-plugin=mysql_native_password',
          ' && sleep 10',
          ' && docker exec mysql mysql -uroot -pcoucou -e "create database mydb" &')
      )
    })
  }

  # Chromium container ----
  if (FALSE) {
    if (isTRUE(with_chrome)) {
      if (isTRUE(update_docker)) {
        system("docker pull microbox/chromium-headless")
      }

      if (!isTRUE(with_mysql)) {
        # Otherwise already started
        system("docker network create r-db")
      }

      # future::future({
      #   system(
      #     paste0(
      #       'docker run --net r-db',
      #       ' --name chrome -d ', mysql_docker)
      #   )
      # })

      future::future({
        system(
          paste0(
            'docker run',
            ' --security-opt seccomp=', system.file("seccomp/chromium.json", package = "devindocker"),
            ' -v `pwd`/node-ci-demo:/app',
            ' tkp1n/chromium')
        )
      })


    }
  }

  ## Launch ----
  projectname <- basename(project_path)

  if (!is.null(renv_cache)) {
    # {renv} path in container
    RENV_PATHS_CACHE_CONTAINER <- "/opt/local/renv/cache"
    # RENV_PATHS_CACHE_CONTAINER <- "/home/rstudio/.local/share/renv/cache"
    RENV_PATHS_LIBRARY_ROOT_CONTAINER = "/home/rstudio/.renv/library"
    # Directory with all {renv} package cache on host
    RENV_PATHS_CACHE_HOST <- normalizePath(renv_cache, mustWork = FALSE)
    if (!dir.exists(RENV_PATHS_CACHE_HOST)) {
      dir.create(RENV_PATHS_CACHE_HOST)
      message(RENV_PATHS_CACHE_HOST, " was created")
    }
    # Directory with local copy of packages for the project
    if (isTRUE(renv_out)) {
      RENV_PATHS_LIBRARY_ROOT_HOST = renv_out_dir
    }
  }

  # Problem windows with virtualbox
  # if (.Platform$OS.type == "windows" & isTRUE(vbox)) {
  #   project_path <- normalizePath(project_path, winslash = "/", mustWork = FALSE)
  #   if (grepl("^[A-Z]:", project_path)) {
  #   project_path <- gsub("^([A-Z]):", "//\\1", project_path)
  #   }
  # }


  ## Launch the server in the new R session (terminal have to be active...)
  future::future({
    system(
      paste0(
        "docker run --name ", projectname,
        ifelse(isTRUE(with_mysql), " --net r-db", ""),
        " -d -e DISABLE_AUTH=true",
        # {renv}
        # _Global renv cache
        ifelse(!is.null(renv_cache), paste0(" -e RENV_PATHS_CACHE=", RENV_PATHS_CACHE_CONTAINER), ""),
        ifelse(!is.null(renv_cache), paste0(" -v ", RENV_PATHS_CACHE_HOST, ":", RENV_PATHS_CACHE_CONTAINER), ""),
        # _Project renv library
        ifelse(isTRUE(renv_out), paste0(" -e RENV_PATHS_LIBRARY_ROOT=", RENV_PATHS_LIBRARY_ROOT_CONTAINER), ""),
        ifelse(isTRUE(renv_out), paste0(" -v ", RENV_PATHS_LIBRARY_ROOT_HOST, ":", RENV_PATHS_LIBRARY_ROOT_CONTAINER), ""),

        # Chrome
        ifelse(isTRUE(with_chrome), paste0(" -v ", chrome_path, ":/opt/local/chromium"), ""),

        # Rstudio server
        " -v ", project_path, ":/home/rstudio/", projectname,
        " -v ", normalizePath(file.path(project_path, "rstudio_dotconfig"), mustWork = TRUE), ":/home/rstudio/.config", #/rstudio
        " -v ", normalizePath(file.path(project_path, "rstudio_dotrstudio"), mustWork = TRUE), ":/home/rstudio/.rstudio",
        " -p 127.0.0.1:", port, ":8787 ",
        container),
      intern = TRUE)
  })

  if (isTRUE(renv_inst)) {
    if (!file.exists(file.path(project_path, "renv_instructions.Rmd"))) {
      update_renv_help(project_path, overwrite = FALSE)
    }
  }
  Sys.sleep(5)
  browseURL(paste0("http://127.0.0.1:", port))
}

#' Update renv instructions with last version
#'
#' @param project_path Path where to save the renv_instructions.Rmd file
#' @param overwrite Whether to overwrite existing file
#'
#' @export
update_renv_help <- function(project_path = "", overwrite = TRUE) {
  file.copy(
    system.file("renv", "renv_instructions.Rmd", package = "devindocker"),
    file.path(project_path, "renv_instructions.Rmd"),
    overwrite = overwrite
  )
}

#' Stop running Docker container
#'
#' @param sleep Numeric. Number of seconds to wait for user to correctly stop Rstudio Server
#'
#' @export
#' @rdname launch_proj_docker

stop_proj_docker <- function(project_path, with_mysql = FALSE, with_chrome = FALSE, sleep = 10) {

  projectname <- basename(project_path)

  message("
  # --- /!\ Do not forget to stop properly the Rstudio Server /!\ --- #
  # Click on Top right button to quit or `q()` in the console
  ")

  Sys.sleep(sleep)

  ## To stop your image after your work proprely
  if (isTRUE(with_mysql)) {
    system("docker kill mysql")
    system("docker rm mysql")
  }
  if (FALSE) {
    if (isTRUE(with_chrome)) {
      system("docker kill chrome")
      system("docker rm chrome")
    }
  }
  system(paste("docker kill", projectname))
  system(paste("docker rm", projectname))

  if (isTRUE(with_mysql)) {
    system("docker network remove r-db")
  }
  ## If needed INSIDE the Docker Rstudio Server ----
  ### _You will need to set the credentials
  # git config credential.helper store
  # _and your name
  # usethis::use_git_config(scope = "project", user.name = "", user.email = "@thinkr.fr")
  ## Template git
  # git2r::config(global = FALSE, commit.template = "config_git/template_commit")

  ## To install new packages used
  # remotes::install_github("ThinkR-open/attachment")
  # pkgs <- attachment::att_from_description()
  # attachment::install_if_missing(pkgs)
  # renv::restore()

  # If you installed package from github or forced one CRAN
  # you may have to be sure to install dependencies from MRAN
  # ll <- list.files("library")
  # for (l in ll) {
  #   try(remotes::install_version(l))
  # }
}
