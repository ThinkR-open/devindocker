#' Launch and stop Docker
#'
#' @param path Path to project or any directory to launch.
#' @param container Docker container to download from docker hub.
#' @param network_name Character. Give the name of the network in which the
#'  container will be included using \code{--net network_name}.
#' @param port Local port to which to launch Rstudio Server
#' @param renv_inst Logical. Whether to add a R script with {renv} instructions in the project.
#' @param renv_cache Path to renv cache on your computer. Set to FALSE to not use renv.
#' @param renv_out Whether to set {renv} libraries out of the project.
#' @param renv_out_dir Where to store project libraries. Default to a ".renv" folder
#' in the parent directory of "path"
#' @param update_docker Logical. Whether to update Docker container with DockerHub.
#' @param is_root logical. Whether the Docker user has root permissions (add to sudoers).
#' Can be useful if you want to simulate your CI behaviour in the Terminal using "\code{sudo R}".
#' @param volumes data.frame with two columns named `local` and `docker`.
#'  `local` contains path to local computer.
#'  `docker` contains path inside the docker container linked to local.
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
#' path <- file.path(tempdir, "myproject")
#' usethis::create_package(path, open = FALSE)
#'
#' # Which Rstudio container ? ----
#' container <- c("thinkr/rstudio3_5_2",
#'                "rocker/geospatial:4.0.1")[2]
#'
#' # Which port ? ----
#' # _Useful if multiple Rstudio Server to launch
#' port <- 8788
#'
#' # _Start Docker project
#' launch_proj_docker(path = path,
#'                    container = container,
#'                    port = port)
#'
#' # _Stop Docker properly
#' stop_proj_docker(path = path, sleep = 5)
#'
#' # With renv cache shared with host
#' dir.create(file.path(tempdir, "cache"))
#' tempcache <- file.path(tempdir, "cache")
#' # _Start Docker project
#' launch_proj_docker(path = path,
#'                    container = container,
#'                    port = port,
#'                    renv_cache = tempcache)
#'
#' # _Stop Docker properly
#' stop_proj_docker(path = path, sleep = 5)
#'
#' # Mount with additional volumes
#' }

launch_proj_docker <- function(path = ".",
                               container = "thinkr/rstudio3_6_1_geo",
                               network_name = "r-db",
                               port = 8787,
                               renv_inst = FALSE,
                               renv_cache = FALSE,
                               renv_out = FALSE,
                               renv_out_dir,
                               update_docker = TRUE,
                               is_root = FALSE,
                               volumes
                               # vbox = FALSE
) {
  # @param vbox Logical. If Docker run on windows in a virtual box, paths need to be changed

  path <- normalizePath(path)

  if (missing(renv_cache) | renv_cache == FALSE) {
    renv_cache <- NULL
  }

  # addtional volumes
  if (!missing(volumes)) {
    if (!all(c("local", "docker") %in% names(volumes))) {
      stop("To mount external volumes, use a data.frame with names: 'local' and 'docker'")
    }
    volumes[,"local"] <- normalizePath(volumes[,"local"])

    add_volumes <- paste(
      apply(volumes, 1, function(x) paste0(" -v '", x["local"], ":", x["docker"], "'")),
      collapse = ""
    )
  } else {
    add_volumes <- NULL
  }

  # First time ----
  if (!dir.exists(file.path(path, "rstudio_dotrstudio"))) {
    # Hide this file
    # rstudio last files and others
    dir.create(normalizePath(file.path(path, "rstudio_dotrstudio"), mustWork = FALSE))
  }
  if (!dir.exists(file.path(path, "rstudio_dotconfig"))) {
    # Hide this file
    # rstudio appearance preferences
    dir.create(normalizePath(file.path(path, "rstudio_dotconfig"), mustWork = FALSE))
  }
  if (isTRUE(renv_out)) {
    if (missing(renv_out_dir)) {
      renv_out_dir <- normalizePath(file.path(dirname(path), ".renv"), mustWork = FALSE)
    }
    if (!dir.exists(renv_out_dir)) {dir.create(renv_out_dir)}
  }

  # .gitignore
  # Files to ignore
  lines <- stats::na.omit(
    c("rstudio_dotrstudio", "rstudio_dotconfig",
      ifelse(renv_inst, "renv_instructions.Rmd", NA)
    )
  )

  gitfile <- normalizePath(file.path(path, ".gitignore"), mustWork = FALSE)
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
  lines <- stats::na.omit(
    c("rstudio\\_dotconfig",
      "rstudio\\_dotrstudio",
      ifelse(renv_inst, "renv\\_instructions\\.Rmd", NA)
    ))

  buildfile <- normalizePath(file.path(path, ".Rbuildignore"), mustWork = FALSE)
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

  ## Launch ----
  dockername <- clean_project_name(basename(path))
  projectname <- clean_dir_name(basename(path))

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
  #   path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  #   if (grepl("^[A-Z]:", path)) {
  #   path <- gsub("^([A-Z]):", "//\\1", path)
  #   }
  # }

  ## Create docker network
  try(system(paste("docker network create", network_name)))

  ## Launch the server in the new R session (terminal have to be active...)
  future::future({
    system(
      paste0(
        "docker run --name ", dockername,
        # network
        # ifelse(isTRUE(with_mysql), " --net r-db", ""),
        " --net ", network_name,
        # root permission
        ifelse(isTRUE(is_root), " -e ROOT=TRUE", ""),
        " -d -e DISABLE_AUTH=true",
        # {renv}
        # _Global renv cache
        ifelse(!is.null(renv_cache), paste0(" -e RENV_PATHS_CACHE=", RENV_PATHS_CACHE_CONTAINER), ""),
        ifelse(!is.null(renv_cache), paste0(" -v ", RENV_PATHS_CACHE_HOST, ":", RENV_PATHS_CACHE_CONTAINER), ""),
        # _Project renv library
        ifelse(isTRUE(renv_out), paste0(" -e RENV_PATHS_LIBRARY_ROOT=", RENV_PATHS_LIBRARY_ROOT_CONTAINER), ""),
        ifelse(isTRUE(renv_out), paste0(" -v ", RENV_PATHS_LIBRARY_ROOT_HOST, ":", RENV_PATHS_LIBRARY_ROOT_CONTAINER), ""),

        # Rstudio server
        " -v '", path, ":/home/rstudio/", projectname, "'",
        " -v '", normalizePath(file.path(path, "rstudio_dotconfig"), mustWork = TRUE), ":/home/rstudio/.config'", #/rstudio
        " -v '", normalizePath(file.path(path, "rstudio_dotrstudio"), mustWork = TRUE), ":/home/rstudio/.rstudio'",

        # Addtional volumes
        ifelse(!is.null(add_volumes), add_volumes, ""),

        " -p 127.0.0.1:", port, ":8787 ",
        container),
      intern = TRUE)
  })

  if (isTRUE(renv_inst)) {
    if (!file.exists(file.path(path, "renv_instructions.Rmd"))) {
      update_renv_help(path, overwrite = FALSE)
    }
  }
  Sys.sleep(5)
  browseURL(paste0("http://127.0.0.1:", port))
}

#' Update renv instructions with last version
#'
#' @param path Path where to save the renv_instructions.Rmd file
#' @param overwrite Whether to overwrite existing file
#'
#' @export
update_renv_help <- function(path = "", overwrite = TRUE) {
  file.copy(
    system.file("renv", "renv_instructions.Rmd", package = "devindocker"),
    file.path(path, "renv_instructions.Rmd"),
    overwrite = overwrite
  )
}

#' Stop running Docker container
#'
#' @param sleep Numeric. Number of seconds to wait for user to correctly stop Rstudio Server
#' @param stop_network Logical. Whether to stop Docker network.
#'
#' @export
#' @rdname launch_proj_docker

stop_proj_docker <- function(path, sleep = 10, network_name = "r-db", stop_network = TRUE) {

  dockername <- clean_project_name(basename(path))

  message("
  # --- /!\ Do not forget to stop properly the Rstudio Server /!\ --- #
  # Click on Top right button to quit or `q()` in the console
  ")

  Sys.sleep(sleep)

  system(paste("docker kill", dockername))
  system(paste("docker rm", dockername))

  if (isTRUE(stop_network)) {
    try(system(paste("docker network remove", network_name)))
  }
}


clean_project_name <- function(x) {
  tolower(gsub(" ", "", x))
}
clean_dir_name <- function(x) {
  gsub(" ", "\\", x)
}
