
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <span style="color: #2395ec;">dev</span><span style="color: #38424f;">in</span><span style="color: #2395ec;">docker</span> <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R build
status](https://github.com/ThinkR-open/devindocker/workflows/R-CMD-check/badge.svg)](https://github.com/ThinkR-open/devindocker/actions)
<!-- badges: end -->

The goal of {devindocker}, as “Development In Docker”, is to help launch
your R project inside a Docker container with Rstudio server.

  - You can develop with the same architecture as your clients
  - You can test your project / package on a specific architecture
  - You can combine this with {renv} to keep packages versions installed
    inside your directory (and thus available next time you start the
    container)
  - All changes in settings of your Rstudio Server will be kept for next
    start (last project opened, editor theme, …)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ThinkR-open/devindocker")
```

## Usage

Let us create a random directory with a file inside.

``` r
# Temporary project
tempdir <- tempdir()
project_path <- file.path(tempdir, "myproject")
dir.create(project_path)
# Add a file inside
cat("# my R file", file = file.path(project_path, "my-file.R"))
```

### Work in a Docker environment

Launch a Docker container with your directory inside. This should be a
container with Rstudio server inside.  
*Note that you start out of your project, which means you will have to
start a new RStudio project if this is your way of working.*  
*Note that packages you install will not be kept after you stop the
container, but RStudio preferences will.*

``` r
library(devindocker)
# Which path to your working directory / project
project_path <- file.path(tempdir, "myproject")

# Which container (with Rstudio inside) ? ----
# https://hub.docker.com/r/rocker/verse
container <- "rocker/geospatial:4.0.1"

# Which port ? ----
# _Useful if multiple Rstudio Server to launch
port <- 8788

# Start Docker project ----
launch_proj_docker(
  project_path = project_path,
  container = container,
  port = port)
```

When you’re done, do not forget to stop properly the Rstudio Server:
Click on Top right button to quit or `q()` in the console.

Then, end the container.

``` r
# Stop Docker properly
stop_proj_docker(project_path = project_path)
```

### Use {renv} inside Docker and keep installation of packages

**Note that you need to launch your project with {devindocker} from
outside this project. Never ever open it again locally (out of a Docker
container) if you want to avoid problems with bad and not compatible
local {renv} setup. It is recommended to create a project dedicated to
launch {devindocker} projects.**

Launch a Docker container with your directory inside. This should be a
container with RStudio Server inside.  
*Note that you start out of your project, which means you will have to
start a new RStudio project if this is your way of working.*  
*Note that packages you install will be kept after you stop the
container, as well as RStudio preferences.*

**Follow instructions in the `"renv_instructions.Rmd"` file that is
created inside your project.**

``` r
# Which path to your working directory / project
project_path <- file.path(tempdir, "myproject")

# Which container (with Rstudio inside) ? ----
# https://hub.docker.com/r/rocker/verse
container <- "rocker/geospatial:4.0.1"

# Which port ? ----
# _Useful if multiple Rstudio Server to launch
port <- 8788

# My renv cache directory on my local computer
# Used as persistent drive for all you Docker container with {devindocker}
renv_cache <- "~/renv_cache"

# Start Docker project ----
devindocker::launch_proj_docker(
  project_path = project_path,
  container = container,
  port = port,
  renv_cache = renv_cache,
  renv_inst = TRUE, # Add an Rmd with instructions inside your project
  update_docker = TRUE
)
```

When you’re done, do not forget to stop properly the Rstudio Server:
Click on Top right button to quit or `q()` in the console.

Then, end the container.

``` r
# Stop Docker properly
stop_proj_docker(project_path = project_path)
```

*There is an implementation to allow connection with a local Docker
container having a mysql database setup, which is experimental. It may
require an RStudio container with databases system dependencies. See
parameter `with_mysql`.*

Find the last version of the instructions for {renv} use here as
follows:

``` r
file <- system.file("renv/renv_instructions.Rmd", package = "devindocker")
file.edit(file)
```

## Install Docker on your OS

Here is the list of resources written by Docker to install Docker on
your computer depending on your OS:

  - Windows 10 64-bit: Pro, Enterprise, or Education (Build 16299 or
    later) : <https://docs.docker.com/docker-for-windows/install/>
  - Install Windows 10 Home, version 1903 or higher. :
    <https://docs.docker.com/docker-for-windows/install-windows-home/>
  - MacOS : <https://docs.docker.com/docker-for-mac/install/>
  - Linux: <https://docs.docker.com/engine/install/#server>
      - Ubuntu: <https://docs.docker.com/engine/install/ubuntu/>

## Code of Conduct

Please note that the devindocker project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
