
<!-- README.md is generated from README.Rmd. Please edit that file -->

# devindocker

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

Launch a Docker container with your directory inside. This should be a
container with Rstudio server inside.

``` r
# Which path to your working directory / project
project_path <- file.path(tempdir, "myproject")

# Which container (with Rstudio inside) ? ----
# https://hub.docker.com/r/thinkr/rstudio3_5_2_geo
# https://hub.docker.com/r/rocker/verse
container <- c("thinkr/rstudio3_5_2_geo", 
               "rocker/verse")[1]

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
