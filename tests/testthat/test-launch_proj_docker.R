# Create a package directory with files
tempdir <- tempdir()
my_project <- normalizePath(file.path(tempdir, "myproject"), mustWork = FALSE)
usethis::create_package(my_project, open = FALSE)

# Which container with RStudio server? ----
container <- "rocker/geospatial:4.0.1"
# Which port ? ----
port <- 8788



test_that("launch_proj_docker and stop work", {
  # skip_on_ci()
  # skip_on_cran()

  # Start Docker project
  output <- launch_proj_docker(project_path = my_project,
                     container = container,
                     port = port)

  # RStudio server has started
  get_html <- httr::GET(paste0(url = "http://127.0.0.1:", port))
  expect_equal(get_html$status_code, 200)
  # RStudio config files exist
  expect_true(dir.exists(file.path(my_project, "rstudio_dotrstudio")))
  expect_true(dir.exists(file.path(my_project, "rstudio_dotconfig")))

  # Stop Docker properly
  stop_proj_docker(project_path = my_project, sleep = 5)
  # RStudio server has stopped
  expect_error(httr::GET(paste0(url = "http://127.0.0.1:", port)))
  # RStudio config files still exist
  expect_true(dir.exists(file.path(my_project, "rstudio_dotrstudio")))
  expect_true(dir.exists(file.path(my_project, "rstudio_dotconfig")))
})
