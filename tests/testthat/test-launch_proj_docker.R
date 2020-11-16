# Create a package directory with files
tempdir <- tempdir()
my_project <- normalizePath(file.path(tempdir, "myproject"), mustWork = FALSE)
usethis::create_package(my_project, open = FALSE)

# Which container with RStudio server? ----
container <- "rocker/geospatial:4.0.1"
# Which port ? ----
port <- Sys.getenv("PORT", unset = 8787)
message("port used is: ", port)

test_that("launch_proj_docker and stop work", {
  # skip_on_ci()
  # skip_on_cran()

  # Start Docker project
  output <- launch_proj_docker(path = my_project,
                     container = container,
                     port = port,
                     open_url = FALSE)

  # RStudio server has started
  try(get_html <- httr::GET(url = paste0("http://127.0.0.1:", port)))
  try(get_html <- httr::GET(url = paste0("http://192.168.160.2:", port)))
  try(get_html <- httr::GET(url = paste0("http://localhost:", port)))
  try(get_html <- httr::GET(url = paste0("http://", host.docker.internal, ":", port)))


  expect_equal(get_html$status_code, 200)
  # RStudio config files exist
  expect_true(dir.exists(file.path(my_project, "rstudio_dotrstudio")))
  expect_true(dir.exists(file.path(my_project, "rstudio_dotconfig")))

  # Stop Docker properly
  stop_proj_docker(path = my_project, sleep = 5)
  # RStudio server has stopped
  # expect_error(httr::GET(url = paste0("http://127.0.0.1:", port)))
  # RStudio config files still exist
  expect_true(dir.exists(file.path(my_project, "rstudio_dotrstudio")))
  expect_true(dir.exists(file.path(my_project, "rstudio_dotconfig")))
})

# Test volumes ----
my_additional <- normalizePath(file.path(tempdir, "my_additional"), mustWork = FALSE)
dir.create(my_additional)
my_additional2 <- normalizePath(file.path(tempdir, "my_additional2"), mustWork = FALSE)
dir.create(my_additional2)

## Create the table of correspondance for additional volumes
volumes <- data.frame(
  local = c(my_additional, my_additional2),
  docker = c("/home/rstudio/my_additional", "/home/rstudio/my_additional2")
)

test_that("other volumes work", {
  # Start Docker project
  output <- launch_proj_docker(path = my_project,
                               container = container,
                               port = port,
                               volumes = volumes)

  # RStudio server has started
  # get_html <- httr::GET(url = paste0("http://127.0.0.1:", port))
  # expect_equal(get_html$status_code, 200)

  # Add a file in the additional directory from inside the container
  system(
    paste0("docker exec ", devindocker:::clean_project_name(basename(my_project)),
           " Rscript -e 'cat(\" test\", file = \"", volumes[1, "docker"], "/test.txt\")'")
  )
  expect_true(file.exists(file.path(volumes[1,"local"], "test.txt")))

  system(
    paste0("docker exec ", devindocker:::clean_project_name(basename(my_project)),
           " Rscript -e 'cat(\" test\", file = \"", volumes[2, "docker"], "/test2.txt\")'")
  )
  expect_true(file.exists(file.path(volumes[2,"local"], "test2.txt")))

  # Stop Docker properly
  stop_proj_docker(path = my_project, sleep = 5)
  # RStudio server has stopped
  # expect_error(httr::GET(url = paste0("http://127.0.0.1:", port)))
})
unlink(my_project, recursive = TRUE)
unlink(volumes[1,"local"], recursive = TRUE)
unlink(volumes[2,"local"], recursive = TRUE)


# _Not recommended dir name ----
my_bad_project <- normalizePath(file.path(tempdir, "my Project"), mustWork = FALSE)
dir.create(my_bad_project)

test_that("not recommended dir name work", {
  # Start Docker project
  output <- launch_proj_docker(path = my_bad_project,
                               container = container,
                               port = port)

  # RStudio server has started
  # get_html <- httr::GET(url = paste0("http://127.0.0.1:", port))
  # expect_equal(get_html$status_code, 200)

  # Stop Docker properly
  stop_proj_docker(path = my_project, sleep = 5)
  # RStudio server has stopped
  # expect_error(httr::GET(url = paste0("http://127.0.0.1:", port)))
})
unlink(my_bad_project, recursive = TRUE)
