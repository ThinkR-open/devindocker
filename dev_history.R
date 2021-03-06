usethis::create_package("devindocker")
usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("_pkgdown.yml")

# License
usethis::use_mit_license("ThinkR")
usethis::use_code_of_conduct()

# Doc
usethis::use_readme_rmd()
usethis::use_vignette("aa-dev-in-docker")
usethis::use_vignette("ab-dev-in-docker-with-renv")
usethis::use_vignette("ac-docker-network")
usethis::use_vignette("ac-docker-volumes")
usethis::use_github_action_check_standard()
usethis::use_github_action("pkgdown")
pkgdown::build_site()
usethis::use_news_md()
usethis::use_package_doc()

# Tests
usethis::use_testthat()
usethis::use_test("launch_proj_docker")

# devindocker inside devindocker
# _Start Docker project
origwd <- setwd(dirname(normalizePath(".")))
container <- "rocker/geospatial:4.0.1"
launch_proj_docker(path = "devindocker",
                   container = container,
                   port = 8080)
# _Stop Docker properly
stop_proj_docker(path = "devindocker", sleep = 2)
setwd(origwd)


# Dev
attachment::att_amend_desc()

options(rmarkdown.html_vignette.check_title = FALSE)
devtools::check()

