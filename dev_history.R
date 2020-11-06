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
usethis::use_github_action_check_standard()
usethis::use_github_action("pkgdown")
pkgdown::build_site()
usethis::use_news_md()

# Dev
attachment::att_amend_desc()


