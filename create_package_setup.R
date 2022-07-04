# renv
renv::init()

# usethis
path <- file.path(here::here())
usethis::create_package(path)
usethis::use_readme_rmd()
usethis::use_mit_license()

# devtools
devtools::check()
