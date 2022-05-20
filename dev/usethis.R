usethis::create_package("../klasset/")

usethis::use_readme_md()

usethis::use_mit_license()

usethis::use_github_action_check_standard()

usethis::use_github_action("pkgdown")


usethis::use_package("tibble")
usethis::use_package("ggplot2")
usethis::use_package("stringr")


devtools::load_all()
