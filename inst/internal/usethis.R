# skeleton and some musts -------------------------------------------------
usethis::create_package("../klasset/")

usethis::use_readme_md()

usethis::use_mit_license()

usethis::use_github_action_check_standard()


# pkgdown -----------------------------------------------------------------
usethis::use_github_action("pkgdown")

usethis::use_readme_rmd()

usethis::use_github_actions_badge()

# add docs folder to Rbuilignore to test
usethis::edit_r_buildignore()
usethis::edit_git_ignore()

pkgdown::init_site()
pkgdown::build_home(preview = TRUE)

pkgdown::build_reference_index()
pkgdown::preview_site()

pkgdown::build_articles()
pkgdown::build_reference()

pkgdown::build_site(preview = TRUE)


# developing package ------------------------------------------------------
usethis::use_package("tibble")
usethis::use_package("ggplot2")
usethis::use_package("stringr")
usethis::use_package("dplyr")
usethis::use_package("rlang")
usethis::use_package("scales")
usethis::use_package("purrr")
usethis::use_package("MASS")
usethis::use_package("forcats")
usethis::use_package("viridisLite")

usethis::use_package("tidyr", type = "Suggests")
usethis::use_package("broom", type = "Suggests")
usethis::use_package("showtext", type = "Suggests")
usethis::use_package("transformr", type = "Suggests")
usethis::use_package("gganimate", type = "Suggests")
usethis::use_package("gifski", type = "Suggests")



devtools::load_all()
