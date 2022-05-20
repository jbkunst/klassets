# skeleton and some musts -------------------------------------------------
usethis::create_package("../klasset/")

usethis::use_readme_md()

usethis::use_mit_license()

usethis::use_github_action_check_standard()


# pkgdown -----------------------------------------------------------------
usethis::use_github_action("pkgdown")

usethis::edit_r_buildignore() # add docs folder to Rbuilignore to test

pkgdown::build_site_github_pages()


# developing package ------------------------------------------------------
usethis::use_package("tibble")
usethis::use_package("ggplot2")
usethis::use_package("stringr")


devtools::load_all()
