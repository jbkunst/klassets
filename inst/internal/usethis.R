# skeleton and some musts -------------------------------------------------
usethis::create_package("../klasset/")

usethis::use_readme_md()

usethis::use_mit_license()

usethis::use_github_action_check_standard()

usethis::use_code_of_conduct()

# pkgdown -----------------------------------------------------------------
usethis::use_github_action("pkgdown")

usethis::use_readme_rmd()

usethis::use_github_actions_badge()
usethis::use_badge(
  badge_name = "Github stars",
  href = "https://github.com/jbkunst/klassets",
  src = "https://img.shields.io/github/stars/jbkunst/klassets.svg?style=social&label=Github"
)

# add docs folder to Rbuilignore to test
usethis::edit_r_buildignore()
usethis::edit_git_ignore()

{pkgdown::init_site(); pkgdown::build_home(preview = TRUE)}

pkgdown::build_reference_index()
pkgdown::preview_site()

pkgdown::build_articles()
pkgdown::build_reference()

pkgdown::build_site(preview = TRUE)



# some checks -------------------------------------------------------------
usethis::use_spell_check()


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
usethis::use_package("metR")
usethis::use_package("partykit")
usethis::use_package("parttree") # Remotes!!
usethis::use_package("ranger") # Remotes!!


usethis::use_package("tidyr", type = "Suggests")
usethis::use_package("broom", type = "Suggests")
usethis::use_package("showtext", type = "Suggests")
usethis::use_package("transformr", type = "Suggests")
usethis::use_package("gganimate", type = "Suggests")
usethis::use_package("gifski", type = "Suggests")
usethis::use_package("here", type = "Suggests")
usethis::use_package("patchwork", type = "Suggests")



devtools::load_all()
