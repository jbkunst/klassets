# setup -------------------------------------------------------------------
library(klassets)
library(ggplot2)
library(gganimate)
library(stringr)

theme_set(theme_minimal(base_size = 15))

if (require(showtext)) {
  sysfonts::font_add_google("IBM Plex Sans", "plex")
  showtext::showtext_auto()
}


# set1 --------------------------------------------------------------------
df <- sim_quasianscombe_set_1(n = 500, x_sd = 5)

plot(df) +
  # xlim(0, NA) +
  # ylim(0, NA) +
  labs(subtitle = "Original data set")

# set2 --------------------------------------------------------------------
func <- function(x) { 2 * x ^ 2}

df2_1 <- sim_quasianscombe_set_2(df, residual_factor = 0, fun = func)

funktion <- function(x) { 2 * sin(x * diff(range(x)))}

df2_2 <- sim_quasianscombe_set_2(df, fun = funktion, residual_factor = 1.25)

plot(df2_2)

# set3 --------------------------------------------------------------------
df3_1 <- sim_quasianscombe_set_3(df, prop = 0.10)

df3_2 <- sim_quasianscombe_set_3(df, prop = 0.15, residual_factor = 0)

# set4 --------------------------------------------------------------------
df4_1 <- sim_quasianscombe_set_4(df, prop = 0.25)

df4_2 <- sim_quasianscombe_set_4(df, rescale_to = c(0, .1), prop = 0.5)

# set5 --------------------------------------------------------------------
df5_1 <- sim_quasianscombe_set_5(df, residual_factor = 2)

df5_2 <- sim_quasianscombe_set_5(df, fun = function(x) rev(x ** 2))

# set6 --------------------------------------------------------------------
df6_1 <- sim_quasianscombe_set_6(df, residual_factor = 1)

df6_2 <- sim_quasianscombe_set_6(df, groups = 4, b1_factor = 0, residual_factor = 0.1)

plot(df6_2)

# combine -----------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(purrr)
library(broom)

dfs <- list(
  "Original" = df,
  "Set 2 v1" = df2_1,
  "Set 2 v2" = df2_2,
  "Set 3 v1" = df3_1,
  "Set 3 v2" = df3_2,
  "Set 4 v1" = df4_1,
  "Set 4 v2" = df4_2,
  "Set 5 v1" = df5_1,
  "Set 5 v2" = df5_2,
  "Set 6 v1" = df6_1,
  "Set 6 v2" = df6_2
)

dfs <- dfs |>
  tibble::enframe(name = "set") |>
  tidyr::unnest(cols = c(value))



# plot --------------------------------------------------------------------
dfs <- dfs |>
  mutate(
    set2 = set,
    set2 = str_replace_all(set2, "Set 2", "No linear relationship"),
    set2 = str_replace_all(set2, "Set 3", "Extreme values (outliers)"),
    set2 = str_replace_all(set2, "Set 4", "Cluster"),
    set2 = str_replace_all(set2, "Set 5", "Heteroskedasticity"),
    set2 = str_replace_all(set2, "Set 6", "Simpson's Paradox"),
    # set2 = str_remove_all(set2, "v[0-9]"),
    set2 = str_trim(set2),
    set2 = forcats::fct_inorder(set2)
  )

gxy <- ggplot(dfs, aes(x, y)) +
  geom_point(shape = 21, fill = "gray80", color = "gray60") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  facet_wrap(vars(set2))

gxy


# animate -----------------------------------------------------------------
gga <- gxy +
  facet_null() +
  # then animate
  labs(
    title = "The importance of visualization and residual analysis",
    subtitle = "{closest_state}",
    caption = "Quasi Anscombe sets\n(Same regression coefficients)"
    ) +
  ease_aes("cubic-in-out") +
  # shadow_wake(wake_length = 0.01, alpha = 0.1) +
  transition_states(set2, transition_length = 2, state_length = 1)

options(gganimate.dev_args = list(width = 800, height = 600))

gganim <- animate(gga, fps = 60, duration = 10)

# .Last.value

gganimate::save_animation(
  gganim,
  file = "man/figures/animation_quasi_anscombre.gif"
  )





