# setup -------------------------------------------------------------------
library(klassets)
library(ggplot2)
library(gganimate)

theme_set(theme_minimal(base_size = 15))

if (require(showtext)) {
  sysfonts::font_add_google("IBM Plex Sans", "plex")
  showtext::showtext_auto()
}

# data --------------------------------------------------------------------
set.seed(1234)

df <- sim_response_xy(
  n = 500,
  x_dist = purrr::partial(runif, min = -1, max = 1),
  relationship = function(x, y) sin(x*pi) > sin(y*pi),
  noise = 0.1
)

df

plot(df)


# logistic regressions ----------------------------------------------------
orders <- 1:4

# data
dflrs <- orders |>
  purrr::map_df(apply_logistic_regression, df = df, .id = "order")

# models
models <- orders |>
  purrr::map(apply_logistic_regression, df = df) |>
  purrr::map(attr, "model")

dfgrid <- klassets:::create_grid_from_data_frame(df)

dfgrids <- orders |>
  purrr::map(klassets:::add_power_variables_to_data_frame, df  = dfgrid)

predictions <- purrr::map2(models, dfgrids, predict, type = "response")

dfgrids <- purrr::map2(dfgrids, predictions, ~ dplyr::mutate(.x, prediction = .y)) |>
  purrr::map(select, x, y, prediction) |>
  purrr::map2_df(orders, ~ dplyr::mutate(.x, order = .y))


# plot --------------------------------------------------------------------
library(ggplot2)

gxy <- ggplot2::ggplot(data = dfgrids) +
  metR::geom_contour_fill(
    ggplot2::aes(.data$x, .data$y, z = .data$prediction),
    bins = 100
  ) +
  metR::geom_text_contour(
    ggplot2::aes(.data$x, .data$y, z = .data$prediction),
    stroke = 0.2
  ) +
  ggplot2::scale_fill_gradient2(
    name = expression("P( |x,y)"),
    midpoint = 0.5,
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1),
    high = scales::muted("blue"),
    low =  scales::muted("red")
  ) +
  ggplot2::geom_point(
    ggplot2::aes(.data$x, .data$y, color = factor(.data$response), shape = factor(.data$response)),
    # data = dflrs,
    data = df,
    size = 2
  ) +
  ggplot2::scale_shape_manual(name = NULL, values = c(4, 1)) +
  ggplot2::scale_color_manual(
    name = NULL,
    values = c(scales::muted("red"), scales::muted("blue"))
  ) +
  ggplot2::facet_wrap(vars(order))

gxy

gxy +
  ggforce::facet_wrap_paginate(facets = vars(order), ncol = 1, nrow = 1, page = 2)

gxy <- ggplot(data = dfgrids, aes(x, y, z = prediction)) +
  geom_contour_filled() +
  scale_fill_brewer(palette = "Spectral") +
  metR::geom_text_contour(stroke = 0.2) +
  facet_wrap(vars(order))

gxy

# animate -----------------------------------------------------------------
gga <- gxy +
  facet_null() +
  # then animate
  labs(
    title = "The importance of visualization and residual analysis",
    subtitle = "Model of order {closest_state}",
    # caption = "Quasi Anscombe sets\n(Same regression coefficients)"
    ) +
  ease_aes("cubic-in-out") +
  enter_appear() +
  exit_disappear() +
  # shadow_wake(wake_length = 0.01, alpha = 0.1) +
  transition_states(order, transition_length = 2, state_length = 1)

options(gganimate.dev_args = list(width = 400, height = 400))

animate(gga, fps = 10, duration = 10)

options(gganimate.dev_args = list(width = 800, height = 600))

gganim <- animate(gga, fps = 60, duration = 10)

# .Last.value

gganimate::save_animation(
  gganim,
  file = "man/figures/animation_logistic_regression.gif"
)




