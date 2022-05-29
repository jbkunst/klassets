# setup -------------------------------------------------------------------
library(klassets)
library(ggplot2)
library(gganimate)

theme_set(theme_minimal(base_size = 10))

if (require(showtext)) {
  sysfonts::font_add_google("IBM Plex Sans", "plex")
  showtext::showtext_auto()
}

# data --------------------------------------------------------------------
set.seed(124)

df <- sim_groups(n = 2000, groups = 6)

plot(df)

set.seed(1247090)

kmi <- kmeans_iterations(df, centers = 4)

p <- plot(kmi)

p

gga <- p +
  # remove facet
  facet_null() +
  # then animate
  labs(
    title = "Iteration {closest_state}"
    # caption = "Frame {frame} of {nframes}"
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(1,"cm")) +
  ease_aes("cubic-in-out") +
  # shadow_wake(wake_length = 0.2, alpha = 0.1) +
  transition_states(iteration, transition_length = 2, state_length = 1)


options(gganimate.dev_args = list(width = 800, height = 600))

gganim <- animate(gga)
# gganim <- animate(gga, fps = 30, duration = 10)

gganim

# .Last.value


gganimate::save_animation(
  gganim,
  file = "man/figures/animation_kmeans_iterations.gif"
)

