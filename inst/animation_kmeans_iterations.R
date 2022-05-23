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
set.seed(124)

df <- sim_clusters(n = 1000, groups = 5)

plot(df)

set.seed(124)

kmi <- kmeans_iterations(df, centers = 4)

p <- plot(kmi)

p

gga <- p +
  # remove facet
  facet_null() +
  # then animate
  labs(
    title = "Iteration {closest_state}",
    caption = "Frame {frame} of {nframes}"
  ) +
  ease_aes("cubic-in-out") +
  shadow_wake(wake_length = 0.2, alpha = 0.1) +
  transition_states(iteration, transition_length = 2, state_length = 1)


gganim <- animate(gga)
# gganim <- animate(gga, fps = 30, duration = 10)

gganim

# .Last.value


gganimate::save_animation(
  gganim,
  file = "man/figures/animation_kmeans_iterations.gif"
)

