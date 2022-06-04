# test --------------------------------------------------------------------
df <- sim_response_xy(n = 500, relationship = function(x, y) x**2 > y)

plot(df)

df_reg_log_3 <- fit_logistic_regression(df, order = 3, stepwise = TRUE)

plot(df_reg_log_3)

options(klassets.geom_contour_fill = FALSE)

plot(df_reg_log_3)

options(klassets.geom_contour_fill = TRUE)

plot(df_reg_log_3)


# dev ---------------------------------------------------------------------
p +
  ggplot2::geom_contour_filled(data = dfgrid,
                               ggplot2::aes(.data$x, .data$y, z = .data$prediction))



p +
  metR::geom_contour_fill(data = dfgrid,
                          ggplot2::aes(.data$x, .data$y, z = .data$prediction))  +
  ggplot2::scale_fill_gradient2(
    name = "Model",
    midpoint = 0.5,
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1),
    high = scales::muted("blue"),
    low =  scales::muted("red")
  )



p +
  ggplot2::geom_raster(data = dfgrid,
                       ggplot2::aes(.data$x, .data$y, fill = .data$prediction)) +
  ggplot2::scale_fill_gradient2(
    name = "Model",
    midpoint = 0.5,
    breaks = seq(0, 1, by = 0.25),
    limits = c(0, 1),
    high = scales::muted("blue"),
    low =  scales::muted("red")
  )
