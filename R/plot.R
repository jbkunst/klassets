#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth  labs
#  @importFrom ggplot2 xlim ylim
#' @importFrom stringr str_glue
#' @export
plot.klassets_quasianscombe <- function(x, add_lm = TRUE, ...){

  pxy <- ggplot2::ggplot(x, ggplot2::aes_string("x", "y"))  +
    ggplot2::geom_point(
      shape = 21,
      color = "gray60",
      fill = "gray80"
    )

  if(add_lm){

    mod <- lm(y ~ x, data = x)

    b <- coefficients(mod)

    b0 <- round(b[1], 2)
    b1 <- round(b[2], 2)

    pxy <- pxy +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE,
        color = "gray40", formula = y ~ x
        ) +
      ggplot2::labs(title = stringr::str_glue("Model: y = {b0} + {b1} x"))

  }

  pxy


}

#' @export
plot.klassets_cluster <- function(x, ...){

  pxy <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))

  if(all(c("group", "cluster") %in% names(x))){

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(
          shape = .data$group,
          color = .data$cluster
          ),
        ...
        ) +
      labs(
        shape = "(Original) Group",
        color = "(Assigned) Cluster"
      )

  } else if ("group" %in% names(x)) {

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(shape = .data$group),
        color = "gray60",
        fill = "gray80",
        ...
      ) +
      labs(shape = "(Original) Group")

  } else if ("cluster" %in% names(x)) {

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$cluster),
        shape = 21,
        ...
        ) +
      labs(color = "(Assigned) Cluster")

  } else {

    pxy <- pxy +
      ggplot2::geom_point(shape = 21, color = "gray60", fill = "gray80", ...)

  }

  pxy

}

#' @export
plot.klassets_kmiterations <- function(x, ...){

  dpoints <- x$points

  dcenters <- x$centers

  k <- dcenters |>
    dplyr::count(.data$cluster) |>
    nrow()

  # ggplot(dcenters, aes(cx, cy)) +
  #   geom_point() +
  #   geom_path(aes(group = cluster))

  colors <- viridisLite::viridis(k, begin = 0.1, end = .9)
  colors <- purrr::set_names(colors, LETTERS[seq_len(k)])

  # colors <- c("Start" = "gray70", colors)

  # scales::show_col(colors)

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dpoints,
      ggplot2::aes(.data$x, .data$y, group = .data$id, color = .data$cluster, shape = .data$group),
      size = 3,
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = dcenters,
      ggplot2::aes(.data$cx, .data$cy, group = .data$cluster, fill = .data$cluster),
      size = 6,
      alpha = 1,
      shape = 21,
    ) +
    ggplot2::labs(shape = "Original\nGroup") +
    ggplot2::scale_color_manual(values = colors, name = "Assigned\nCluster", na.value = "gray70") +
    ggplot2::scale_fill_manual(values = colors, name = "Assigned\nCluster", na.value = "gray70") +
    ggplot2::facet_wrap(dplyr::vars(.data$iteration)) +
    labs()

  p

}

#' @export
plot.klassets_response_xy <- function(x, ...){

  p <- ggplot2::ggplot(x, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(
      ggplot2::aes(color = factor(.data$response), shape = factor(.data$response)),
      size = 2
    ) +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = c(4, 1)
      ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(scales::muted("red"), scales::muted("blue"))
      ) +
    ggplot2::theme(legend.key.width = ggplot2::unit(2, "cm"))

  p

}

#' @export
plot.klassets_response_xy_logistic_regression <- function(x, length_grid = 100, ...){

  # x <- apply_logistic_regression(sim_response_xy(n = 500), order = 2)
  # length_grid <-  100

  # p <- klassets:::plot.klassets_response_xy(x)

  xseq <- pretty(pull(x, .data$x))
  yseq <- pretty(pull(x, .data$y))

  dfgrid <- tidyr::crossing(
    x = seq(min(xseq), max(xseq), length.out = length_grid),
    y = seq(min(xseq), max(xseq), length.out = length_grid)
    )

  dfgrid <- add_power_varaibles_to_data_frame(dfgrid, attr(x, "order"))

  predictions <- predict(attr(x, "model"), newdata = dfgrid, type = "response")

  dfgrid <- dfgrid |>
    dplyr::mutate(prediction = predictions)

  ggplot2::ggplot(data = dfgrid) +
    metR::geom_contour_fill(
    # ggplot2::geom_contour_filled(
      ggplot2::aes(.data$x, .data$y, z = .data$prediction),
      bins = 100
      ) +
    metR::geom_text_contour(
      ggplot2::aes(.data$x, .data$y, z = .data$prediction),
      stroke = 0.2
    ) +
    # metR::scale_fill_divergent(
    #   name = expression("P(·|x,y)"),
    #   midpoint = 0.5,
    #   breaks = seq(0, 1, by = 0.25),
    #   limits = c(0, 1)
    # ) +
    ggplot2::scale_fill_gradient2(
      name = expression("P(·|x,y)"),
      midpoint = 0.5,
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1),
      high = scales::muted("blue"),
      low =  scales::muted("red")
    ) +

    ggplot2::geom_point(
      ggplot2::aes(.data$x, .data$y, color = factor(.data$response), shape = factor(.data$response)),
      data = x,
      size = 2
    ) +
    ggplot2::scale_shape_manual(name = NULL, values = c(4, 1)) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c(scales::muted("red"), scales::muted("blue"))
    )



}
