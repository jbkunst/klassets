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
