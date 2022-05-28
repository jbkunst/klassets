#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth  labs
#' @export
plot.klassets_xy <- function(x, ...){

  p <- ggplot2::ggplot() +
    ggproto_point_xy(x)

  p

}

#' @importFrom stringr str_glue
#' @export
plot.klassets_quasianscombe <- function(x, add_lm = TRUE, ...){

  p <- plot.klassets_xy(x)

  if(add_lm){

    mod <- lm(y ~ x, data = x)

    b <- coefficients(mod)

    b0 <- round(b[1], 2)
    b1 <- round(b[2], 2)

    p <- p +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE,
        color = "gray40", formula = y ~ x
        ) +
      ggplot2::labs(title = stringr::str_glue("Model: y = {b0} + {b1} x"))

  }

  p

}

#' @export
plot.klassets_xy_linear_model <- function(x, length_seq = 100, alpha = 0.05, ...){

  # x <- apply_linear_model(sim_xy())

  dfgrid <- create_grid_from_data_frame(df, length_seq = length_seq)

  dfgrid <- add_power_variables_to_data_frame(dfgrid, order = attr(x, "order")) |>
    dplyr::select(-dplyr::matches("y")) |>
    dplyr::distinct()

  predictions <- predict(attr(x, "model"), newdata = dfgrid, se = TRUE)

  q <- qnorm(1 - alpha/2)

  dfgrid <- dfgrid |>
    dplyr::mutate(
      fit = predictions$fit,
      se  = predictions$se,
      low = fit - se * q,
      high = fit + se * q
      )

  ggplot2::ggplot() +

    ggproto_point_xy(x) +

    ggplot2::geom_ribbon(
      data = dfgrid,
      ggplot2::aes(.data$x, ymin = .data$low, ymax = .data$high),
      fill = "gray60",
      color = "transparent",
      alpha = 0.5
    ) +

    ggplot2::geom_line(
      data = dfgrid,
      ggplot2::aes(.data$x, .data$fit),
      color = "darkred",
      size = 1.0
    )

}

#' @export
plot.klassets_xy_regression_tree <- function(x, length_seq = 100, alpha = 0.05, ...){

  # x <- apply_linear_model(sim_xy())

  dfgrid <- tibble::tibble(
    x = create_seq_from_vector(dplyr::pull(x, .data$x), length_seq = length_seq)
    )

  predictions <- predict(attr(x, "model"), newdata = dfgrid)

  q <- qnorm(1 - alpha/2)

  dfgrid <- dfgrid |>
    dplyr::mutate(
      fit = predictions,
      # se  = predictions$se,
      # low = fit - se * q,
      # high = fit + se * q
    )

  ggplot2::ggplot() +

    ggproto_point_xy(x) +

    # ggplot2::geom_ribbon(
    #   data = dfgrid,
    #   ggplot2::aes(.data$x, ymin = .data$low, ymax = .data$high),
    #   fill = "gray60",
    #   color = "transparent",
    #   alpha = 0.5
    # ) +


    ggplot2::geom_line(
      data = dfgrid,
      ggplot2::aes(.data$x, .data$fit),
      color = "darkred",
      size = 1.0
    )

}


#' @export
plot.klassets_response_xy <- function(x, ...){

  p <- ggplot2::ggplot() +
    ggproto_point_response_xy_color_shape(x)

  addorn_ggplot(p)

}

#' @export
plot.klassets_response_xy_logistic_regression <- function(x, length_seq = 100, ...){

  # x <- apply_logistic_regression(sim_response_xy(n = 500), order = 2)
  # length_grid <-  100

  dfgrid <- create_grid_from_data_frame(x, length_seq = length_seq)

  dfgrid <- add_power_variables_to_data_frame(dfgrid, attr(x, "order"))

  predictions <- predict(attr(x, "model"), newdata = dfgrid, type = "response")

  dfgrid <- dplyr::mutate(dfgrid, prediction = predictions)

  p <- ggplot2::ggplot() +

    ggproto_contour_fill(dfgrid) +

    ggproto_text_contour(dfgrid) +

    ggproto_point_response_xy_color_shape(x) +

    ggplot2::scale_fill_gradient2(
      name = "Model", midpoint = 0.5, breaks = seq(0, 1, by = 0.25), limits = c(0, 1),
      high = scales::muted("blue"), low =  scales::muted("red")
    )

  p

  addorn_ggplot(p)

}

#' @importFrom parttree geom_parttree
#' @export
plot.klassets_response_xy_classification_tree <- function(x, length_seq = 100, ...){

  # df <- sim_response_xy(n = 1000, relationship = function(x, y) x**2 > sin(y))
  # plot(df)
  #
  # t <- "prob"
  # t <- "response"
  # t <- "node"
  #
  # x <- apply_tree(df, type = t)

  type  <- attr(x, "type")

  ptree <- partree2(attr(x, "model"))

  ptree <- dplyr::mutate(ptree, type = .data[[type]])

  scale_fill <- switch(type,
    prob = ggplot2::scale_fill_gradient2(
      name = "Model", midpoint = 0.5,
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1),
      high = scales::muted("blue"),
      low =  scales::muted("red")
    ),

    node = ggplot2::scale_fill_viridis_d("Node"),

    response = ggplot2::scale_fill_manual(
      name = "Model",
      values = c(scales::muted("blue"), scales::muted("red"))
    )
  )

  p <- ggplot2::ggplot() +

    ggplot2::geom_rect(
      data = ptree,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax,
        fill = .data$type
      ),
      alpha = 0.2,
      color = "gray70",
      size = 0.1
    ) +

    ggproto_point_response_xy_color_shape(x) +

    scale_fill

  if(type == "prob"){

    dfgrid <- create_grid_from_data_frame(x, length_seq = length_seq)

    predictions <- predict(attr(x, "model"), newdata = dfgrid, type = "prob")[, 2]

    dfgrid <- dplyr::mutate(dfgrid, prediction = predictions)

    p <- p  +
      ggproto_text_contour(dfgrid, check_overlap = TRUE)

  }

  addorn_ggplot(p)

}

#' @export
plot.klassets_response_xy_knn <- function(x, length_seq = 100, ...){

  # x <- apply_knn(sim_response_xy(n = 1000), neighbours = 100)
  # length_seq <-  200

  dfgrid <- create_grid_from_data_frame(x, length_seq = length_seq)

  preds <- class::knn(
    train = dplyr::select(x, .data$x, .data$y) |> as.matrix(),
    test  = dplyr::select(dfgrid, .data$x, .data$y) |> as.matrix(),
    cl    = dplyr::select(x, .data$response)  |> as.matrix(),
    k     = attr(x, "neighbours"),
    prob  = TRUE
  )

  if(attr(x, "type") == "prob"){
    predictions <- attr(preds, "prob")
  } else {
    predictions <- as.logical(preds)
  }

  if(attr(x, "type") == "prob") {

    predictions <- scales::rescale(as.vector(predictions), to = c(0.01, .99))

    dfgrid <- mutate(dfgrid, prediction = scales::rescale(as.vector(predictions), to = c(0, 1)))

    # return(plot_pred_field(x, dfgrid))

  }

  p <- ggplot2::ggplot() +

    ggproto_contour_fill(dfgrid) +

    ggproto_point_response_xy_color_shape(x) +

    ggplot2::scale_fill_gradient2(
      name = "Model", midpoint = 0.5, breaks = seq(0, 1, by = 0.25), limits = c(0, 1),
      high = scales::muted("blue"), low =  scales::muted("red")
    )

  addorn_ggplot(p)



}


#' @export
plot.klassets_cluster <- function(x, ...){

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))

  if(all(c("group", "cluster") %in% names(x))){

    p <- p +
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

    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(shape = .data$group),
        color = "gray60",
        fill = "gray80",
        ...
      ) +
      labs(shape = "(Original) Group")

  } else if ("cluster" %in% names(x)) {

    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$cluster),
        shape = 21,
        ...
      ) +
      labs(color = "(Assigned) Cluster")

  } else {

    p <- p +
      ggplot2::geom_point(shape = 21, color = "gray60", fill = "gray80", ...)

  }

  p

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

ggproto_point_xy <- function(x){

  ggplot2::geom_point(
    data = x,
    ggplot2::aes(.data$x, .data$y,),
    shape = 21,
    color = "gray60",
    fill = "gray80"
  )

}

ggproto_point_response_xy_color_shape <- function(x){

  ggplot2::geom_point(
    data = x,
    ggplot2::aes(
      .data$x, .data$y,
      color = .data$response, shape = .data$response
      ),
    size = 2
    )

}

ggproto_contour_fill <- function(dfgrid, bins = 100, ...) {

  # ggplot2::geom_contour_filled(
  metR::geom_contour_fill(
    data = dfgrid,
    ggplot2::aes(.data$x, .data$y, z = .data$prediction),
    bins = bins,
    ...
    )

}

ggproto_text_contour <- function(dfgrid, stroke = 0.2, ...){

  metR::geom_text_contour(
    data = dfgrid,
    ggplot2::aes(.data$x, .data$y, z = .data$prediction),
    stroke = stroke,
    ...
  )

}

addorn_ggplot <- function(p){

  p +
    ggplot2::scale_shape_manual(
      name = NULL, values = c(4, 1)
      ) +
    ggplot2::scale_color_manual(
      name = NULL, values = c(scales::muted("red"), scales::muted("blue"))
      )

}

partree2 <- function(tree) {

  # tree <- attr(x, "model")
  tree_distinct <- tibble(
    response = partykit::predict.party(tree, type = "response"),
    node     = partykit::predict.party(tree, type = "node"),
    prob     = partykit::predict.party(tree, type = "prob")[, 2]
  ) |>
    dplyr::distinct(.data$response, .data$node, .data$prob)

  ptree <- dplyr::left_join(
    tree_distinct,
    parttree::parttree(tree),
    by = c("response", "node")
  )

  ptree <- dplyr::mutate(ptree, node = factor(.data$node))

  ptree

}
