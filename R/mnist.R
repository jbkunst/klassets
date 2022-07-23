# https://stackoverflow.com/a/29384538/829971
# getUsefulPredictors <- function(x) {
#   varid <- partykit::nodeapply(
#     x,
#     ids = partykit::nodeids(x),
#     FUN = function(n)
#       partykit::split_node(n)$varid
#   )
#   varid <- unique(unlist(varid))
#   names(partykit::data_party(x))[varid]
# }

#' Plot some digits from train mnist data
#'
#' @param ids Rows to show.
#'
#' @examples
#'
#' mnist_plot_digits(1)
#'
#' mnist_plot_digits(c(10, 20, 40, 22))
#'
#' set.seed(123)
#'
#' mnist_plot_digits(sample(seq(60000), size = 16))
#'
#' @export
mnist_plot_digits <- function(ids = NULL){

  # ids <- sample(1:60000, size = sample(1:20, size = 1))

  stopifnot(
    !is.null(ids),
    is.numeric(ids),
    all(dplyr::between(ids, 0, 60000)),
    all(ids == as.integer(ids))
    )

  data_id_label <- klassets::mnist_train |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::filter(dplyr::row_number() %in% ids) |>
    tidyr::pivot_longer(cols = c(-.data$id, -.data$label)) |>
    dplyr::mutate(name = stringr::str_remove(.data$name, "pixel_")) |>
    tidyr::separate(.data$name, c("x", "y"), sep = "x") |>
    dplyr::mutate(
      x = as.numeric(.data$x),
      y = as.numeric(.data$y),
      label = stringr::str_glue("Row {scales::comma(id)} - Label {label}")
    )

  ggplot2::ggplot(data_id_label, ggplot2::aes(.data$y, -.data$x)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$label)) +
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::theme_void() +
    ggplot2::coord_equal()

}

# mnist_plot_fit_importance <- function(fit_function = ctree, ...){
#
#   # d <- ranger::importance(object) |>
#   #   enframe() |>
#   #   arrange(desc(value))
#
#   fit <- glmnet::glmnet(
#     x = as.matrix(klassets::mnist_train[, -1]),
#     y = klassets::mnist_train[[1]],
#     family = "multinomial"
#   )
#
#   fit
#
#   dimp <- d |>
#     dplyr::mutate(name = stringr::str_remove(.data$name, "pixel_")) |>
#     tidyr::separate(.data$name, c("x", "y"), sep = "x") |>
#     dplyr::mutate(
#       x = as.numeric(.data$x),
#       y = as.numeric(.data$y)
#       )
#
#   ggplot2::ggplot(dimp, ggplot2::aes(.data$y, -.data$x)) +
#     ggplot2::geom_tile(ggplot2::aes(fill = .data$value))
#
# }
#
# mnist_fit_classification_tree <- function(maxdepth = 5, ...){
#
#   # ... <- NULL
#
#   model <- partykit::ctree(
#     label ~ .,
#     data = klassets::mnist_train,
#     control = partykit::ctree_control(maxdepth = maxdepth, ...)
#   )
#
#   getUsefulPredictors(model)
#
#   plot(model, gp = grid::gpar(fontsize = 4))
#
#   model
#
# }
#
# mnist_fit_random_forest <- function(max.depth = 5, ...){
#
#   # ... <- NULL
#
#   model <- ranger::ranger(
#     label ~ .,
#     data = klassets::mnist_train,
#     max.depth = max.depth,
#     importance = "impurity",
#     ...
#   )
#
# }
#
# mnist_fit_gmb <- function(...){}
#
# mnist_fit_neural_network <- function(...){}
#
# mnist_fit_knn <- function(...){}
