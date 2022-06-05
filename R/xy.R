#' Generate data sets to apply regression methods
#'
#' @param n Number of observations
#' @param beta0 beta0, default value: 3,
#' @param beta1 beta1, default value: 0.5
#' @param x_dist A random number generation function. Default is a `rnorm`
#'   with mean 5 and sd 1.
#' @param error_dist A random number generation function. Default is a `rnorm`
#'   with mean 0 and sd 0.5.
#'
#' @examples
#'
#' df <- sim_xy()
#'
#' df
#'
#' plot(df)
#'
#' klassets:::plot.klassets_xy(setNames(cars, c("x", "y")))
#'
#' @export
sim_xy <- function(n = 100,
                   beta0 = 3,
                   beta1 = 0.5,
                   x_dist = purrr::partial(rnorm, mean = 5, sd = 1),
                   error_dist = purrr::partial(rnorm, sd = 0.5)){

  # set.seed(seed)

  x <- sort(x_dist(n))

  e <- error_dist(n)

  y <- beta0 + beta1 * x + e

  df <- tibble::tibble(x, y)

  class(df) <- c( "klassets_xy", class(df))

  df

}

#' Fit Linear model to `klassets_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param order Order of predictive variable x.
#' @param stepwise A logical value to indicate to perform stepwise.
#' @param verbose A logical value to indicate to show the trace of the
#'   stepwise procedure.
#'
#' @examples
#'
#' df <- sim_xy()
#'
#' df
#'
#' dflm <- fit_linear_model(df)
#'
#' dflm
#'
#' plot(dflm)
#'
#' df <- sim_xy(1000)
#' df <- dplyr::mutate(df, y = y + 10 * sin(x) + sqrt(abs(x)))
#'
#' plot(df)
#'
#' plot(fit_linear_model(df))
#'
#' plot(fit_linear_model(df, order = 5, stepwise = TRUE, verbose = TRUE))
#'
#' @importFrom dplyr matches
#' @importFrom stats lm
#' @export
fit_linear_model <- function(df, order = 1, stepwise = FALSE, verbose = FALSE){

  # df <- sim_xy(1000)
  # plot(df)

  # df <- sim_xy(1000)
  # df <- sim_quasianscombe_set_2(df)

  df <- add_power_variables_to_data_frame(df, order = order) |>
    dplyr::select(-dplyr::matches("y_"))

  mod <- stats::lm(y ~ ., data = df)

  if(stepwise) mod <- step(mod, trace = verbose)

  df <- df |>
    dplyr::mutate(prediction = predict(mod, newdata = df)) |>
    dplyr::select(.data$x, .data$y, .data$prediction)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_xy_linear_model", class(df))

  attr(df, "model") <- mod
  attr(df, "order") <- order

  df

}

#' Fit regression tree to `klassets_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param maxdepth Max depth of the tree. Same used in `partykit::ctree_control`.
#' @param alpha Alpha value, same used in `partykit::ctree_control`
#' @param ... Options for `partykit::ctree_control`.
#'
#' @examples
#'
#' df <- sim_xy()
#'
#' df
#'
#' dflm <- fit_regression_tree(df)
#'
#' dflm
#'
#' plot(dflm)
#'
#' df <- sim_xy(1000)
#' df <- dplyr::mutate(df, y = y + 10 * sin(x) + sqrt(abs(x)))
#'
#' plot(df)
#'
#' plot(fit_regression_tree(df, maxdepth = 3))
#'
#' # default
#' plot(fit_regression_tree(df))
#'
#' @export
fit_regression_tree <- function(df, maxdepth = Inf, alpha = 0.05, ...){

  mod <- partykit::ctree(
    y ~ x,
    data = df,
    control = partykit::ctree_control(maxdepth = maxdepth, alpha = alpha, ...)
  )

  df <- dplyr::mutate(df, prediction = partykit::predict.party(mod))

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_xy_regression_tree", class(df))

  attr(df, "model") <- mod

  df

}

#' Fit Linear Model tree to `klassets_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param maxdepth Max depth of the tree. Same used in `partykit::mob_control`.
#' @param alpha Alpha value, same used in `partykit::mob_control`
#' @param ... Addiotional options passed to `partykit::mob_control`.
#'
#' @examples
#'
#' df <- sim_xy()
#'
#' df
#'
#' dflm <- fit_linear_model_tree(df)
#'
#' dflm
#'
#' plot(dflm)
#'
#' df <- sim_xy(1000)
#' df <- dplyr::mutate(df, y = y + 10 * sin(x) + sqrt(abs(x)))
#'
#' plot(df)
#'
#' plot(fit_linear_model_tree(df))
#'
#' @importFrom partykit lmtree
#' @export
fit_linear_model_tree <- function(df, maxdepth = Inf, alpha = 0.05, ...){

  df <- dplyr::mutate(df, x2 = .data$x)

  mod <- partykit::lmtree(
    y ~ x | x2, data = df,
    maxdepth = maxdepth,
    alpha = alpha,
    ...
    )

  df <- dplyr::mutate(df, prediction = predict(mod, newdata = df))

  df <- df |>
    dplyr::select(.data$x, .data$y, .data$prediction)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_xy_linear_model_tree", class(df))

  attr(df, "model") <- mod

  df

}


#' Fit regression random forest to `klassets_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param ntree Number of trees to grow for the forest.
#' @param maxdepth Max depth of the tree. Same used in `partykit::ctree_control`.
#' @param alpha Alpha value, same used in `partykit::ctree_control`
#' @param trace A logical indicating if a progress bar shall be printed while the forest grows.
#' @param ... Options for `partykit::ctree_control`.
#'
#' @examples
#'
#' df <- sim_xy()
#'
#' df
#'
#' dflm <- fit_regression_random_forest(df)
#'
#' dflm
#'
#' plot(dflm)
#'
#' df <- sim_xy(1000)
#' df <- dplyr::mutate(df, y = y + 3 * sin(x) + 5 * sqrt(abs(x)))
#'
#' plot(df)
#'
#' plot(fit_regression_random_forest(df))
#'
#' # default
#' plot(fit_regression_random_forest(df))
#'
#' @importFrom partykit cforest
#' @export
fit_regression_random_forest <- function(df,
                                         ntree = 500L,
                                         maxdepth = Inf,
                                         alpha = 0.05,
                                         trace = TRUE,
                                         ...){

  mod <- partykit::cforest(
    y ~ x,
    data = df,
    ntree = ntree,
    trace = trace,
    control = partykit::ctree_control(maxdepth = maxdepth, alpha = alpha, ...)
  )

  df <- dplyr::mutate(df, prediction = partykit::predict.cforest(mod))

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_xy_regression_random_forest", class(df))

  attr(df, "model") <- mod

  df

}
