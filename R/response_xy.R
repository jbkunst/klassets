#' Generate data sets to apply binary classifiers
#'
#' @param n An intenger
#' @param x_dist A random number generation function.
#' @param y_dist A random number generation function.
#' @param relationship A function specify the relationship between x, y and
#'   the response. A function f(x, y) need return a logical value.
#' @param noise A number between 0 and 1.
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(n = 500)
#'
#' df
#'
#' plot(df)
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rbeta
#' @importFrom purrr map pmap map2_df
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
sim_response_xy <- function(n = 500,
                            # x_dist = purrr::partial(rnorm, mean = 0, sd = 1),
                            x_dist = purrr::partial(runif, min = -1, max = 1),
                            y_dist = x_dist,
                            relationship = function(x, y) x > y,
                            noise = 0.2){

  df <- tibble::tibble(
    x = x_dist(n),
    y = y_dist(n)
  )

  df <- dplyr::mutate(
    df,
    response = relationship(.data$x, .data$y),
    response = ifelse(runif(n) < noise, !.data$response, .data$response),
    response = factor(.data$response, levels = c("FALSE", "TRUE")),
    .before = 1
    )

  class(df) <- c("klassets_response_xy", class(df))

  df

}

#' Fit Logistic regression to `klassets_response_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param order Order of values of x and y.
#' @param stepwise A logical value to indicate to perform stepwise.
#' @param verbose A logical value to indicate to show the trace of the
#'   stepwise procedure.
#'
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(n = 500, relationship = function(x, y) x**2 > y)
#'
#' plot(df)
#'
#' df_reg_log <- fit_logistic_regression(df)
#'
#' plot(df_reg_log)
#'
#' df_reg_log_3 <- fit_logistic_regression(df, order = 3, stepwise = TRUE)
#'
#' plot(df_reg_log_3)
#'
#' @importFrom stats binomial glm predict step
#' @export
fit_logistic_regression <- function(df,
                                    order = 1,
                                    stepwise = FALSE,
                                    verbose = FALSE){
  # df <- sim_response_xy(n = 500)
  # order <- 3
  # stepwise <- TRUE
  # verbose <- TRUE

  df <- add_power_variables_to_data_frame(df, order = order)

  mod <- glm(response ~ .,  family = binomial, data = df)

  if(stepwise) mod <- step(mod, trace = verbose)

  df <- df |>
    dplyr::mutate(prediction = predict(mod, newdata = df, type = "response")) |>
    dplyr::select(.data$response, .data$x, .data$y, .data$prediction)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_response_xy")
  class(df) <- c("klassets_response_xy_logistic_regression", class(df))

  attr(df, "model") <- mod
  attr(df, "order") <- order

  df

}

#' Fit classification tree to `klassets_response_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param type Type of prediction, one of prob, response, node.
#' @param maxdepth Max depth of the tree. Same used in `partykit::ctree_control`.
#' @param alpha Alpha value, same used in `partykit::ctree_control`
#' @param ... Options for `partykit::ctree_control`.
#'
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(n = 1000, relationship = function(x, y) x**2 > sin(y))
#'
#' plot(df)
#'
#' # default type = "prob"
#' df_tree_prob <- fit_classification_tree(df)
#' df_tree_prob
#'
#' df_tree_resp <- fit_classification_tree(df, type = "response")
#' df_tree_resp
#'
#' df_tree_node <- fit_classification_tree(df, type = "node")
#' df_tree_node
#'
#' plot(df_tree_prob)
#' plot(df_tree_resp)
#' plot(df_tree_node)
#'
#' @importFrom partykit ctree
#' @export
fit_classification_tree <- function(df,
                                    type = "prob",
                                    maxdepth = Inf,
                                    alpha = 0.05,
                                    ...){

  mod <- partykit::ctree(
    response ~ .,
    data = df,
    control = partykit::ctree_control(maxdepth = maxdepth, alpha = alpha, ...)
    )

  # type <- "node"
  # type <- "response"
  # type <- "prob"

  predictions <- partykit::predict.party(mod, type = type)

  if(type == "prob"){
    predictions <- predictions[, 1]
  }

  df <- dplyr::mutate(df, prediction = predictions)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_response_xy")
  class(df) <- c("klassets_response_xy_classification_tree", class(df))

  attr(df, "type")  <- type
  attr(df, "model") <- mod

  df

}

#' Fit classification random forest to `klassets_response_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param type Type of prediction, one of prob, response, node.
#' @param ntree Number of trees to grow for the forest.
#' @param maxdepth Max depth of each trees.
#' @param trace A logical indicating if a progress bar shall be printed while the forest grows.
#' @param ... Options for `ranger::ranger`.
#'
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(n = 1000, relationship = function(x, y) x**2 > sin(y))
#'
#' plot(df)
#'
#' dfcrf <- fit_classification_random_forest(df)
#'
#' dfcrf
#'
#' plot(dfcrf)
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
#' @importFrom ranger ranger
#' @export
fit_classification_random_forest <- function(df,
                                             type = "prob",
                                             ntree = 500L,
                                             maxdepth = NULL,
                                             trace = FALSE,
                                             ...){

  stopifnot(type %in% c("prob", "response"))

  mod <- ranger::ranger(
    response ~ x + y,
    data      = df,
    num.trees = ntree,
    verbose   = trace,
    max.depth = maxdepth,
    probability = if_else(type == "prob", TRUE, FALSE),
    ...
  )

  predictions <- ranger::predictions(mod)

  if(type == "prob"){
    predictions <- predictions[, 2]
  }

  df <- dplyr::mutate(df, prediction = predictions)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_xy_classification_random_forest", class(df))

  attr(df, "type")  <- type
  attr(df, "model") <- mod

  df

}

#' Fit K Nearest Neighbours to `klassets_response_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param neighbours The neighbours parameter.
#' @param type Type of prediction, one of prob or response.
#'
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(relationship = function(x, y) x**2 > sin(y))
#'
#' plot(df)
#'
#' # defaults to prob
#' fit_knn(df)
#'
#' fit_knn(df, type = "response")
#'
#' plot(fit_knn(df))
#'
#' plot(fit_knn(df, neighbours = 3))
#'
#' plot(fit_knn(df, neighbours = 10))
#'
#' plot(fit_knn(df, neighbours = 200))
#'
#' plot(fit_knn(df, neighbours = 3, type = "response"))
#'
#' plot(fit_knn(df, neighbours = 10, type = "response"))
#'
#' plot(fit_knn(df, neighbours = 200, type = "response"))
#'
#' @importFrom class knn
#' @export
fit_knn <- function(df, neighbours = 10, type = "prob"){

  stopifnot(type %in% c("prob", "response"))

  preds <- class::knn(
    train = dplyr::select(df, .data$x, .data$y) |> as.matrix(),
    test  = dplyr::select(df, .data$x, .data$y) |> as.matrix(),
    cl    = dplyr::select(df, .data$response)  |> as.matrix(),
    k     = neighbours,
    prob  = TRUE
  )

  if(type == "prob"){
    predictions <- attr(preds, "prob")
  } else {
    predictions <- as.factor(preds)
  }

  df <- dplyr::mutate(df, prediction = predictions)

  # Mmm...
  class(df) <- setdiff(class(df), "klassets_response_xy")
  class(df) <- c("klassets_response_xy_knn", class(df))

  attr(df, "neighbours") <- neighbours
  attr(df, "type")       <- type

  df

}
