#' Generate data sets to apply a binary classifier or logistic regression
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
sim_response_xy <- function(n = 100,
                            x_dist = purrr::partial(rnorm, mean = 0, sd = 1),
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
    .before = 1
    )

  class(df) <- c("klassets_response_xy", class(df))

  df

}

#' Apply Logistic regression to `klassets_response_xy` object
#'
#' @param df A object from `sim_response_xy`.
#' @param order Order of values of x and y.
#' @param stepwise A logical value to indicate to perform stepwise.
#'
#' @examples
#'
#' set.seed(123)
#'
#' df <- sim_response_xy(n = 500)
#'
#' df <- apply_logistic_regression(df)
#'
#' df
#'
#' @importFrom stats binomial
#' @export
apply_logistic_regression <- function(df, order = 1, stepwise = FALSE){

  if(order > 1) {

    pwr <- function(x, p) { x**p }

    fns <- purrr::map(
      2:order, ~ purrr::partial(pwr, p = .x)
      ) |>
      purrr::set_names(stringr::str_c(2:order))

    df <- dplyr::mutate(df, dplyr::across(.data$x:.data$y, .fns = fns))

  }

  mod <- glm(response ~ .,  family = binomial, data = df)

  if(stepwise) mod <- step(mod)

  mod

  df <- df |>
    dplyr::mutate(prediction = predict(mod, newdata = df)) |>
    dplyr::select(.data$response, .data$x, .data$y, .data$prediction)

  df

}


