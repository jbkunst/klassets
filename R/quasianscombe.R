#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth xlim ylim labs
#' @importFrom stringr str_glue
#' @export
plot.quasi_anscombe <- function(x, ...){

  # cor_xy <- cor(df[["x"]], df[["y"]])

  mod <- lm(y ~ x, data = x)

  b <- coefficients(mod)

  b0 <- round(b[1], 2)
  b1 <- round(b[2], 2)

  ggplot2::ggplot(x, ggplot2::aes_string("x", "y")) +
    ggplot2::geom_point(shape = 21, color = "gray60", fill = "gray80") +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "gray40") +
    ggplot2::xlim(c(0, NA)) +
    ggplot2::ylim(c(0, NA)) +
    ggplot2::labs(title = stringr::str_glue("Model: y = {b0} + {b1} x"))

}


#' Functions to generate _quasi_ Anscombe data sets
#'
#' @param n n, default value: 50
#' @param beta0 beta0, default value: 3,
#' @param beta1 beta1, default value: 0.5
#' @param error_sd error_sd, default value: 0.1
#' @param x_mean x_mean, default value: 5
#' @param x_dist x_dist, default value: "norm"
#' @param seed seed, default value: 1234
#'
#' @examples
#'
#' dataset <- sim_quasianscombe_set_1(n = 100)
#'
#' class(dataset)
#'
#' plot(dataset)
#'
#' @importFrom tibble tibble
#' @importFrom stats coefficients lm rnorm runif
#' @export
sim_quasianscombe_set_1 <- function(n = 50,
                                    beta0 = 3,
                                    beta1 = 0.5,
                                    error_sd = 0.1,
                                    x_mean = 5,
                                    x_dist = "norm",
                                    seed = 1234
                                    ){

  set.seed(seed)

  x <- sort(rnorm(n, x_mean))

  if(x_dist == "unif"){
    x <- sort(runif(n, min = min(x), max = max(x)))
  }

  e <- rnorm(n, error_sd)

  y <- beta0 + beta1 * x + e

  df <- tibble::tibble(x, y)

  class(df) <- c("quasi_anscombe", class(df))

  df

}
