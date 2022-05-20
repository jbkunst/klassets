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


#' Generate _quasi_ Anscombe data sets Type 1
#'
#' @param n n, default value: 100
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
#' dataset
#'
#' plot(dataset)
#'
#' @importFrom tibble tibble
#' @importFrom stats coefficients lm rnorm runif
#' @export
sim_quasianscombe_set_1 <- function(n = 100,
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



#' Generate _quasi_ Anscombe data sets Type 3
#'
#' Data sets _Type 3_ get some outliers but conserving the $x$ mean and
#' the same coefficients -but different significance- of the adjusted linear
#' model.
#'
#' This function will:
#' - Calculate the linear regression model and will calculate new trend usingo
#'   0.5 times beta1
#' - Take `prop`% values from the greater `2*prop` `x` values and modify the
#'   related `y` value to get the original estimation of `beta1`
#' - Apply `residual_factor` factor to residual to get minor variance and
#'   better visual impresion of the outlier effect.
#'
#' @param prop The proportion of value to modify as outliers.
#' @param residual_factor Numeric value to multiply residual to modify their
#'     variance.
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' dataset3 <- sim_quasianscombe_set_3(df)
#'
#' dataset3
#'
#' # plot(df)
#'
#' plot(dataset3)
#'
#' plot(sim_quasianscombe_set_3(df, prop = 0.1, residual_factor = 0))
#'
#' @importFrom dplyr select pull
#' @importFrom rlang .data
#' @export
sim_quasianscombe_set_3 <- function(df, prop = .05, residual_factor = 0.25){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  # creating y3 1st ver
  df <- df |>
    mutate(y3 = b[1] + (b[2] * 0.5) * .data$x + e * residual_factor)

  # plot(df)
  # plot(df |> select(x, y = y3))

  ids <- sample(seq(round((1 - 2*prop) * n), n), round(n * prop))

  f_to_optim <- function(value = 0){

    y3_new <- pull(df, y3)
    y3_new[ids] <- y3_new[ids] + value
    y3_mod <- lm(y3_new ~ x, data = tibble(x = pull(df, .data$x), y3_new))

    (b[2] - coefficients(y3_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  # apply the noise
  df <- df |>
    mutate(
      y3 = case_when(
        row_number() %in% ids ~ .data$y3 + value,
        TRUE                  ~ .data$y3
        )
    )

  # plot(df |> select(x, y = y3))

  # fix mean adding differences between intercept
  # y3_new <- y3_new - (mean(y) - mean(y3_new)) + b[0]

  beta0     <- lm(y  ~ x, df)$coefficients[1]
  beta0_new <- lm(y3 ~ x, df)$coefficients[1]

  df <- df |>
    mutate(y3 = .data$y3 + beta0 - beta0_new)

  df <- df |>
    select(x, y = y3)

  class(df) <- c("quasi_anscombe", class(df))

  df

}

