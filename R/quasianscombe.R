#' Generate _quasi_ Anscombe data sets Type 1
#'
#' This function generate a data set _Type 1_  creating first a `x` a random vector
#' then apply a linear transformation using `beta0` and `beta1` and finally
#' adding a normal distributed noise using `error_sd` creating `y` values.
#'
#' This is the _typical_ example when regression analysis is taught.
#'
#' @param n n, default value: 100
#' @param beta0 beta0, default value: 3,
#' @param beta1 beta1, default value: 0.5
#' @param error_sd error_sd, default value: 0.1
#' @param x_mean x_mean, default value: 5
#' @param x_sd x_sd, default value: 1
#' @param x_dist x_dist, default value: "norm"
#' @param seed seed, default value: 1234
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' df
#'
#' plot(df)
#'
#' plot(sim_quasianscombe_set_1(x_sd = 0))
#'
#' plot(sim_quasianscombe_set_1(error_sd = 0))
#'
#' plot(sim_quasianscombe_set_1(x_mean = 0, x_sd = 0))
#'
#' @importFrom tibble tibble
#' @importFrom stats coefficients lm rnorm runif
#' @export
sim_quasianscombe_set_1 <- function(n = 100,
                                    beta0 = 3,
                                    beta1 = 0.5,
                                    error_sd = 0.5,
                                    x_mean = 5,
                                    x_sd = 1,
                                    x_dist = "norm",
                                    seed = 123
                                    ){

  set.seed(seed)

  x <- sort(rnorm(n = n, mean = x_mean, sd = x_sd))

  if(x_dist == "unif"){
    x <- sort(runif(n, min = min(x), max = max(x)))
  }

  e <- rnorm(n, sd = error_sd)

  y <- beta0 + beta1 * x + e

  df <- tibble::tibble(x, y)

  class(df) <- c("klassets_xy", "klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 3
#'
#' Data sets _Type 3_ get some outliers but conserving the $x$ mean and
#' the same coefficients -but different significance- of the adjusted linear
#' model.
#'
#' This function will:
#' - Calculate the linear regression model and will calculate new trend using
#'   0.5 times beta1
#' - Take `prop`% values from the greater `2*prop` `x` values and modify the
#'   related `y` value to get the original estimation of `beta1`
#' - Apply `residual_factor` factor to residual to get minor variance and
#'   better visual impresion of the outlier effect.
#'
#' @param df A data frame from `sim_quasianscombe_set_1` (or similar).
#' @param prop The proportion of value to modify as outliers.
#' @param beta1_factor Numeric value to modify the beta1 value.
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
#' plot(dataset3)
#'
#' plot(sim_quasianscombe_set_3(df, prop = 0.1, residual_factor = 0))
#'
#' @importFrom dplyr select mutate pull case_when row_number
#' @importFrom rlang .data
#' @importFrom stats optim
#' @export
sim_quasianscombe_set_3 <- function(df,
                                    prop = .05,
                                    beta1_factor = 0.5,
                                    residual_factor = 0.25){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  # creating y3 1st ver
  df <- df |>
    mutate(y3 = b[1] + (b[2] * beta1_factor) * .data$x + e * residual_factor)

  # plot(df)
  # plot(df |> select(x, y = y3))

  ids <- sample(seq(round((1 - 2*prop) * n), n), round(n * prop))

  f_to_optim <- function(value = 0){

    y3_new <- pull(df, .data$y3)
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
    select(x = .data$x, y = .data$y3)

  class(df) <- c("klassets_xy", "klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 4
#'
#' Data sets _Type 4_ recreate two cluster keeping the coefficient of the original
#' regression model.
#'
#' This function will:
#' - Disorder the order of `x` values.
#' - Rescale the `x` value to specific original quantiles.
#' - Then take a porportion of value and traslate to left keeppping the original
#' mean of `x`.
#' - Finally add some value to the associated `y` value and substract to the
#' complement group to have the same regression model in term of coefficients.
#'
#' @param df A data frame from `sim_quasianscombe_set_1` (or similar).
#' @param rescale_to Rescale the x value before create the second cluster.
#' @param prop The proportion of value to modify as the second group/cluster.
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' dataset4 <- sim_quasianscombe_set_4(df)
#'
#' dataset4
#'
#' plot(dataset4)
#'
#' plot(sim_quasianscombe_set_4(df, rescale_to = c(0, .1), prop = 0.5))
#'
#' @importFrom dplyr if_else between arrange
#' @importFrom stats quantile
#' @importFrom scales rescale
#' @export
sim_quasianscombe_set_4 <- function(df, rescale_to = c(.10, .20), prop = 0.15){

  # validation
  stopifnot(
    is.numeric(rescale_to),
    length(rescale_to) == 2,
    all(dplyr::between(rescale_to, 0, 1))
  )

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  qs <- quantile(df$x, rescale_to)

  df <- df |>
    mutate(x4 = sample(.data$x)) |>
    mutate(x4 = scales::rescale(.data$x4, to = qs))

  # plot(df |> select(x, y))      + ggplot2::xlim(0, NA)
  # plot(df |> select(x = x4, y)) + ggplot2::xlim(0, NA)

  ids <- sample(n, size = round(n * prop))

  f_to_optim <- function(value = 0){

    x4_new <- pull(df, .data$x4)
    x4_new[ids] <- x4_new[ids] + value

    (mean(df$x) - mean(x4_new))^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  df <- df |>
    mutate(x4 = dplyr::if_else(row_number() %in% ids, .data$x4 + value, .data$x4))

  f_to_optim2 <- function(value = 10){

    y4_new <- pull(df, .data$y)

    y4_new[ ids] <- y4_new[ ids] + value
    y4_new[-ids] <- y4_new[-ids] - value

    xy4_mod <- lm(y4 ~ x, data = tibble(x = df$x4, y4 = y4_new))

    # plot(df$x4, y4_new)

    (b[2] - coefficients(xy4_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim2)$par)

  df <- df |>
    mutate(
      y4 = .data$y,
      y4 = case_when(
        row_number() %in%  ids ~ .data$y4 + value,
        !row_number() %in% ids ~ .data$y4 - value
        )
      )

  beta0     <- lm(y  ~ x , df)$coefficients[1]
  beta0_new <- lm(y4 ~ x4, df)$coefficients[1]

  df <- df |>
    mutate(y4 = .data$y4 + beta0 - beta0_new)

  df <- df |>
    select(x = .data$x4, y = .data$y4) |>
    arrange(.data$x)

  class(df) <- c("klassets_xy", "klassets_quasianscombe", class(df))

  df

}

#' Generate _quasi_ Anscombe data sets Type 5
#'
#' Data sets _Type 5_ recreates the phenomenon of heteroskedasticity in
#' the residuals.
#'
#' This function will take residuals $e_i$ and then get $e'_i = e_i * fun(i)$
#' and then rescale the $e'_i$ to the range of $e_i$.
#'
#' @param df A data frame from `sim_quasianscombe_set_1` (or similar).
#' @param fun A function to apply to the index to multiply the residuals of
#'   the original model.
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' dataset5 <- sim_quasianscombe_set_5(df)
#'
#' dataset5
#'
#' plot(dataset5)
#'
#' plot(sim_quasianscombe_set_5(df, fun = rev))
#'
#' plot(sim_quasianscombe_set_5(df, fun = sqrt))
#'
#' plot(sim_quasianscombe_set_5(df, fun = log))
#'
#' plot(sim_quasianscombe_set_5(df, fun = function(x) x^(1+0.6)))
#'
#' @export
sim_quasianscombe_set_5 <- function(df, fun = identity){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  e_new <- e * fun(1:length(e))

  e_new <- scales::rescale(e_new,to = c(min(abs(e)), max(abs(e))))

  df <- df |>
    mutate(y5 = b[1] + b[2] * .data$x + e_new)

  # plot(df |> dplyr::select(x, y = y5))

  # kind of _rotate_ to get same b1
  f_to_optim <- function(value = 1){

    y5_new <- pull(df, .data$y5)

    values <- seq(-1, 1, length.out = n) * value

    y5_new <- y5_new + values

    y5_mod <- lm(y5_new ~ x, data = tibble(x = pull(df, .data$x), y5_new))

    (b[2] - coefficients(y5_mod)[2])^2

  }

  # values  <- seq(-5, 5, length.out = 100)
  # fvalues <- Vectorize(f_to_optim)(values)
  # plot(values, fvalues)

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  df <- df |>
    mutate(y5 = .data$y5 + seq(-1, 1, length.out = n) * value)

  # plot(df |> dplyr::select(x, y = y5))

  beta0     <- lm(y  ~ x, df)$coefficients[1]
  beta0_new <- lm(y5 ~ x, df)$coefficients[1]

  df <- df |>
    mutate(y5 = .data$y5 + beta0 - beta0_new)

  # plot(df |> dplyr::select(x, y = y5))

  df <- df |>
    select(x = .data$x, y = .data$y5)

  class(df) <- c("klassets_xy", "klassets_quasianscombe", class(df))

  df

}