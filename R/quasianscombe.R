#' Generate _quasi_ Anscombe data sets Type 1
#'
#' This function generate a data set _Type 1_  creating first a `x` a random vector
#' then apply a linear transformation using `beta0` and `beta1` and finally
#' adding a normal distributed noise using `error_sd` creating `y` values.
#'
#' This is the _typical first_ example when regression analysis is taught.
#'
#' Thei
#'
#' @inheritParams sim_xy
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' df
#'
#' plot(df)
#'
#' plot(df, add_lm = FALSE)
#'
#' plot(sim_quasianscombe_set_1(n = 1000))
#'
#' plot(sim_quasianscombe_set_1(n = 1000, beta0 = 0, beta1 = 1, x_dist = runif))
#'
#' @importFrom tibble tibble
#' @importFrom stats coefficients lm rnorm runif
#' @export
sim_quasianscombe_set_1 <- function(n = 100,
                                    beta0 = 3,
                                    beta1 = 0.5,
                                    x_dist = purrr::partial(rnorm, mean = 5, sd = 1),
                                    error_dist = purrr::partial(rnorm, sd = 0.5)){

  df <- sim_xy(n, beta0, beta1, x_dist, error_dist)

  class(df) <- setdiff(class(df), "klassets_xy")
  class(df) <- c("klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 2: No linear relationship
#'
#' Data sets _Type 2_ shows how a no linear realtionship between `x` and `y` can
#' lead in the same regression model (in terms of parameter values) of
#' the _Type 1_.
#'
#' @param df A data frame from `sim_quasianscombe_set_1` (or similar).
#' @param fun A function to apply, this is applied to normalized version of `x`.
#' @param residual_factor Numeric value to multiply residual to modify their
#'     variance.
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' dataset2 <- sim_quasianscombe_set_2(df)
#'
#' dataset2
#'
#' plot(dataset2)
#'
#' plot(sim_quasianscombe_set_2(df, residual_factor = 0))
#'
#' fun1 <- function(x){ 2 * sin(x*diff(range(x))) }
#'
#' plot(sim_quasianscombe_set_2(df, fun = fun1))
#'
#' fun2 <- abs
#'
#' plot(sim_quasianscombe_set_2(df, fun = fun2))
#'
#' fun3 <- function(x){ (x - mean(x)) * sin(x*diff(range(x))) }
#'
#' plot(sim_quasianscombe_set_2(df, fun = fun3))
#'
#' @importFrom stats dnorm sd
#' @export
sim_quasianscombe_set_2 <- function(df,
                                    fun = function(x){ x**2 },
                                    residual_factor = 0.25){

  # df <- sim_quasianscombe_set_1()

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  x_mean <- mean(df$x)
  x_stdv <- sd(df$x)

  e_new <- e * residual_factor
  # e_new <- 0

  df <- df |>
    dplyr::mutate(
      x2 = (.data$x - x_mean)/x_stdv,
      # x2 = x,
      y2 = fun(.data$x2) + e_new
      )

  # plot(df |> dplyr::select(x, y))
  # plot(df |> dplyr::select(x = x2, y = y2))

  # kind of _rotate_ to get same b1
  # I think this method is more fancy than sim_quasianscombe_set_5
  f_to_optim <- function(value = 1){

    y2_new <- dplyr::pull(df, .data$y2)

    values <- scales::rescale(1 - dnorm(df$x2), to = c(0, 1)) * sign(df$x2) * value
    # values <- scales::rescale(1 - df$x2, to = c(0, 1)) * sign(df$x2) * value
    # values <- seq(-1, 1, length.out = n) * value

    y2_new <- y2_new + values

    y2_mod <- lm(y2_new ~ x, data = tibble(x = pull(df, .data$x), y2_new))

    (b[2] - coefficients(y2_mod)[2])^2

  }

  # values  <- seq(-5, 5, length.out = 100)
  # fvalues <- Vectorize(f_to_optim)(values)
  # plot(values, fvalues)
  value <- suppressWarnings(optim(0, f_to_optim)$par)

  # f_to_optim(value)

  values <- scales::rescale(1 - dnorm(df$x2), to = c(0, 1)) * sign(df$x2) * value
  # values <- scales::rescale(1 - df$x2, to = c(0, 1)) * sign(df$x2) * value

  df <- df |>
    mutate(y2 = .data$y2 + values)

  # plot(df |> dplyr::select(x = x , y = y))
  # plot(df |> dplyr::select(x = x2, y = y2))

  # df <- df |>
  #   mutate(x2 = .data$x2*x_stdv + x_mean)
  #
  # plot(df |> dplyr::select(x = x , y = y))
  # plot(df |> dplyr::select(x = x2, y = y2))

  beta0     <- lm(y  ~ x, df)$coefficients[1]
  beta0_new <- lm(y2 ~ x, df)$coefficients[1]

  df <- df |>
    mutate(y2 = .data$y2 + beta0 - beta0_new)

  # plot(df |> dplyr::select(x, y = y))
  # plot(df |> dplyr::select(x = x, y = y2))

  df <- df |>
    select(x = .data$x, y = .data$y2)

  class(df) <- c( "klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 3: Extreme values (a.k.a Outliers)
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
#'   better visual impression of the outliers effect.
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

  class(df) <- c( "klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 4: 2 Clusters
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
#' complement group to have the same regression model in terms of coefficients.
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

  class(df) <- c( "klassets_quasianscombe", class(df))

  df

}

#' Generate _quasi_ Anscombe data sets Type 5: Heteroskedasticity
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
#' @param residual_factor Numeric value to multiply residual to modify their
#'     variance.
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
sim_quasianscombe_set_5 <- function(df, fun = identity, residual_factor = 10){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  e_new <- e * fun(1:length(e))

  e_new <- scales::rescale(e_new,to = c(min(abs(e)), max(abs(e)))) * residual_factor

  df <- df |>
    dplyr::mutate(y5 = b[1] + b[2] * .data$x + e_new)

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

  values <- seq(-1, 1, length.out = n) * value

  df <- df |>
    mutate(y5 = .data$y5 + values)

  # plot(df |> dplyr::select(x, y = y5))

  beta0     <- lm(y  ~ x, df)$coefficients[1]
  beta0_new <- lm(y5 ~ x, df)$coefficients[1]

  df <- df |>
    dplyr::mutate(y5 = .data$y5 + beta0 - beta0_new)

  # plot(df |> dplyr::select(x, y = y5))

  df <- df |>
    dplyr::select(x = .data$x, y = .data$y5)

  class(df) <- c( "klassets_quasianscombe", class(df))

  df

}


#' Generate _quasi_ Anscombe data sets Type 6: Simpson's Paradox
#'
#' Data sets _Type 6_ recreates the phenomenon of Simpon's paradox.
#'
#' This function will take `x` vector and separate `groups` groups to apply
#' a local model with a modified regresion using the `b1_factor` factor.
#'
#' The residual will be multiply with a value between 0 and 1 to make the visual
#' effect greater.
#'
#' @param df A data frame from `sim_quasianscombe_set_1` (or similar).
#' @param groups Number of groups to separate `x` values.
#' @param b1_factor A numeric value get the slope in each group from $beta_1$.
#' @param residual_factor Numeric value to multiply residual to modify their
#'     variance.
#'
#' @examples
#'
#' df <- sim_quasianscombe_set_1()
#'
#' dataset6 <- sim_quasianscombe_set_6(df)
#'
#' dataset6
#'
#' plot(dataset6)
#'
#' @export
sim_quasianscombe_set_6 <- function(df,
                                    groups = 3,
                                    b1_factor = -1,
                                    residual_factor = 0.25){

  # pars
  modlm <- lm(y ~ x, data = df)
  b <- coefficients(modlm)
  e <- modlm$residuals
  n <- length(e)

  group <- sort(rep(1:groups, length.out = n))

  df <- df |>
    mutate(y6 = b[1] + b[2] * b1_factor * .data$x + e*residual_factor)

  # plot(df |> dplyr::select(x, y))
  # plot(df |> dplyr::select(x, y = y6))

  # kind of _rotate_ in groups
  f_to_optim <- function(value = 1.2){

    y6_new <- pull(df, .data$y6)

    values <- sort(rep(seq(-1, 1, length.out = groups), length.out = n)) * value

    # table(group, values)

    y6_new <- y6_new + values

    y6_mod <- lm(y6_new ~ x, data = tibble(x = pull(df, .data$x), y6_new))

    (b[2] - coefficients(y6_mod)[2])^2

  }

  value <- suppressWarnings(optim(0, f_to_optim)$par)

  values <- values <- sort(rep(seq(-1, 1, length.out = groups), length.out = n)) * value

  df <- df |>
    mutate(y6 = .data$y6 + values)

  # plot(df |> dplyr::select(x, y = y6))

  beta0     <- lm(y  ~ x, df)$coefficients[1]
  beta0_new <- lm(y6 ~ x, df)$coefficients[1]

  df <- df |>
    mutate(y6 = .data$y6 + beta0 - beta0_new)

  # plot(df |> dplyr::select(x, y = y6))

  df <- df |>
    select(x = .data$x, y = .data$y6)

  # plot(df)

  class(df) <- c( "klassets_quasianscombe", class(df))

  df

}


