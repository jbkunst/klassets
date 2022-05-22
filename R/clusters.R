Posdef <- function (n, ev = runif(n, 0, 10)) {
  # https://stat.ethz.ch/pipermail/r-help/2008-February/153708
  Z <- matrix(ncol = n, rnorm(n ^ 2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}


#' Generate data sets to apply a clustering algorithm
#'
#' @param n An intenger
#' @param groups An integer
#' @param props A vector of probabilties with length `groups`.
#' @param add_group_col A logical value.
#' @examples
#'
#' set.seed(123456)
#'
#' df <- sim_clusters()
#'
#' plot(df)
#'
#' set.seed(123456)
#'
#' df <- sim_clusters(add_group_col = TRUE)
#'
#' plot(df)
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rbeta
#' @importFrom purrr map pmap map2_df
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
sim_clusters <- function(n = 1000,
                         groups = 3,
                         props = NULL,
                         add_group_col = FALSE) {

  stopifnot(
    as.integer(n) == n,
    as.integer(groups) == groups,
    is.integer(as.integer(groups)),
    groups > 1
    )

  if(is.null(props)) {

    props <- rbeta(groups, 5, 5)
    props <- props/sum(props)

  } else {

    stopifnot(
      length(props) == groups,
      is.numeric(props)
    )

  }

  seq_len_groups <- seq_len(groups)

  mus <- purrr::map(seq_len_groups, ~ runif(2, min = -10, max = 10))

  sigmas <- purrr::map(seq_len_groups, function(k){

    Posdef(2)

  })

  ns <- round(n * props)

  # to get n
  ns[groups] <- ns[groups] + (sum(ns) - n)

  dpars <- tibble(
    n = ns,
    mu = mus,
    Sigma = sigmas
  )

  df <- purrr::pmap(dpars, MASS::mvrnorm) |>
    purrr::map(as_tibble, .name_repair = "minimal") |>
    purrr::map(purrr::set_names, c("x", "y"))

  if(add_group_col){

    df <- df |>
      purrr::map2_df(seq_len_groups, ~ mutate(.x, group = .y, .before = 1))

  } else {

    df <- dplyr::bind_rows(df)

  }

  class(df) <- c("klassets_xy", "klassets_cluster", class(df))

  df

}

apply_kmeans <- function(df){

  stopifnot(inherits(df, "klassets_cluster"))

}

apply_hclust <- function(df){

  stopifnot(inherits(df, "klassets_cluster"))

}

