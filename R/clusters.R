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


#' Generate data sets to apply clustering algorithms
#'
#' @param n An integer.
#' @param groups An integer
#' @param props A vector of probabilities with length `groups`.
#' @examples
#'
#' set.seed(123456)
#'
#' df <- sim_groups()
#'
#' df
#'
#' plot(df)
#'
#' plot(sim_groups(500, 5))
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rbeta
#' @importFrom purrr map pmap map2_df
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
sim_groups <- function(n = 1000,
                                    groups = 3,
                                    props = NULL) {

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

  dpars <- tibble::tibble(
    n = ns,
    mu = mus,
    Sigma = sigmas
  )

  df <- purrr::pmap(dpars, MASS::mvrnorm) |>
    purrr::map(tibble::as_tibble, .name_repair = "minimal") |>
    purrr::map(purrr::set_names, c("x", "y"))

  df <- purrr::map2_df(
    df,
    seq_len_groups,
    ~ dplyr::mutate(.x, group = as.character(.y), .before = 1)
  )

  class(df) <- c("klassets_cluster", class(df))

  df

}

#' Generate intermediate iterations when performing K-means
#'
#' @param df A object from `sim_groups`.
#' @param centers How many clusters.
#' @param tolerance A value to indicating early stop.
#' @param max_iterations Max iterations to calculate.
#' @param verbose A logical value, to show or not iterations messages.
#' @examples
#'
#' set.seed(12)
#'
#' df <- sim_groups(n = 200, groups = 3)
#'
#' plot(df)
#'
#' set.seed(124)
#'
#' kmi <- kmeans_iterations(df, centers = 4, max_iterations = 6)
#'
#' plot(kmi)
#'
#' @importFrom stats dist
#' @export
kmeans_iterations <- function(df,
                              centers = 3,
                              tolerance = 10e-6,
                              max_iterations = 15,
                              verbose = FALSE
){

  stopifnot(inherits(df, "klassets_cluster"))

  daux <- df |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::mutate(cluster = NA_integer_)

  dcenters <- daux |>
    dplyr::sample_n(centers) |>
    dplyr::select(cx = .data$x, cy = .data$y) |>
    dplyr::arrange(.data$cx, .data$cy) |>
    dplyr::mutate(cluster = dplyr::row_number(), .before = 1)

  center_hist <- list(dcenters)
  data_hist   <- list(daux)

  iteration <- 1

  while(TRUE){

    if(verbose) message(iteration)

    daux <- tidyr::crossing(
      daux |> dplyr::select(.data$id, .data$group, .data$x, .data$y),
      dcenters
    ) |>
      dplyr::mutate(dist = (.data$x - .data$cx)^2 + (.data$y - .data$cy)^2) |>
      dplyr::group_by(.data$id) |>
      # new cluster
      dplyr::filter(dist == min(.data$dist)) |>
      dplyr::ungroup()

    # daux
    # daux |> count(group, cluster)

    dcenters <- daux |>
      dplyr::group_by(.data$cluster) |>
      # new centers
      dplyr::summarise(cx = mean(.data$x), cy = mean(.data$y))

    # dcenters

    daux <- daux |>
      select(.data$id, .data$group, .data$x, .data$y, .data$cluster)

    center_hist <- append(center_hist, list(dcenters))
    data_hist   <- append(data_hist, list(daux))

    c1 <- dplyr::last(center_hist) |>
      dplyr::select(.data$cx, .data$cy) |>
      as.matrix()

    c2 <- dplyr::nth(center_hist, -2) |>
      dplyr::select(.data$cx, .data$cy) |>
      as.matrix()

    if(mean((c1 - c2)^2) < tolerance) break

    iteration <- iteration + 1

    if(iteration == max_iterations) break

  }

  length(center_hist)
  length(data_hist)

  seq_along_i <- seq_len(length(center_hist))

  center_hist <- map2_df(center_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))

  assigned_cluster <- pull(center_hist, .data$cluster)
  assigned_cluster <- LETTERS[assigned_cluster]

  center_hist <- center_hist |>
    dplyr::mutate(cluster = forcats::fct_inorder(assigned_cluster))

  data_hist <- map2_df(data_hist, seq_along_i, ~ mutate(.x, iteration = .y, .before = 1))

  assigned_cluster <- pull(data_hist, .data$cluster)
  assigned_cluster <- LETTERS[assigned_cluster]

  data_hist <- data_hist |>
    dplyr::mutate(cluster = forcats::fct_inorder(assigned_cluster))

  # data_hist |> dplyr::count(cluster)

  kmi <- list(points = data_hist, centers = center_hist)

  class(kmi) <- c("klassets_kmiterations", class(kmi))

  kmi

}


#' Fit K-means to `klassets_cluster` object
#'
#' @param df A `klassets_cluster` object. A object from `sim_groups`.
#' @param centers A numeric value to pass to `kmeans_iterations` function
#'   The famous k parameter.
#' @param ... Extra parameters for `kmeans_iterations` function.
#'
#' @examples
#'
#' set.seed(12)
#'
#' df <- sim_groups(n = 200, groups = 3)
#'
#' plot(df)
#'
#' set.seed(124)
#'
#' dfc <- fit_kmeans(df, centers = 4, max_iterations = 6)
#'
#' plot(dfc)
#'
#' @export
fit_kmeans <- function(df, centers = 3, ...){

  stopifnot(inherits(df, "klassets_cluster"))

  kmi <- kmeans_iterations(df, centers = centers, ...)

  cls <- kmi$points |>
    dplyr::filter(.data$iteration == max(.data$iteration)) |>
    pull(.data$cluster)

  df <- df |>
    mutate(cluster = cls)

  df

}

#' Fit K-means to `klassets_cluster` object using `stats::kmeans`
#'
#' @param df A `klassets_cluster` object.
#' @param centers A numeric value to pass to `stats::kmeans` method.
#'   The famous k parameter.
#' @param ... Extra parameter for `stats::kmeans` function.
#'
#' @examples
#'
#' set.seed(12)
#'
#' df <- sim_groups(n = 200, groups = 3)
#'
#' plot(df)
#'
#' dfc <- fit_statskmeans(df, centers = 4)
#'
#' plot(dfc)
#'
#' @importFrom stats kmeans
#' @export
fit_statskmeans <- function(df, centers = 3, ...){

  stopifnot(inherits(df, "klassets_cluster"))

  cl <- kmeans(
    dplyr::select(df, .data$x, .data$y),
    centers = centers,
    ...
  )

  assigned_cluster <- cl$cluster

  assigned_cluster <- LETTERS[assigned_cluster]

  df <- df |>
    mutate(cluster = assigned_cluster)

  df

}


#' Fit Hierarchical Clustering to `klassets_cluster` object using `stats::hclust`
#'
#' @param df A `klassets_cluster` object.
#' @param k A numeric determine number of clusters. This value is passed to
#'   `stats::cutree` method.
#' @param method The agglomeration method to be used.
#'
#' @examples
#'
#' set.seed(12)
#'
#' df <- sim_groups(n = 200, groups = 3)
#'
#' plot(df)
#'
#' dfhc <- fit_hclust(df, k = 4)
#'
#' plot(dfhc)
#'
#' @importFrom stats dist hclust cutree
#' @export
fit_hclust <- function(df, k = 3, method = "complete"){

  stopifnot(inherits(df, "klassets_cluster"))


  distances <- dist(dplyr::select(df, .data$x, .data$y))

  hc <- stats::hclust(distances, method = method)

  clust <- stats::cutree(hc, k = k)

  clust <- LETTERS[clust]

  df <- df |>
    dplyr::mutate(cluster = clust)

  df

}

