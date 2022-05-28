add_power_variables_to_data_frame <- function(df, order = 1){

  if(order > 1) {

    pwr <- function(x, p) { x**p }

    fns <- purrr::map(2:order, ~ purrr::partial(pwr, p = .x)) |>
      purrr::set_names(stringr::str_c(2:order))

    df <- dplyr::mutate(df, dplyr::across(.data$x:.data$y, .fns = fns))

  }

  df

}

create_grid_from_data_frame <- function(df, length_seq = 100){

  stopifnot("x" %in% names(df))
  stopifnot("y" %in% names(df))

  dfgrid <- tidyr::crossing(
    x = create_seq_from_vector(dplyr::pull(df, .data$x), length_seq = length_seq),
    y = create_seq_from_vector(dplyr::pull(df, .data$y), length_seq = length_seq)
  )

  dfgrid

}

create_seq_from_vector <- function(x, length_seq = 100){

  # x <- rnorm(10)

  xseq <- pretty(x)

  seq(min(xseq), max(xseq), length.out = length_seq)

}

