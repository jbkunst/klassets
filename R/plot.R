#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth  labs
#  @importFrom ggplot2 xlim ylim
#' @importFrom stringr str_glue
#' @export
plot.klassets_quasianscombe <- function(x, add_lm = TRUE, ...){

  pxy <- ggplot2::ggplot(x, ggplot2::aes_string("x", "y"))  +
    ggplot2::geom_point(
      shape = 21,
      color = "gray60",
      fill = "gray80"
    )

  if(add_lm){

    mod <- lm(y ~ x, data = x)

    b <- coefficients(mod)

    b0 <- round(b[1], 2)
    b1 <- round(b[2], 2)

    pxy <- pxy +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE,
        color = "gray40", formula = y ~ x
        ) +
      ggplot2::labs(title = stringr::str_glue("Model: y = {b0} + {b1} x"))

  }

  pxy


}

#' @export
plot.klassets_cluster <- function(x, ...){

  pxy <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))

  if(all(c("group", "cluster") %in% names(x))){

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(
          shape = (.data$group),
          color = (.data$cluster)
          ),
        ...
        )

  } else if ("group" %in% names(x)) {

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(shape = (.data$group)),
        color = "gray60",
        fill = "gray80",
        ...

      )

  } else if ("cluster" %in% names(x)) {

    pxy <- pxy +
      ggplot2::geom_point(
        ggplot2::aes(color = (.data$cluster)),
        shape = 21,
        ...
        )

  } else {

    pxy <- pxy +
      ggplot2::geom_point(shape = 21, color = "gray60", fill = "gray80", ...)

  }

  pxy

}
