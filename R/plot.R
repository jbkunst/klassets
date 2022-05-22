#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth  labs
#  @importFrom ggplot2 xlim ylim
#' @importFrom stringr str_glue
#' @export
plot.klassets_xy <- function(x, ...){


  pxy <- ggplot2::ggplot(x, ggplot2::aes_string("x", "y")) +
    ggplot2::geom_point(
      shape = 21,
      color = "gray60",
      fill = "gray80"
    )

  if(inherits(x, "klassets_quasianscombe")){

    mod <- lm(y ~ x, data = x)

    b <- coefficients(mod)

    b0 <- round(b[1], 2)
    b1 <- round(b[2], 2)

    pxy <- pxy  +
      ggplot2::geom_smooth(method = "lm", se = FALSE,
                           color = "gray40", formula = y ~ x) +
      # ggplot2::xlim(c(0, NA)) +
      # ggplot2::ylim(c(0, NA)) +
      ggplot2::labs(title = stringr::str_glue("Model: y = {b0} + {b1} x"))

  }

  pxy

}
