
<!-- README.md is generated from README.Rmd. Please edit that file -->

# klassets

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbkunst/klassets/workflows/R-CMD-check/badge.svg)](https://github.com/jbkunst/klassets/actions)
<!-- badges: end -->

The `{klassets}` package is a collection of functions to simulate data
sets to:

-   Teach how some Statistics Models and Machine Learning algorithms
    works.
-   Illustrate certain some particular events such as heteroskedasticity
    or the Simpson’s paradox

<img src="man/figures/animation_quasi_anscombre.gif" width="100%" />

For example:

``` r
library(klassets)

set.seed(123)

df <- sim_quasianscombe_set_1(n = 500, beta0 = 3, beta1 = 0.5)

plot(df) +
  ggplot2::labs(subtitle = "Very similar to the given parameters (3 and 0.5)")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
library(patchwork)

df2 <- sim_quasianscombe_set_2(df, fun = sin)
df6 <- sim_quasianscombe_set_6(df, groups = 2, b1_factor = -1)

plot(df2) + plot(df6)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Another example of what can be donde with `{klassets}`.

<img src="man/figures/animation_kmeans_iterations.gif" width="100%" />

## Where to start

You can check:

-   `vignette("Quasi-Anscombe-data-sets")` to know more about
    `sim_quasianscombe_set*` functions family.
-   `vignette("Clustering-helpers")` to check some helper for
    clustering.

## Installation

You can install the development version of klassets from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jbkunst/klassets")
```

## Why *Klassets*?

Just a weird merge for Class/Klass and sets.

## Inspiration/Similar Ideas

-   <https://jumpingrivers.github.io/datasauRus/>
-   <https://eliocamp.github.io/metamer/>
-   <http://www.econometricsbysimulation.com/2019/03/the-importance-of-graphing-your-data.html>
    This is almost the same, but the approach it’s different.
