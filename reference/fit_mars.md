# Fit Multivariate Adaptive Regression Splines to `klassets_xy` object

Fit Multivariate Adaptive Regression Splines to `klassets_xy` object

## Usage

``` r
fit_mars(df, ...)
```

## Arguments

- df:

  A object from `sim_xy`.

- ...:

  Options for
  [`earth ::earth`](https://rdrr.io/pkg/earth/man/earth.html).

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  2.45  4.75
#>  2  2.49  4.56
#>  3  2.62  4.53
#>  4  2.80  4.59
#>  5  2.83  5.06
#>  6  2.84  3.92
#>  7  2.89  3.89
#>  8  2.96  4.78
#>  9  2.98  4.43
#> 10  2.99  4.53
#> # ℹ 490 more rows

if (rlang::is_installed("earth")) {
  dfmars <- fit_mars(df)

  dfmars

  plot(dfmars)

  df <- sim_xy(n = 1000, x_dist = runif)
  df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
  plot(df)

  plot(fit_mars(df))
}

```
