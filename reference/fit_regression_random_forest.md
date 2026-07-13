# Fit regression random forest to `klassets_xy` object

Fit regression random forest to `klassets_xy` object

## Usage

``` r
fit_regression_random_forest(
  df,
  ntree = 500L,
  maxdepth = Inf,
  trace = FALSE,
  ...
)
```

## Arguments

- df:

  A object from `sim_xy`.

- ntree:

  Number of trees to grow for the forest.

- maxdepth:

  Max depth of each trees.

- trace:

  A logical indicating if a progress bar shall be printed while the
  forest grows.

- ...:

  Options for
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md).

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  1.87  3.22
#>  2  2.16  4.59
#>  3  2.32  4.50
#>  4  2.34  4.02
#>  5  2.39  4.19
#>  6  2.61  4.55
#>  7  2.68  4.67
#>  8  2.78  3.53
#>  9  2.80  5.13
#> 10  2.94  3.77
#> # ℹ 490 more rows

if (rlang::is_installed("ranger")) {
  dfrrf <- fit_regression_random_forest(df)

  dfrrf

  plot(dfrrf)

  df <- sim_xy(n = 1000, x_dist = runif)
  df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
  plot(df)

  plot(fit_regression_random_forest(df))
  plot(fit_regression_random_forest(df, ntree = 100, maxdepth = 3))
}

```
