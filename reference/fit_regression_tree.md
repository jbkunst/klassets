# Fit regression tree to `klassets_xy` object

Fit regression tree to `klassets_xy` object

## Usage

``` r
fit_regression_tree(df, maxdepth = Inf, alpha = 0.05, ...)
```

## Arguments

- df:

  A object from `sim_response_xy`.

- maxdepth:

  Max depth of the tree. Same used in
  [`partykit::ctree_control`](https://rdrr.io/pkg/partykit/man/ctree_control.html).

- alpha:

  Alpha value, same used in
  [`partykit::ctree_control`](https://rdrr.io/pkg/partykit/man/ctree_control.html)

- ...:

  Options for
  [`partykit::ctree_control`](https://rdrr.io/pkg/partykit/man/ctree_control.html).

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  1.85  4.55
#>  2  2.06  3.94
#>  3  2.18  4.24
#>  4  2.69  3.51
#>  5  2.70  4.42
#>  6  2.71  5.01
#>  7  2.74  4.19
#>  8  2.77  3.95
#>  9  2.86  5.40
#> 10  2.99  4.86
#> # ℹ 490 more rows

dflm <- fit_regression_tree(df)

dflm
#> # A tibble: 500 × 3
#>        x     y prediction
#>    <dbl> <dbl>      <dbl>
#>  1  1.85  4.55       4.60
#>  2  2.06  3.94       4.60
#>  3  2.18  4.24       4.60
#>  4  2.69  3.51       4.60
#>  5  2.70  4.42       4.60
#>  6  2.71  5.01       4.60
#>  7  2.74  4.19       4.60
#>  8  2.77  3.95       4.60
#>  9  2.86  5.40       4.60
#> 10  2.99  4.86       4.60
#> # ℹ 490 more rows

plot(dflm)


df <- sim_xy(n = 1000, x_dist = runif)
df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
plot(df)


plot(fit_regression_tree(df, maxdepth = 3))


# default
plot(fit_regression_tree(df))

```
