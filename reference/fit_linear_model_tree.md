# Fit Linear Model tree to `klassets_xy` object

Fit Linear Model tree to `klassets_xy` object

## Usage

``` r
fit_linear_model_tree(df, maxdepth = Inf, alpha = 0.05, ...)
```

## Arguments

- df:

  A object from `sim_response_xy`.

- maxdepth:

  Max depth of the tree. Same used in
  [`partykit::mob_control`](https://rdrr.io/pkg/partykit/man/mob_control.html).

- alpha:

  Alpha value, same used in
  [`partykit::mob_control`](https://rdrr.io/pkg/partykit/man/mob_control.html)

- ...:

  Additional options passed to
  [`partykit::mob_control`](https://rdrr.io/pkg/partykit/man/mob_control.html).

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  2.36  3.80
#>  2  2.41  3.60
#>  3  2.43  3.79
#>  4  2.56  4.61
#>  5  2.57  4.38
#>  6  2.58  3.27
#>  7  2.71  3.27
#>  8  2.76  4.18
#>  9  2.78  3.95
#> 10  2.82  4.43
#> # ℹ 490 more rows

dflm <- fit_linear_model_tree(df)

dflm
#> # A tibble: 500 × 3
#>        x     y prediction
#>    <dbl> <dbl>      <dbl>
#>  1  2.36  3.80       4.18
#>  2  2.41  3.60       4.20
#>  3  2.43  3.79       4.21
#>  4  2.56  4.61       4.28
#>  5  2.57  4.38       4.28
#>  6  2.58  3.27       4.29
#>  7  2.71  3.27       4.35
#>  8  2.76  4.18       4.37
#>  9  2.78  3.95       4.38
#> 10  2.82  4.43       4.41
#> # ℹ 490 more rows

plot(dflm)


df <- sim_xy(n = 1000, x_dist = runif)
df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
plot(df)


plot(fit_linear_model_tree(df))

```
