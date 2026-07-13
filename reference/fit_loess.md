# Fit Local polynomial regression to `klassets_xy` object

Fit Local polynomial regression to `klassets_xy` object

## Usage

``` r
fit_loess(df, ...)
```

## Arguments

- df:

  A object from `sim_xy`.

- ...:

  Options for [`stats::loess`](https://rdrr.io/r/stats/loess.html).

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  2.36  3.73
#>  2  2.43  4.47
#>  3  2.48  3.23
#>  4  2.68  4.38
#>  5  2.77  4.36
#>  6  2.85  4.17
#>  7  2.86  3.64
#>  8  2.88  3.47
#>  9  2.95  4.40
#> 10  2.99  3.60
#> # ℹ 490 more rows

dfloess <- fit_loess(df)

dfloess
#> # A tibble: 500 × 3
#>        x     y prediction
#>    <dbl> <dbl>      <dbl>
#>  1  2.36  3.73       3.72
#>  2  2.43  4.47       3.80
#>  3  2.48  3.23       3.85
#>  4  2.68  4.38       4.05
#>  5  2.77  4.36       4.14
#>  6  2.85  4.17       4.21
#>  7  2.86  3.64       4.22
#>  8  2.88  3.47       4.24
#>  9  2.95  4.40       4.30
#> 10  2.99  3.60       4.34
#> # ℹ 490 more rows

plot(dfloess)
#> Warning: Removed 9 rows containing missing values or values outside the scale range
#> (`geom_line()`).


df <- sim_xy(n = 1000, x_dist = runif)
df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
plot(df)


plot(fit_loess(df))
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_line()`).

```
