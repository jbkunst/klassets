# Fit Linear model to `klassets_xy` object

Fit Linear model to `klassets_xy` object

## Usage

``` r
fit_linear_model(df, order = 1, stepwise = FALSE, verbose = FALSE)
```

## Arguments

- df:

  A object from `sim_response_xy`.

- order:

  Order of predictive variable x.

- stepwise:

  A logical value to indicate to perform stepwise.

- verbose:

  A logical value to indicate to show the trace of the stepwise
  procedure.

## Examples

``` r

df <- sim_xy()

df
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  1.97  3.23
#>  2  2.08  3.82
#>  3  2.31  4.10
#>  4  2.49  4.42
#>  5  2.50  4.65
#>  6  2.60  4.04
#>  7  2.77  4.93
#>  8  2.88  4.25
#>  9  2.96  3.73
#> 10  2.98  4.85
#> # ℹ 490 more rows

dflm <- fit_linear_model(df)

dflm
#> # A tibble: 500 × 3
#>        x     y prediction
#>    <dbl> <dbl>      <dbl>
#>  1  1.97  3.23       4.02
#>  2  2.08  3.82       4.07
#>  3  2.31  4.10       4.18
#>  4  2.49  4.42       4.27
#>  5  2.50  4.65       4.28
#>  6  2.60  4.04       4.32
#>  7  2.77  4.93       4.41
#>  8  2.88  4.25       4.46
#>  9  2.96  3.73       4.51
#> 10  2.98  4.85       4.52
#> # ℹ 490 more rows

plot(dflm)


df <- sim_xy(n = 1000, x_dist = runif)
df <- dplyr::mutate(df, y = y + 2*sin(5 * x))
plot(df)


plot(fit_linear_model(df))


plot(fit_linear_model(df, order = 5, stepwise = TRUE, verbose = TRUE))
#> Start:  AIC=-1331.12
#> y ~ x + x_2 + x_3 + x_4 + x_5
#> 
#>        Df Sum of Sq    RSS     AIC
#> <none>              261.03 -1331.1
#> - x_2   1    1.3189 262.35 -1328.1
#> - x_5   1    2.6237 263.65 -1323.1
#> - x     1    3.1489 264.18 -1321.1
#> - x_4   1    4.1460 265.18 -1317.4
#> - x_3   1    4.5131 265.54 -1316.0

```
