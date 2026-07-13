# Fit Logistic regression to `klassets_response_xy` object

Fit Logistic regression to `klassets_response_xy` object

## Usage

``` r
fit_logistic_regression(df, order = 1, stepwise = FALSE, verbose = FALSE)
```

## Arguments

- df:

  A object from `sim_response_xy`.

- order:

  Order of values of x and y.

- stepwise:

  A logical value to indicate to perform stepwise.

- verbose:

  A logical value to indicate to show the trace of the stepwise
  procedure.

## Examples

``` r

set.seed(123)

df <- sim_response_xy(n = 500, relationship = function(x, y) x**2 > y)

plot(df)


df_reg_log <- fit_logistic_regression(df)

plot(df_reg_log)


df_reg_log_3 <- fit_logistic_regression(df, order = 3, stepwise = TRUE)

plot(df_reg_log_3)

```
