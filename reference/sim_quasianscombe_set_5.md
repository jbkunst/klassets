# Generate *quasi* Anscombe data sets Type 5: Heteroskedasticity

Data sets *Type 5* recreates the phenomenon of heteroskedasticity in the
residuals.

## Usage

``` r
sim_quasianscombe_set_5(df, fun = identity, residual_factor = 10)
```

## Arguments

- df:

  A data frame from `sim_quasianscombe_set_1` (or similar).

- fun:

  A function to apply to the index to multiply the residuals of the
  original model.

- residual_factor:

  Numeric value to multiply residual to modify their variance.

## Details

This function will take residuals \$e_i\$ and then get \$e'\_i = e_i \*
fun(i)\$ and then rescale the \$e'\_i\$ to the range of \$e_i\$.

## Examples

``` r

df <- sim_quasianscombe_set_1()

dataset5 <- sim_quasianscombe_set_5(df)

dataset5
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  1.25  3.61
#>  2  2.19  4.08
#>  3  2.40  4.20
#>  4  2.45  4.25
#>  5  2.57  4.25
#>  6  2.68  4.30
#>  7  2.70  4.32
#>  8  2.79  4.42
#>  9  2.97  4.60
#> 10  2.97  4.42
#> # ℹ 490 more rows

plot(dataset5)


plot(sim_quasianscombe_set_5(df, fun = rev))


plot(sim_quasianscombe_set_5(df, fun = sqrt))


plot(sim_quasianscombe_set_5(df, fun = log))


plot(sim_quasianscombe_set_5(df, fun = function(x) x^(1+0.6)))

```
