# Generate *quasi* Anscombe data sets Type 3: Extreme values (a.k.a Outliers)

Data sets *Type 3* get some outliers but conserving the \$x\$ mean and
the same coefficients -but different significance- of the adjusted
linear model.

## Usage

``` r
sim_quasianscombe_set_3(
  df,
  prop = 0.05,
  beta1_factor = 0.5,
  residual_factor = 0.25
)
```

## Arguments

- df:

  A data frame from `sim_quasianscombe_set_1` (or similar).

- prop:

  The proportion of value to modify as outliers.

- beta1_factor:

  Numeric value to modify the beta1 value.

- residual_factor:

  Numeric value to multiply residual to modify their variance.

## Details

This function will:

- Calculate the linear regression model and will calculate new trend
  using 0.5 times beta1

- Take `prop`% values from the greater `2*prop` `x` values and modify
  the related `y` value to get the original estimation of `beta1`

- Apply `residual_factor` factor to residual to get minor variance and
  better visual impression of the outliers effect.

## Examples

``` r

df <- sim_quasianscombe_set_1()

dataset3 <- sim_quasianscombe_set_3(df)

dataset3
#> # A tibble: 500 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1  2.17  4.36
#>  2  2.26  4.70
#>  3  2.37  4.81
#>  4  2.39  4.86
#>  5  2.41  4.81
#>  6  2.46  4.62
#>  7  2.59  4.75
#>  8  2.66  4.78
#>  9  2.68  4.64
#> 10  2.83  4.67
#> # ℹ 490 more rows

plot(dataset3)


plot(sim_quasianscombe_set_3(df, prop = 0.1, residual_factor = 0))

```
