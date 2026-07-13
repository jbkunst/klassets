# Fit classification random forest to `klassets_response_xy` object

Fit classification random forest to `klassets_response_xy` object

## Usage

``` r
fit_classification_random_forest(
  df,
  type = "prob",
  ntree = 500L,
  maxdepth = NULL,
  trace = FALSE,
  ...
)
```

## Arguments

- df:

  A object from `sim_response_xy`.

- type:

  Type of prediction, one of prob, response, node.

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

set.seed(123)

df <- sim_response_xy(n = 1000, relationship = function(x, y) x**2 > sin(y))

plot(df)


if (rlang::is_installed("ranger")) {
  dfcrf <- fit_classification_random_forest(df)

  dfcrf

  plot(dfcrf)
}

```
