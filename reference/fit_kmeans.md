# Fit K-means to `klassets_cluster` object

Fit K-means to `klassets_cluster` object

## Usage

``` r
fit_kmeans(df, centers = 3, ...)
```

## Arguments

- df:

  A `klassets_cluster` object. A object from `sim_groups`.

- centers:

  A numeric value to pass to `kmeans_iterations` function The famous k
  parameter.

- ...:

  Extra parameters for `kmeans_iterations` function.

## Examples

``` r

set.seed(12)

df <- sim_groups(n = 200, groups = 3)

plot(df)


set.seed(124)

dfc <- fit_kmeans(df, centers = 4, max_iterations = 6)

plot(dfc)

```
