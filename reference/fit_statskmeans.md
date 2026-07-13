# Fit K-means to `klassets_cluster` object using `stats::kmeans`

Fit K-means to `klassets_cluster` object using
[`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html)

## Usage

``` r
fit_statskmeans(df, centers = 3, ...)
```

## Arguments

- df:

  A `klassets_cluster` object.

- centers:

  A numeric value to pass to
  [`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html) method. The
  famous k parameter.

- ...:

  Extra parameter for
  [`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html) function.

## Examples

``` r

set.seed(12)

df <- sim_groups(n = 200, groups = 3)

plot(df)


dfc <- fit_statskmeans(df, centers = 4)

plot(dfc)

```
