# Fit Hierarchical Clustering to `klassets_cluster` object using `stats::hclust`

Fit Hierarchical Clustering to `klassets_cluster` object using
[`stats::hclust`](https://rdrr.io/r/stats/hclust.html)

## Usage

``` r
fit_hclust(df, k = 3, method = "complete")
```

## Arguments

- df:

  A `klassets_cluster` object.

- k:

  A numeric determine number of clusters. This value is passed to
  [`stats::cutree`](https://rdrr.io/r/stats/cutree.html) method.

- method:

  The agglomeration method to be used.

## Examples

``` r

set.seed(12)

df <- sim_groups(n = 200, groups = 3)

plot(df)


dfhc <- fit_hclust(df, k = 4)

plot(dfhc)

```
