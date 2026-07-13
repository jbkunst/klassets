# Generate intermediate iterations when performing K-means

Generate intermediate iterations when performing K-means

## Usage

``` r
kmeans_iterations(
  df,
  centers = 3,
  tolerance = 1e-05,
  max_iterations = 15,
  verbose = FALSE
)
```

## Arguments

- df:

  A object from `sim_groups`.

- centers:

  How many clusters.

- tolerance:

  A value to indicating early stop.

- max_iterations:

  Max iterations to calculate.

- verbose:

  A logical value, to show or not iterations messages.

## Examples

``` r

set.seed(12)

df <- sim_groups(n = 200, groups = 3)

plot(df)


set.seed(124)

kmi <- kmeans_iterations(df, centers = 4, max_iterations = 6)

plot(kmi)

```
