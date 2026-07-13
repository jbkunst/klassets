# Plot some digits from train mnist data

Plot some digits from train mnist data

## Usage

``` r
mnist_plot_digits(ids = NULL)
```

## Arguments

- ids:

  Rows to show.

## Examples

``` r

mnist_plot_digits(1)


mnist_plot_digits(c(10, 20, 40, 22))


set.seed(123)

mnist_plot_digits(sample(seq(60000), size = 16))

```
