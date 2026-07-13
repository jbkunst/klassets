# Package index

## Simulation of Quasi-Anscombe data sets

Functions to simulate data sets with related regression patterns.

- [`sim_quasianscombe_set_1()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_1.md)
  :

  Generate *quasi* Anscombe data sets Type 1

- [`sim_quasianscombe_set_2()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_2.md)
  :

  Generate *quasi* Anscombe data sets Type 2: No linear relationship

- [`sim_quasianscombe_set_3()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_3.md)
  :

  Generate *quasi* Anscombe data sets Type 3: Extreme values (a.k.a
  Outliers)

- [`sim_quasianscombe_set_4()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_4.md)
  :

  Generate *quasi* Anscombe data sets Type 4: 2 Clusters

- [`sim_quasianscombe_set_5()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_5.md)
  :

  Generate *quasi* Anscombe data sets Type 5: Heteroskedasticity

- [`sim_quasianscombe_set_6()`](https://jkunst.com/klassets/reference/sim_quasianscombe_set_6.md)
  :

  Generate *quasi* Anscombe data sets Type 6: Simpson's Paradox

## Clustering helpers

Functions to simulate data and explain clustering algorithms.

- [`sim_groups()`](https://jkunst.com/klassets/reference/sim_groups.md)
  : Generate data sets to apply clustering algorithms

- [`fit_kmeans()`](https://jkunst.com/klassets/reference/fit_kmeans.md)
  :

  Fit K-means to `klassets_cluster` object

- [`fit_hclust()`](https://jkunst.com/klassets/reference/fit_hclust.md)
  :

  Fit Hierarchical Clustering to `klassets_cluster` object using
  [`stats::hclust`](https://rdrr.io/r/stats/hclust.html)

- [`fit_statskmeans()`](https://jkunst.com/klassets/reference/fit_statskmeans.md)
  :

  Fit K-means to `klassets_cluster` object using
  [`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html)

- [`kmeans_iterations()`](https://jkunst.com/klassets/reference/kmeans_iterations.md)
  : Generate intermediate iterations when performing K-means

## Binary classification

Functions to simulate data and explain classification algorithms.

- [`sim_response_xy()`](https://jkunst.com/klassets/reference/sim_response_xy.md)
  : Generate data sets to apply binary classifiers

- [`fit_logistic_regression()`](https://jkunst.com/klassets/reference/fit_logistic_regression.md)
  :

  Fit Logistic regression to `klassets_response_xy` object

- [`fit_classification_tree()`](https://jkunst.com/klassets/reference/fit_classification_tree.md)
  :

  Fit classification tree to `klassets_response_xy` object

- [`fit_classification_random_forest()`](https://jkunst.com/klassets/reference/fit_classification_random_forest.md)
  :

  Fit classification random forest to `klassets_response_xy` object

- [`fit_knn()`](https://jkunst.com/klassets/reference/fit_knn.md) :

  Fit K Nearest Neighbours to `klassets_response_xy` object

## Regression

Functions to simulate data and explain regression algorithms.

- [`sim_xy()`](https://jkunst.com/klassets/reference/sim_xy.md) :
  Generate data sets to apply regression methods

- [`fit_linear_model()`](https://jkunst.com/klassets/reference/fit_linear_model.md)
  :

  Fit Linear model to `klassets_xy` object

- [`fit_regression_tree()`](https://jkunst.com/klassets/reference/fit_regression_tree.md)
  :

  Fit regression tree to `klassets_xy` object

- [`fit_linear_model_tree()`](https://jkunst.com/klassets/reference/fit_linear_model_tree.md)
  :

  Fit Linear Model tree to `klassets_xy` object

- [`fit_regression_random_forest()`](https://jkunst.com/klassets/reference/fit_regression_random_forest.md)
  :

  Fit regression random forest to `klassets_xy` object

- [`fit_loess()`](https://jkunst.com/klassets/reference/fit_loess.md) :

  Fit Local polynomial regression to `klassets_xy` object

- [`fit_mars()`](https://jkunst.com/klassets/reference/fit_mars.md) :

  Fit Multivariate Adaptive Regression Splines to `klassets_xy` object

## MNIST

Functions and data sets to fit models to MNIST data and compare
predictive performance and variable importance.

- [`mnist_train`](https://jkunst.com/klassets/reference/mnist_train.md)
  : MNIST train data
- [`mnist_test`](https://jkunst.com/klassets/reference/mnist_test.md) :
  MNIST test data
- [`mnist_plot_digits()`](https://jkunst.com/klassets/reference/mnist_plot_digits.md)
  : Plot some digits from train mnist data

## Example data sets

Small data sets used in examples and teaching material.

- [`idyob10k`](https://jkunst.com/klassets/reference/idyob10k.md) :
  10,000 observations of ID and year of birth
- [`idyob1k`](https://jkunst.com/klassets/reference/idyob1k.md) : 1000
  observations of ID and year of birth
