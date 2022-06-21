library(tidyverse)


# Original source    http://yann.lecun.com/exdb/mnist/
# Converted to csv   https://pjreddie.com/projects/mnist-in-csv/
# Downloaded from    https://www.kaggle.com/datasets/oddrationale/mnist-in-csv

mnist_train <- read_csv(unz("data-raw/mnist.zip", "mnist_train.csv"))
mnist_test  <- read_csv(unz("data-raw/mnist.zip", "mnist_test.csv"))

# it is a classification problem
mnist_train <- mutate(mnist_train, label = factor(label))
mnist_test  <- mutate(mnist_test , label = factor(label))

# rename to dont have problem with some models
format_colname <- function(x = c("1x3", "12x3")){

  symbol <- "x"

  tibble(name = x) |>
    separate(name, c("x", "y"), sep = symbol) |>
    mutate(across(everything(), str_pad, width = 2, pad = "0")) |>
    unite(col = "name", everything(), sep = symbol) |>
    mutate(name = str_c("pixel_", name)) |>
    pull(name)

}

mnist_train <- rename_with(mnist_train, format_colname, .cols = -label)
mnist_test  <- rename_with(mnist_test,  format_colname, .cols = -label)

mnist_train
mnist_test

usethis::use_data(mnist_train, overwrite = TRUE)
usethis::use_data(mnist_test,  overwrite = TRUE)
