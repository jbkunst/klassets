library(tidyverse)
library(klassets)

set.seed(123)

dvars <- readRDS("D:/OneDrive - Ripley Corp/br-modelos-provision/data/M2/06_dvars.rds")

glimpse(dvars)

dedad <- select(dvars, rut, edad, periodo)

rm(dvars)

dedad <- dedad |>
  mutate(anio_nac = as.numeric(yyyymm::ym_format(periodo, "%Y")) - edad)

dedad

dedad <- dedad |>
  filter(edad >= 18, edad <= 80)

dedad <- dedad |>
  select(rut, anio_nac)

dedad <- dedad |>
  mutate(
    rut_noise = sample(c(-100:-1, 1:100), size = nrow(dedad), replace = TRUE),
    anio_noise = sample(c(-2, -1, 1, 2), size = nrow(dedad), replace = TRUE),
    rut_new = rut + rut_noise,
    anio_nac_new = anio_nac + anio_noise
  )

idyob <- dedad |>
  select(x = rut_new, y = anio_nac_new)

class(idyob) <- c( "klassets_xy", class(idyob))

idyob100k <- sample_n(idyob, 100000)

plot(idyob100k)

idyob10k <- sample_n(idyob, 10000)

plot(idyob10k)

idyob1k <- sample_n(idyob, 1000)

plot(idyob1k)


# export ------------------------------------------------------------------
usethis::use_data(idyob10k, overwrite = TRUE)
usethis::use_data(idyob1k,  overwrite = TRUE)


