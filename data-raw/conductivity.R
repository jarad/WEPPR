library("dplyr")
library("tidyr")
library("readr")

effective_hydraulic_conductivity <-
  readr::read_csv("conductivity/effective_hydraulic_conductivity.csv",
                col_types = readr::cols(
                  soil = readr::col_character(),
                  .default = readr::col_double()
                ))

usethis::use_data(effective_hydraulic_conductivity, overwrite = TRUE)


################################################################################

time_invariant_effective_conductivity_multiplier <-
  readr::read_csv("conductivity/time_invariant_effective_conductivity_multiplier.csv",
                  col_types = readr::cols(
                    management = readr::col_character(),
                    .default = readr::col_double()
                  )) %>%
  tidyr::gather(hydrologic_group, multiplier, -management)


usethis::use_data(time_invariant_effective_conductivity_multiplier, overwrite = TRUE)
