library("dplyr")

run_variables <- readr::read_csv("run_variables.csv") %>%
  as.data.frame

usethis::use_data(run_variables, overwrite = TRUE)
