library("dplyr")

run_variables <- readr::read_csv("run/run_variables.csv") %>%
  as.data.frame

wepp_run_questions <- readr::read_csv("run/wepp_run_questions.csv") %>%
  as.data.frame

usethis::use_data(run_variables, overwrite = TRUE)
usethis::use_data(wepp_run_questions, overwrite = TRUE)
