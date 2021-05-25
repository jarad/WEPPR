cli_file <- system.file("extdata", "092.63x040.90.cli", package="WEPPR")

test_that("correct type", {
  expect_error(d <- read_cli(cli_file), NA)

  expect_type(d, "list")
  expect_named(d, c("cligen_version", "cligen_settings", "station", "location",
                    "averages", "precip", "breakpoints"))

  expect_type(d$cligen_version,  "list")
  expect_type(d$cligen_settings, "list")
  expect_type(d$station,         "list")
  expect_type(d$location,        "list")
  expect_type(d$averages,        "list")
  expect_type(d$precip,          "list")
  expect_type(d$breakpoints,     "list")

  expect_s3_class(d$cligen_version,  "data.frame")
  expect_s3_class(d$cligen_settings, "data.frame")
  expect_s3_class(d$station,         "data.frame")
  expect_s3_class(d$location,        "data.frame")
  expect_s3_class(d$averages,        "data.frame")
  expect_s3_class(d$precip,          "data.frame")
  expect_s3_class(d$breakpoints,     "data.frame")
})
