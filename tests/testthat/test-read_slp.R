slp_file <- system.file("extdata", "071000090603_2.slp", package="WEPPR")

test_that("no error", {
  expect_error(read_slp(slp_file), NA)
})
