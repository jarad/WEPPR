yld_file <- system.file("extdata", "071000090603_2.yld", package="WEPPR")

test_that("no error", {
  expect_error(read_yld(yld_file), NA)
})
