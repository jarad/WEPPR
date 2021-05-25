wb_file <- system.file("extdata", "071000090603_2.wb", package="WEPPR")

test_that("no error", {
  expect_error(read_wb(wb_file), NA)
})
