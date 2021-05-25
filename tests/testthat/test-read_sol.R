sol_file <- system.file("extdata", "071000090603_2.sol", package="WEPPR")

test_that("no error", {
  expect_error(read_sol(sol_file), NA)
})
