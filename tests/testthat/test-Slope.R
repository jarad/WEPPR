sol_file <- system.file("extdata", "071000090603_2.sol", package="WEPPR")

test_that("propotion values are all between range 0 and 1", {
  slp <- read_slp(slp_file)

  # manipulate data frame to include values outside of the range
  slp$p[slp$p == 1.000000 & slp$n == 1] <- 2
  slp$p[slp$p == 1.000000 & slp$n == 2] <- -1

  expect_error(read_slp(slp), "OFEs are not in nondimensional distances")
})

test_that("no negative values present in slope file", {
  slp <- read_slp(slp_file)
  # manipulate data frame to include negative values
  slp_w_neg <- data.frame(read_slp(slp_file))
  slp_w_neg$slope[slp_w_neg$slope == 0.040870] <- -0.01

  expect_error(new_Slope(slp_w_neg), "Negative values exist in slope data")
})

test_that("all OFE slopes are matching", {
  slp <- read_slp(slp_file)
  # create data frame with non-matching slope
  bad_slp <- data.frame(read_slp(slp_file))
  bad_slp$slope[bad_slp$slope == 0.040864 & bad_slp$n == 2] <- -0.01

  expect_error(new_Slope(bad_slp), "Slope does not match across OFE")
})
