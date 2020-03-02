test_that("correct Kb calculation", {
  sand = 35; clay = 35; cec = 2
  expect_equal(calculate_Kb(sand, clay, cec), -.265+.0086*sand^1.8+11.46*cec^-.75)
  expect_equal(calculate_Kb(sand, clay+10, cec), .0066*exp(244/(clay+10) ))
})
