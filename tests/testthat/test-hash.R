files <- system.file("extdata",
                        c("092.63x040.90.cli",
                          "071000090603_2.env",
                          "071000090603_2.man",
                          "071000090603_2.run",
                          "071000090603_2.slp",
                          "071000090603_2.sol",
                          "071000090603_2.wb",
                          "071000090603_2.yld"),
                        package="WEPPR")

test_that("correct md5sum", {
  expect_equal(tools::md5sum(files), hash(files))
})


