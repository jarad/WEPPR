run_file <- system.file("extdata", "071000090603_2.run", package="WEPPR")

library("tools")

test_that("Correct extension",{expect_equal(tools::file_ext(run_file)=="run",TRUE)})
test_that("Correct type",{
  expect_error(d <- run_wepp(run_file), NA)
  expect_named(d, c("Input_files", "Output_files"))

  expect_type(d, "list")
  expect_s3_class(d, "data.frame")

  expect_type(d$File_name, "character")
  expect_type(d$File_extension, "character")
  expect_type(d$File_type, "character")
  expect_type(d$Type, "character")
  expect_type(d$md5sum, "character")
  expect_type(d$Id, "character")
})
