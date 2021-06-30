#skips the test if not on Linux OS
skip_if_not((WEPPR::is_linux() == "TRUE"),
            message="Operating system is not Linux")
#skips the test if the WEPP executable is not available
skip_if_not((WEPPR::is_wepp_available() == "TRUE"),
            message="WEPP executable not found")

#proceeds only on Linux OS
run_file <- system.file("extdata", "071000090603_2.run", package="WEPPR")
man_file <- system.file("extdata", "071000090603_2.man", package="WEPPR")
slp_file <- system.file("extdata", "071000090603_2.slp", package="WEPPR")
sol_file <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
cli_file <- system.file("extdata", "092.63x040.90.cli", package="WEPPR")
input_files <- c(man_file,slp_file,sol_file,cli_file)
copy.file(c(input_files,run_file),tempdir())

library("tools")

test_that("Correct extension",
          {expect_equal(tools::file_ext(run_file)=="run",TRUE)})
test_that("Correct type",{
  expect_error(d <- run_wepp(run_file), NA)
  expect_named(d, c("file_name", "file_type", "type","md5sum", "Id"))

  expect_type(d, "list")
  expect_s3_class(d, "data.frame")

  expect_type(d$file_name, "character")
  expect_type(d$file_type, "character")
  expect_type(d$type, "character")
  expect_type(d$md5sum, "character")
  expect_type(d$Id, "character")
})
