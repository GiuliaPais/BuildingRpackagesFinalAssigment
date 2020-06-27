context("Read tests")

test_that("trying to import non-existing file fails", {
  file <- system.file("extdata", "accident_2016.csv.bz2", package ="fars")
  expect_error(fars_read(file))
})

test_that("importing a proper file returns a tibble", {
  file <- system.file("extdata", "accident_2015.csv.bz2", package ="fars")
  df <- fars_read(file)
  expect_s3_class(df, "tbl_df")
})
