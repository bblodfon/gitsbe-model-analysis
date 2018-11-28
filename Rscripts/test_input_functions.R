require(testthat)
# Set the working directory to the Rscripts folder: setwd("pathTo/Rscripts")
source("input_functions.R")

# Run: test_file("test_input_functions.R")

# remove.commented.and.empty.lines
test_that("it removes commented lines", {
  lines = c("aaa", "#comment1", "bbb", "#comment2", "ccc")

  res = remove.commented.and.empty.lines(lines)
  expected.res = c("aaa", "bbb", "ccc")

  expect_equal(res, expected.res)
})

test_that("it removes empty lines", {
  lines = c("aaa", "", "bbb", "    ", "ccc", "\t  \t")

  res = remove.commented.and.empty.lines(lines)
  expected.res = c("aaa", "bbb", "ccc")

  expect_equal(res, expected.res)
})

test_that("it removes both commented and empty lines", {
  lines = c("aaa", "#comment1", "\t", "#comment2", "ccc", "")

  res = remove.commented.and.empty.lines(lines)
  expected.res = c("aaa", "ccc")

  expect_equal(res, expected.res)
})