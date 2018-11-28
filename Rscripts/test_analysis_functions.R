require(testthat)
# Set the working directory to the Rscripts folder: setwd("pathTo/Rscripts")
source("analysis_functions.R")

# Run: test_file("test_analysis_functions.R")

# get.average.over.unique.values
test_that("unique value in vec1", {
  vec1 = c(rep(1,10))
  vec2 = c(rep(2,10))
  names.vec = c(seq(1,10))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get.average.over.unique.values(vec1, vec2)
  expected.res = cbind(1,2,0)
  colnames(expected.res) = c("vec1","vec2","sd")

  expect_equal(res, expected.res)
})

test_that("2 unique values in vec1", {
  vec1 = c(rep(c(1,2),5))
  vec2 = c(rep(2,5), rep(3,5))
  names.vec = c(seq(1,10))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get.average.over.unique.values(vec1, vec2)
  expected.res = cbind(c(1,2), c(2.4, 2.6), c(0.5477226, 0.5477226))
  colnames(expected.res) = c("vec1", "vec2", "sd")

  expect_equal(res, expected.res, tolerance = .0000001)
})

test_that("3 unique values out of 4 in vec1", {
  vec1 = c(1, 2, 3, 2)
  vec2 = c(20, 2, 2.5, 8)
  names.vec = c(seq(1,4))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get.average.over.unique.values(vec1, vec2)
  expected.res = cbind(c(1,2,3), c(20,5,2.5), c(0,4.2426407,0))
  colnames(expected.res) = c("vec1", "vec2", "sd")

  expect_equal(res, expected.res, tolerance = .00000001)
})

# get.percentage.of.matches

test_that("returns proper results", {
  a = c(1,1,1,1)
  b = c(1,0,1,1)
  c = c(0,0,1,1)
  d = c(1,0,0,0)
  e = c(0,0,0,0)

  res.1 = get.percentage.of.matches(a, a)
  res.2 = get.percentage.of.matches(a, b)
  res.3 = get.percentage.of.matches(a, c)
  res.4 = get.percentage.of.matches(a, d)
  res.5 = get.percentage.of.matches(a, e)

  expected.res.1 = 1
  expected.res.2 = 0.75
  expected.res.3 = 0.5
  expected.res.4 = 0.25
  expected.res.5 = 0

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
  expect_equal(res.4, expected.res.4)
  expect_equal(res.5, expected.res.5)
})
