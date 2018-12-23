# get.average.over.unique.values
test_that("it returns proper results when there is a unique value in vec1", {
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

test_that("it returns proper results when there are 2 unique values in vec1", {
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

test_that(paste("it returns proper results when there are ",
                "3 unique values out of 4 in vec1", sep = ""), {
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
test_that("it returns proper results", {
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

  res = c(res.1, res.2, res.3, res.4, res.5)

  expected.res.1 = 1
  expected.res.2 = 0.75
  expected.res.3 = 0.5
  expected.res.4 = 0.25
  expected.res.5 = 0

  expected.res = c(expected.res.1, expected.res.2, expected.res.3,
                   expected.res.4, expected.res.5)

  expect_equal(res, expected.res)
})

# is.between
test_that("it returns proper results", {
  low.thres = -0.23456
  high.thres = 1.26346

  res.1 = is.between(value = 0, low.thres, high.thres)
  res.2 = is.between(value = -1, low.thres, high.thres)
  res.3 = is.between(value = 1, low.thres, high.thres)
  res.4 = is.between(value = low.thres, low.thres, high.thres)
  res.5 = is.between(value = low.thres, low.thres, high.thres,
                     include.high.value = TRUE)
  res.6 = is.between(value = high.thres, low.thres, high.thres)
  res.7 = is.between(value = high.thres, low.thres, high.thres,
                     include.high.value = TRUE)

  expect_true(res.1)
  expect_false(res.2)
  expect_true(res.3)
  expect_true(res.4)
  expect_true(res.5)
  expect_false(res.6)
  expect_true(res.7)
})
