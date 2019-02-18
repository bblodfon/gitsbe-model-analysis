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

# get.biomarkers
test_that("it returns proper results", {
  m = matrix(0, 5, 5)
  set.seed(1)
  diff.res = apply(m, c(1, 2), function(x) runif(n = 1, min = -1, max = 1))
  colnames(diff.res) = c("a", "b", "c", "d", "e")

  res.1 = get.biomarkers(diff.res, threshold = 0.87, type = "active")
  expected.res.1 = c("b", "d")

  res.2 = get.biomarkers(diff.res, threshold = 0.7, type = "inhibited")
  expected.res.2 = c("b", "e")

  res.3 = get.biomarkers(diff.res, threshold = 0.99, type = "active")
  expected.res.3 = character(0)

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
})

# prune.columns.from.df
test_that("it returns proper results", {
  df.1 = data.frame(c(0,0,0), c(0,1,0), c(1,0,0), c(0,0,0))
  df.2 = data.frame(c(2,2,2), c(2,1,0), c(1,0,1), c(1,1,1))

  res.1 = prune.columns.from.df(df.1, value = 0)
  res.2 = prune.columns.from.df(df.2, value = 2)
  res.3 = prune.columns.from.df(res.2, value = 1)

  expected.res.1 = data.frame(c(0,1,0), c(1,0,0))
  expected.res.2 = data.frame(c(2,1,0), c(1,0,1), c(1,1,1))
  expected.res.3 = data.frame(c(2,1,0), c(1,0,1))

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
})

# prune.rows.from.df
test_that("it returns proper results", {
  df.1 = data.frame(c(0,0,0), c(0,1,0), c(1,0,0), c(0,0,0))
  df.2 = data.frame(c(2,2,1), c(0,2,1))
  colnames(df.1) = 1:length(colnames(df.1))
  colnames(df.2) = 1:length(colnames(df.2))

  res.1 = prune.rows.from.df(df.1, value = 0)
  res.2 = prune.rows.from.df(df.2, value = 2)
  rownames(res.2) = 1:length(rownames(res.2))
  res.3 = prune.rows.from.df(res.2, value = 1)
  res.4 = prune.rows.from.df(res.3, value = 10)

  expected.res.1 = data.frame(c(0,0), c(0,1), c(1,0), c(0,0))
  expected.res.2 = data.frame(c(2,1), c(0,1))
  expected.res.3 = data.frame(c(2), c(0))
  expected.res.4 = expected.res.3
  colnames(expected.res.1) = 1:length(colnames(expected.res.1))
  colnames(expected.res.2) = 1:length(colnames(expected.res.2))
  colnames(expected.res.3) = 1:length(colnames(expected.res.3))
  colnames(expected.res.4) = 1:length(colnames(expected.res.4))

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
  expect_equal(res.4, expected.res.4)
})

