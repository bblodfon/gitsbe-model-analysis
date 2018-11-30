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

# prune.to.common.nodes.and.reorder
test_that(paste("it correctly prunes and reorders first vector based on its ",
                "column names and the values of the second vector", sep = ""), {
  a = c(1,2,3,4,5,6)
  names(a) = c("MPAK","ADER","FEK2","AKT","OLGA","BAD")
  nodes = c("AKT","MPAK","FEK2","BAD")

  res = prune.to.common.nodes.and.reorder(a, nodes)
  expected.res = c(4,1,3,6)
  names(expected.res) = c("AKT","MPAK","FEK2","BAD")

  expect_equal(res, expected.res)
})
