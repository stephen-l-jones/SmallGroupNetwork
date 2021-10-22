library(SmallGroupNetwork)

test_that("absdiffNA", {
  expect_equal(SmallGroupNetwork:::absdiffNA(c(NA, rep(2, 3)), c(3:1, NA)), c(0,0,1,0)) 
})

test_that("prodNA", {
  expect_equal(SmallGroupNetwork:::prodNA(c(NA, rep(2, 3)), c(3:1, NA)), c(0,4,2,0)) 
})

test_that("expand_int_matrix", {
  expect_equal(SmallGroupNetwork:::expand_int_matrix(c(NA,1), 1:2, 3), matrix(c(NA,1,NA,1,1,1,2,2,3,3,3,3), 4, 3))
})

test_that("mbind", {
  expect_equal(
    SmallGroupNetwork:::mbind(
      F = matrix(1:6, 3, 2, dimnames = list(LETTERS[1:3], LETTERS[4:5])),
      matrix(c(NA,1))
    ),
    array(c(1:6,rep(c(NA,1), 3)), c(3, 2, 2), dimnames = list(c("A","B","C"),c("D","E"),c("F","")))
  )
  expect_equal(
    SmallGroupNetwork:::mbind(matrix(1:6, 3, 2, dimnames = list(LETTERS[1:3], LETTERS[4:5]))),
    array(1:6, c(3,2,1), dimnames = list(c("A","B","C"),c("D","E"),NULL))
  )
  expect_equal(dim(SmallGroupNetwork:::mbind(integer(0))), c(0,1,1))
})

