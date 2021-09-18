library(SmallGroupNetwork)

test_that("star", {
  out = star(2)
  exp = matrix(c(NA,1,1,NA), 2, 2, dimnames = list(1:2, 1:2))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "binary", 
    loops = FALSE, 
    group_size = 2, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  out = star(2:4)
  expect_equal(length(out), 3)
  expect_equal(sapply(out, attr, which = "group_size"), c(4,4,4))
  expect_equal(sapply(out, attr, which = "id"), 1:3)
  expect_equal(sapply(out, attr, which = "description"), paste(2:4, "star"))
  
  out = star(2, loops = TRUE)
  exp = matrix(c(0,1,1,0), 2, 2, dimnames = list(1:2, 1:2))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "binary", 
    loops = TRUE, 
    group_size = 2, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  out = star(2, value = 2)
  exp = matrix(c(NA,2,2,NA), 2, 2, dimnames = list(1:2, 1:2))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "weighted", 
    loops = FALSE, 
    group_size = 2, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  out = star(2, mode = "in")
  exp = matrix(c(NA,1,0,NA), 2, 2, dimnames = list(1:2, 1:2))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "binary", 
    loops = FALSE, 
    group_size = 2, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  out = star(2, mode = "out")
  exp = matrix(c(NA,0,1,NA), 2, 2, dimnames = list(1:2, 1:2))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "binary", 
    loops = FALSE, 
    group_size = 2, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  out = star(2, group_size = 3)
  exp = matrix(c(NA,1,0,1,NA,0,0,0,NA), 3, 3, dimnames = list(1:3, 1:3))
  attributes(exp) = list(
    dim = attr(exp, "dim"),
    dimnames = attr(exp, "dimnames"),
    class = c("configuration","matrix"),
    description = "2 star", 
    type = "binary", 
    loops = FALSE, 
    group_size = 3, 
    id = 1
  )
  exp = list(exp)
  class(exp) = c("configuration_list", "list")
  expect_equal(out, exp)
  
  
  expect_error(star(2, group_size = 1))
  expect_error(star(1))
})


