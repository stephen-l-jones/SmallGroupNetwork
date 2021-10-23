library(SmallGroupNetwork)

test_that("fit_configuration", {
  
  # 1x2 edgelist
  el <- cbind(1, 2)
  f  <- star(2)[[1]]
  y  <- fit_configuration(el, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,0,1,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 1)
  
  # 1x1 adjacency
  x  <- 1
  f  <- configuration(1L, loops = TRUE)
  y  <- fit_configuration(x, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(1, dimnames = list(1, 1)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 1)
  
  # 2x2 adjacency
  x  <- matrix(c(0,1,1,0), 2, 2)
  f  <- configuration(matrix(c(0,0,1,0), 2, 2))
  y  <- fit_configuration(x, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,1,1,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 0)
  
  # 2x2 edgelist
  x  <- matrix(c(1,2,2,1), 2, 2)
  f  <- configuration(matrix(c(1,2,2,1), 2, 2), group_size = 2)
  y  <- fit_configuration(x, f, group_size = 2, weights = c(.5, .5))
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,.5,.5,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 1)
  

})
