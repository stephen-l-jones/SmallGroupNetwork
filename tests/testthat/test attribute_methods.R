library(SmallGroupNetwork)

test_that("get_attribute", {
  f = star(2:4)
  w = lapply(1:4, function(i) matrix(round(rnorm(16), 1), 4, 4))
  y = fit_configuration_set(w, f)
  
  # Configuration list
  expect_equal(get_attribute(f, "class"), matrix(c("configuration","matrix"), 2, 3))
  expect_equal(get_attribute(f, "description"), c("2 star","3 star","4 star"))
  expect_equal(get_attribute(f, "type"), rep("binary", 3))
  expect_equal(get_attribute(f, "loops"), rep(FALSE, 3))
  expect_equal(get_attribute(f, "group_size"), rep(4, 3))
  expect_equal(get_attribute(f, "id"), 1:3)
  expect_equal(get_attribute(f, "none"), list(NULL, NULL, NULL))
  expect_equal(get_attribute(list(f, f[1]), "id"), list(1:3, 1))

  # Configuration
  expect_equal(get_attribute(f[[1]], "class"), c("configuration","matrix"))
  expect_equal(get_attribute(f[[1]], "description"), "2 star")
  expect_equal(get_attribute(f[[1]], "type"), "binary")
  expect_equal(get_attribute(f[[1]], "loops"), FALSE)
  expect_equal(get_attribute(f[[1]], "group_size"), 4)
  expect_equal(get_attribute(f[[1]], "id"), 1)
  expect_equal(get_attribute(f[[1]], "none"), NULL)
  
  # Configuration set
  
  # Other
  f = list(1, 2)
  attr(f[[1]], "other") = "a"
  expect_equal(get_attribute(f, "class"), list(NULL, NULL))
  expect_equal(get_attribute(f[[1]], "other"), "a")
})

test_that("filter_by_attribute", {
  f = star(2:4)
  
  # Configuration list
  expect_equal(filter_by_attribute(f, c(class = "configuration"), as.boolean = TRUE), rep(TRUE, 3))
  f1 = f[1]
  class(f1) = c("configuration_set","list")
  expect_equal(filter_by_attribute(f, c(description = "2 star")), f1)
  expect_equal(filter_by_attribute(f, list(id = 1:2), as.boolean = TRUE), c(TRUE,TRUE,FALSE))
  expect_equal(filter_by_attribute(list(f, f[1]), list(id = 1:2), as.boolean = TRUE), list(c(TRUE,TRUE,FALSE),TRUE))
  f0 = list()
  class(f0) = c("configuration_set","list")
  expect_equal(filter_by_attribute(f, c(type = "weighted")), f0)
  expect_equal(filter_by_attribute(f, c(type = "weighted", id = 1)), f0)
  expect_equal(filter_by_attribute(f, c(type = "binary", id = 1)), f1)
  
  # Configuration
  expect_equal(filter_by_attribute(f[[1]], c(class = "configuration")), f[[1]])
  expect_equal(filter_by_attribute(f[[1]], c(description = "2 star"), as.boolean = TRUE), TRUE)
  expect_equal(filter_by_attribute(f[[1]], c(type = "weighted", id = 1)), NULL)
  expect_equal(filter_by_attribute(f[[1]], c(type = "binary", id = 1)), f[[1]])
  
  # Configuration set
  
  # Other
  f = list(1, 2)
  attr(f[[1]], "other") = "a"
  expect_equal(filter_by_attribute(f, c(other = "a")), f[1])
  expect_equal(filter_by_attribute(f[[1]], c(other = "a")), f[[1]])
})
  