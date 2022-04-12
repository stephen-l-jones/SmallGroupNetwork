library(SmallGroupNetwork)


test_that("fit_configuration error", {
  expect_error(fit_configuration(), 'argument "x" is missing, with no default')
  x  <- matrix(c(0,1,1,0), 2, 2)
  expect_error(fit_configuration(x),'argument "configuration" is missing, with no default')
  f <- star(3)
  expect_error(fit_configuration(x, f),'The group network must be the same size as the configuration.')
  
})
test_that("fit_configuration inputs", {
  
  # 1x2 edgelist, binary, no loops
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
  
  # 1x2 edgelist, weighted, no loops
  el <- cbind(1, 2)
  w  <- 1.5
  f  <- star(2, value = 2L)[[1]]
  y  <- fit_configuration(el, f, weights = w)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("weighted"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,0,1.5,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 2.5)
  
  # 1x2 edgelist, binary, loops
  el <- cbind(1, 1)
  f  <- star(2, loops = TRUE)[[1]]
  y  <- fit_configuration(el, f, group_size = 2, loops = TRUE)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(1,0,0,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, -1)
  
  # 1x1 adjacency, binary, loops
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
  
  # 2x2 adjacency, binary, no loops
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
  
  # 2x2 adjacency, weighted, no loops
  x  <- matrix(c(0,1,2,0), 2, 2)
  f  <- configuration(matrix(c(0,0,1,0), 2, 2), type = "weighted")
  y  <- fit_configuration(x, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("weighted"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,1,2,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, 2)
  
  # 2x2 adjacency, binary, loops
  x  <- matrix(c(.1,1,1,0), 2, 2)
  f  <- configuration(matrix(c(1,0,1,0), 2, 2), loops = TRUE)
  y  <- fit_configuration(x, f, loops = TRUE)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(.1,1,1,0), 2, dimnames = list(1:2, 1:2)))
  expect_equal(y$fit, f)
  expect_equal(y$score, .1)
  
  # 2x2 edgelist, binary, no loops
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
  
  # nxn adjacency, binary, no loops
  x  <- matrix(c(0,1,2,3,1,0,2,3,1,2,0,3,1,2,3,0), 4, 4)
  f  <- star(4)[[1]]
  y  <- fit_configuration(x, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(as.vector(x), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(2,3,4,1),c(2,3,4,1)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(2,3,4,1)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 6)
  
  # nxn adjacency, weighted, no loops
  x  <- matrix(c(0,1,2,3,1,0,2,3,1,2,0,3,1,2,3,0), 4, 4)
  f  <- star(4, value = 2L)[[1]]
  f[]  <- f + star(4)[[1]][c(2,1,3,4),c(2,1,3,4)]
  y  <- fit_configuration(x, f)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("weighted"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(as.vector(x), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(3,4,1,2),c(3,4,1,2)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(3,4,1,2)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 8)
  
  # nxn adjacency, binary, loops
  x  <- matrix(c(0,1,2,-3,1,1,2,-3,1,2,0,-3,1,-2,3,0), 4, 4)
  f  <- star(4, loops = TRUE)[[1]]
  f[2, 2] <- 1
  y  <- fit_configuration(x, f, loops = TRUE)
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(as.vector(x), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(3,2,1,4),c(3,2,1,4)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(3,2,1,4)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 13)
  
  # nx2 edgelist, binary, no loops
  x  <- matrix(c(1,2,2,1,3,4,2,4), 4, 2, byrow = TRUE)
  f  <- star(4)[[1]]
  y  <- fit_configuration(x, f, group_size = 4, weights = c(.5, .5, -1, -1))
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,.5,0,0,.5,rep(0,8),-1,-1,0), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(1,3,4,2),c(1,3,4,2)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(1,3,4,2)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 3)
  
  # nxn igraph, binary, loops
  x  <- matrix(c(0,1,2,-3,1,1,2,-3,1,2,0,-3,1,-2,3,0), 4, 4)
  x.igraph <- igraph::graph_from_adjacency_matrix(x, weighted = TRUE)
  f  <- star(4, loops = TRUE)[[1]]
  f[2, 2] <- 1
  y  <- fit_configuration(x.igraph, f, loops = TRUE, attrname = "weight")
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(as.vector(x), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(3,2,1,4),c(3,2,1,4)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(3,2,1,4)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 13)
  
  # nxn network, binary, loops
  x  <- matrix(c(0,1,2,-3,1,1,2,-3,1,2,0,-3,1,-2,3,0), 4, 4)
  x.network <- network::network(x, loops = TRUE, ignore.eval=FALSE, names.eval='weight')
  f  <- star(4, loops = TRUE)[[1]]
  f[2, 2] <- 1
  y  <- fit_configuration(x.network, f, loops = TRUE, attrname = "weight")
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), TRUE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(as.vector(x), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(3,2,1,4),c(3,2,1,4)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(3,2,1,4)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 13)
  
  # multiteam edgelist, binary, no loops
  x  <- matrix(c(1,2,2,1,3,4,2,4), 4, 2, byrow = TRUE)
  f  <- star(4)[[1]]
  y  <- fit_configuration(x, f, group_index = c(1,1,2,2), group_size = 4, weights = c(.5, .5, -1, -1))
  expect_equal(class(y), c("configuration_fit"))
  expect_equal(get_attribute(y, "type"), c("binary"))
  expect_equal(get_attribute(y, "loops"), FALSE)
  expect_equal(get_attribute(y, "solver"), "naive")
  expect_equal(y$x, matrix(c(0,.5,0,0,.5,rep(0,8),-1,-1,0), 4, dimnames = list(1:4, 1:4)))
  f[] <- f[c(1,3,4,2),c(1,3,4,2)]
  dimnames(f) <- lapply(dimnames(f), function(x) x[c(1,3,4,2)])
  expect_equal(y$fit, f)
  expect_equal(y$score, 3)
})
