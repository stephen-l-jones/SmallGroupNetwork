
library(SmallGroupNetwork)

test_that("fit_group_network", {
  w = matrix(c(0,-0.4,-0.6,-1.0,-0.2,0,1.2,-1.0,-1.2,-.1,0,0.4,-1.2,-0.8,0.5,0), 4, 4)
  f = set_configuration_ids(list(
    star(2:4),
    subgroup_all(4, relation = "between"), 
    configuration(add_component(c(star(2), star(2))), description = "2 dyads")#,
    #configuration(subgroup(4), description = "team")
  ))
  y = fit_group_network(w, f, solver = "lpsolve")
  attr(w, "group_name") = "G1"
  expect_equal(names(y), "G1")
  expect_equal(y[[1]]$x, w)
  expect_equal(y[[1]]$configuration_id, attr(f[[2]], "id"))
  fit = f[[2]][c(4,3,1,2),c(4,3,1,2)]
  attributes(fit) = c(
    attributes(fit)[names(attributes(fit)) %in% c("dim","dimnames")],
    attributes(f[[2]])[!(names(attributes(f[[2]])) %in% c("dim","dimnames"))]
  )
  expect_equal(y[[1]]$fit, fit)
  expect_equal(y[[1]]$score, 8.4)
  expect_equal(y[[1]]$potential, 8.6)
  
  w = list(matrix(c(0,-0.4,-0.6,-1.0,-0.2,0,1.2,-1.0,-1.2,-.1,0,0.4,-1.2,-0.8,0.5,0), 4, 4), 
           matrix(c(0,0.5,0.3,1.2,-0.5,0,1.1,-1.5,-1.9,0.1,0,-0.5,1.2,-0.1,0.4,0), 4, 4))
  y = fit_group_network(w, f)
  attr(w, "group_name") = "G1"
  expect_equal(names(y), "G1")
  expect_equal(y[[1]]$x, w)
  expect_equal(y[[1]]$configuration_id, attr(f[[2]], "id"))
  fit = f[[2]][c(4,3,1,2),c(4,3,1,2)]
  attributes(fit) = c(
    attributes(fit)[names(attributes(fit)) %in% c("dim","dimnames")],
    attributes(f[[2]])[!(names(attributes(f[[2]])) %in% c("dim","dimnames"))]
  )
  expect_equal(y[[1]]$fit, fit)
  expect_equal(y[[1]]$score, 8.4)
  expect_equal(y[[1]]$potential, 8.6)
  
  set.seed(10001)
  w = round(matrix(rnorm(64, -.5), 8, 8), 1)
  diag(w) = 0
  f = set_configuration_ids(list(
    star(3:8),
    subgroup_all(2:5),
    subgroup_all(2:6, relation = "between")
  ))
  y = fit_group_network(w, f, solver = "gurobi")
  attr(y$G1, "duration")
  y = lapply(f, fit_group_network, x = w, solver = "gurobi")
  rowSums(lapply(y, function(a) attr(a$G1, "duration")))
  
  
})