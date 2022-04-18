
library(SmallGroupNetwork)

test_that("configuration", {
 f = configuration(matrix(c(0,1,1,1,0,0,1,0,0), 3), description = "A")
 m = matrix(c(NA,1,1,1,NA,0,1,0,NA), 3)
 attributes(m) = list(
   dim = dim(m),
   dimnames = list(1:3, 1:3),
   class = c("configuration","matrix"),
   group_size = 3,
   type = "binary",
   loops = FALSE,
   description = "A",
   id = 0
 )
 expect_equal(f, m)
 
 f = configuration(cbind(c(1,1,2,4),c(2,3,1,3)), group_size = 5)
 m = matrix(c(NA,1,0,0,0,1,NA,0,0,0,1,0,NA,1,0,0,0,0,NA,0,0,0,0,0,NA), 5)
 attributes(m) = list(
   dim = dim(m),
   dimnames = list(1:5, 1:5),
   class = c("configuration","matrix"),
   group_size = 5,
   type = "binary",
   loops = FALSE,
   description = "",
   id = 0
 )
 expect_equal(f, m)
 
 f = configuration(matrix(c(1,1,1,1,0,0,1,0,1), 3), description = "A", loops = TRUE)
 m = matrix(c(1,1,1,1,0,0,1,0,1), 3)
 attributes(m) = list(
   dim = dim(m),
   dimnames = list(1:3, 1:3),
   class = c("configuration","matrix"),
   group_size = 3,
   type = "binary",
   loops = TRUE,
   description = "A",
   id = 0
 )
 expect_equal(f, m)
 
 f = configuration(matrix(c(1,2,2,2,0,0,1,0,1), 3), type = "weighted", id = 1)
 m = matrix(c(NA,2,2,2,NA,0,1,0,NA), 3)
 attributes(m) = list(
   dim = dim(m),
   dimnames = list(1:3, 1:3),
   class = c("configuration","matrix"),
   group_size = 3,
   type = "weighted",
   loops = FALSE,
   description = "",
   id = 1
 )
 expect_equal(f, m)
})