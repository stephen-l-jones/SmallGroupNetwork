#' Fit configuration or configuration set to group networks
#'
#' \code{fit_configuration} fits a configuration to one or more group networks.
#'
#' @param x
#' A group network or list of group networks. A group network can be an adjacency 
#' matrix, edge list, \code{igraph} object, or \code{network} object. \code{x} may 
#' also be an edge list for multiple groups when \code{group_index} is provided.
#' @param configuration
#' A \code{\link{configuration}} object.
#' @return
#' \code{fit_configuration} returns a \code{\link{configuration_fit}} object.
#' @details
#' \code{fit_configuration} fits a single configuration to one or more group
#' networks. All group networks must be the same size as the configuration.
#' @export
fit_configuration <- function (x, configuration, ...) {
  UseMethod("fit_configuration", x)
}

#' @describeIn fit_configuration Fit configuration to group network(s) from 
#' adjacency matrix or edge list
#' @param group_index
#' Vector of unique identifiers for each group network in \code{x} when \code{x} is
#' an edge list. The length of \code{group_index} should match the number of rows 
#' in \code{x}. Providing a group index indicates to the function that \code{x} is 
#' an edge list. If x is a list, \code{group_index} should also be a list of 
#' the same length, with each element corresponding to the elements in \code{x}.
#' @param weights
#' Numeric vector of edge weights when \code{x} is an edge list. Length should equal 
#' the number of rows in \code{x}. If x is a list, \code{weights} should also be a 
#' list of the same length, with each element corresponding to the elements in 
#' \code{x}.
#' @examples
#' 
#' f <- star(4)[[1]]
#' set.seed(102)
#' x <- matrix(rnorm(16), 4)
#' fit_configuration(x, f)
#' 
#' f <- add_component(c(star(3, value = 2L),star(3))) 
#' el <- cbind(c(1,1,2,3), c(2,3,3,4))
#' w <- runif(4, 0, 3)
#' fit_configuration(el, f, group_size = 6, weights = w)
#' @export
fit_configuration.default <- function (
  x, configuration, group_index, group_size, weights, parallel = FALSE, ...
) {
  x             <- as.matrix(x)
  configuration <- as.configuration(configuration)
  if (missing(group_size)) {
    group_size <- NULL
  }
  if (missing(weights)) {
    weights <- NULL
  }
  if (!missing(group_index) && !is.null(group_index)) {
    if (length(group_index) == 1) {
      group_index <- rep(group_index, nrow(x))
    }
    w_list <- adjacency_matrix_set_from_edgelist(
      x, 
      group_index = group_index, 
      group_size  = rep(group_size, nrow(x)),
      weights     = weights
    )
    return(pb_lapply(
      x        = w_list, 
      fun      = "fit_configuration_solver", 
      parallel = parallel,
      f        = configuration,
      ...
    ))
  } else {
    w <- adjacency_matrix(
      x,
      group_size  = group_size,
      weights     = weights,
      ...
    )
    return(fit_configuration_solver(w, configuration, ...))
  }
}

#' @describeIn fit_configuration Fit configuration to group networks from list of 
#' networks
#' @export
fit_configuration.list <- function (
  x, configuration, group_size, weights, parallel = FALSE, ...
) {
  w_list <- adjacency_matrix_set.list(x, group_size, weights, ...)
  pb_lapply(
    x        = w_list, 
    fun      = "fit_configuration_solver", 
    parallel = parallel,
    f        = configuration,
    ...
  )
}

#' @describeIn fit_configuration Fit configuration to group network from 
#' \code{network} object
#' @param attrname
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_configuration.igraph <- function (x, configuration, attrname, ...) {
  w <- adjacency_matrix.igraph(x, attrname, ...)
  fit_configuration_solver(w, configuration, ...)
}


#' @describeIn fit_configuration Fit configuration to group network from 
#' \code{network} object
#' @export
fit_configuration.network <- function (x, configuration, attrname, ...) {
  w <- adjacency_matrix.network(x, attrname, ...)
  fit_configuration_solver(w, configuration, ...)
}

fit_configuration_solver <- function (w, f, solver = "naive", ...) {
  f <- as.configuration(f)
  if (!all(dim(w) == dim(f))) 
    stop("The group network must be the same size as the configuration.")
  if (solver == "naive") {
    return(permute_solve_fit(w, f, ...))
  } else {
    return(lp_solve_fit(w, f, solver, ...))
  }
}
