#' Fit configuration to group network
#'
#' \code{fit_configuration} fits a configuration to group networks and returns
#' a \code{configuration_fit} object.
#'
#' @param x
#' A group network or list of group networks. A group network can be an adjacency
#' matrix, edge list, \code{igraph} object, or \code{network} object. \code{x} may
#' also be an edge list for multiple groups when \code{group_index} is provided.
#' @param configuration
#' A \code{configuration} object.
#' @return
#' A \code{configuration_fit} object.
#' @details
#' \code{fit_configuration} fits a single configuration. To fit multiple
#' configurations to group networks, see \code{\link{fit_configuration_set}}.
#' @examples
#' @export
fit_configuration <- function (x, configuration, ...) {
  UseMethod("fit_configuration", x)
}

#' @describeIn fit_configuration Fit configuration to group networks from adjacency
#' matrix or edge list
#' @param weights
#' Numeric vector of edge weights for an edge list. Length should equal the number
#' of rows in \code{x}.
#' @param input_type
#' Used to specify \code{x}. Can be \code{"unspecified"}, \code{"edgelist"}, or
#' \code{"adjacency"}. If unspecified, the function will guess whether \code{x} is
#' an edge list or adjacency matrix.
#' @examples
#' f <- star(4)[[1]]
#' set.seed(102)
#' x = list(
#'   matrix(rnorm(16), 4),
#'   matrix(rnorm(25), 5)
#' )
#' fit_group_network(x, f_list)
#' @export
fit_configuration.default <- function (
  x, configuration, group_index, group_size, weights, parallel = FALSE, ...
) {
  if (missing(group_size)) {
    group_size <- NULL
  }
  if (missing(weights)) {
    weights <- NULL
  }
  if (!missing(group_index) && !is.null(group_index)) {
    w_list <- adjacency_matrix_set_from_edgelist(
      x, 
      group_index = group_index, 
      group_size  = group_size,
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

fit_configuration.igraph <- function (x, configuration, attrname, ...) {
  w <- adjacency_matrix.igraph(x, attrname, ...)
  fit_configuration_solver(w, configuration, ...)
}


#' @describeIn configuration_fit Group networks from \code{network} objects
#' @param attrname
#' Attribute name that holds the edge weights (usually \code{"weight"}).
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
