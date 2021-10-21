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
fit_configuration_set <- function (x, configuration_set, ...) {
  UseMethod("fit_configuration_set", x)
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
#' fit_group_network(x, f_set)
#' @export
fit_configuration_set.default <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  group_index, group_size, weights, parallel = FALSE, ...
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
      x           = w_list, 
      fun         = "fit_configuration_set_solver", 
      parallel    = parallel,
      f_set       = configuration_set,
      ties.method = ties.method,
      ...
    ))
  } else {
    w <- adjacency_matrix(
      x,
      group_size  = group_size,
      weights     = weights,
      ...
    )
    return(fit_configuration_set_solver(w, configuration_set, ties.method, ...))
  }
}

#' @export
fit_configuration_set.list <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  group_size, weights, parallel = FALSE, ...
) {
  w_list <- adjacency_matrix_set.list(x, group_size, weights, ...)
  pb_lapply(
    x           = w_list, 
    fun         = "fit_configuration_set_solver", 
    parallel    = parallel,
    f_set       = configuration_set,
    ties.method = ties.method,
    ...
  )
}

fit_configuration_set.igraph <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  attrname, ...
) {
  w <- adjacency_matrix.igraph(x, attrname, ...)
  fit_configuration_set_solver(w, configuration_set, ties.method, ...)
}


#' @describeIn configuration_fit Group networks from \code{network} objects
#' @param attrname
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_configuration_set.network <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  attrname, ...
) {
  w <- adjacency_matrix.network(x, attrname, ...)
  fit_configuration_set_solver(w, configuration, ties.method, ...)
}

fit_configuration_set_solver <- function (x, f_set, ties.method, ...) {
  f_set <- as.configuration_set(f_set)
  f_set <- filter_by_attribute(f_set, list(group_size = nrow(x)))
  type  <- get_attribute(f_set, "type")
  utype <-  unique(type)
  if (length(utype) == 1) {
    
    return(solve_fit_by_type(
      w           = w, 
      f_set       = f_set[type == utype], 
      ties.method = ties.method,
      ...
    ))
  } else {
    
    return(list(
      binary = solve_fit_by_type(
        w           = w, 
        f_set      = f_set[type == "binary"], 
        ties.method = ties.method,
        ...
      ),
      weighted = solve_fit_by_type(
        w           = w, 
        f_set      = f_set[type == "weighted"], 
        ties.method = ties.method,
        ...
      )
    ))
  }
}

solve_fit_by_type <- function (w, f_set, ties.method, ...) {
  if (length(f_set) == 0) {
    fit_set <- list(make_configuration_fit(
      x                = w,
      configuration_id = NULL,
      fit              = NULL,
      score            = NULL,
      potential        = NULL,
      lp_structure     = NULL,
      ROI_obj          = NULL,
      duration         = 0,
      solver           = NA
    ))
  } else {
    prescore  <- sapply(f_set, presolve_potential_score, w = w, ...)
    maximize  <- (get_attribute(f_set[[1]], "type") == "binary")
    order_ndx <- order(prescore, decreasing = maximize)
    fit_set   <- list(fit_configuration_solver(w, f_set[[order_ndx[1]]], ...))
    scores    <- fit_set[[1]]$score
    i         <- 2
    while(i <= length(order_ndx)) {
      if (maximize) {
        if (max(scores) > prescore[order_ndx[i]]) 
          break
      } else {
        if (min(scores) < prescore[order_ndx[i]]) 
          break
      }
      fit_set[[i]] <- fit_configuration_solver(w, f_set[[order_ndx[i]]], ...)
      scores[i] <- fit_set[[i]]$score
      i <- i + 1   
    }
    if (maximize) {
      fit_set <- as.configuration_fit_set(fit_set[scores == max(scores)])   
    } else {
      fit_set <- as.configuration_fit_set(fit_set[scores == min(scores)])   
    }
  }
  switch(
    ties.method,
    all    = fit_set,
    first  = fit_set[[1]],
    last   = fit_set[[length(fit_set)]],
    random = fit_set[[sample.int(length(fit_set), 1)]]
  )
}


