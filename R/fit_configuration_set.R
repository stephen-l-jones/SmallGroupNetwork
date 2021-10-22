#' @rdname fit_configuration
#' @description 
#' \code{fit_configuration_set} selects configurations from a set that 
#' best fit the given group networks. 
#' @param configuration_set
#' A \code{\link{configuration_set}} object.
#' @return
#' For a given group network, the \code{fit_configuration_set} returns a \code{
#' \link{configuration_fit_set}} object with all configurations that best fit the 
#' group network when \code{ties.method = "all"}. Otherwise it returns a \code{
#' configuration_fit} object.
#' @details 
#' For \code{fit_configuration_set}, group networks can be different sizes. The 
#' function will only fit configurations in the set that are the same size as a 
#' group network.
#' 
#' Fitting is done using a naive algorithm or using a linear program (LP) through 
#' the \code{ROI} package. The naive algorithm is written in C++ to improve
#' performance and often is faster than LP solvers. To use an LP solver, pass the 
#' solver name and any solver parameters through \code{...} to the \code{
#' \link{ROI_solve}} function (e.g., \code{solver = "glpk"}). When parallel 
#' processing, also pass the solver package name and the associated \code{
#' ROI.plugin.*} package name through \code{...} using a \code{"packages"} 
#' parameter (e.g., \code{packages = c("Rglpk","ROI.plugin.glpk")}). 
#' 
#' See \code{\link{SmallGroupNetwork}} for details of how configurations are fit
#' to group networks. 
#' @seealso \code{\link{configuration}}, \code{\link{configuration_set}}
#' @export
fit_configuration_set <- function (x, configuration_set, ...) {
  UseMethod("fit_configuration_set", x)
}

#' @describeIn fit_configuration Fit configuration set to group network(s) from 
#' adjacency matrix or edge list
#' @param group_index
#' Vector of unique identifiers for each group network in \code{x} when \code{x} is
#' an edge list. The length of \code{group_index} should match the number of rows 
#' in \code{x}. Providing a group index indicates to the function that \code{x} is 
#' an edge list. If x is a list, \code{group_index} should also be a list of 
#' the same length, with each element corresponding to the elements in \code{x}.
#' @param group_size
#' Numeric vector of group size(s) when \code{x} is an edge list. \code{group_size} 
#' must match the group size of \code{configuration}. Length of \code{group_size} 
#' should equal the number of rows in \code{x}. (Group size is repeated 
#' for all edge list rows in a group.) If \code{x} is a list of edge lists, \code{
#' group_size} should also be a list of the same length.
#' @param ties.method
#' When \code{ties.method = "all"}, the function will return a \code{
#' configuration_fit_set} object for a given group network. When \code{ties.method} 
#' is \code{"first"}, \code{"last"}, or \code{"random"}, the function will return 
#' the first, last, or a random \code{configuration_fit} from a set.
#' @param parallel
#' Use parallel processing. Parallel processing can only be used when multiple group 
#' networks are being fit. To explicitly set the number of cores used, pass the
#' number through \code{...} using a \code{"cores"} parameter (e.g., \code{
#' cores = 4}).
#' @examples
#' 
#' f_set <- c(
#'   star(4),
#'   subgroup_all(4, relation = "between"),
#'   subgroup(4),
#'   star(4:5),
#'   subgroup_all(4:5, relation = "between"),
#'   subgroup(4:5)
#' )
#' set.seed(102)
#' x <- list(
#'   matrix(rnorm(16), 4),
#'   matrix(rnorm(25), 5)
#' )
#' fit_group_network(x, f_sets)
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

#' @describeIn fit_configuration Fit configuration set to group networks from list
#' of networks
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

#' @describeIn fit_configuration Fit configuration set to group network from 
#' \code{igraph} object
#' @export
fit_configuration_set.igraph <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  attrname, ...
) {
  w <- adjacency_matrix.igraph(x, attrname, ...)
  fit_configuration_set_solver(w, configuration_set, ties.method, ...)
}


#' @describeIn fit_configuration Fit configuration set to group network from 
#' \code{network} object
#' @export
fit_configuration_set.network <- function (
  x, configuration_set, ties.method = c("all","first","last","random"),
  attrname, ...
) {
  w <- adjacency_matrix.network(x, attrname, ...)
  fit_configuration_set_solver(w, configuration, ties.method, ...)
}

fit_configuration_set_solver <- function (w, f_set, ties.method, ...) {
  f_set <- as.configuration_set(f_set)
  f_set <- filter_by_attribute(f_set, list(group_size = nrow(w)))
  type  <- get_attribute(f_set, "type")
  utype <-  unique(type)
  if (length(utype) <= 1) {
    
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
        f_set       = f_set[type == "binary"], 
        ties.method = ties.method,
        ...
      ),
      weighted = solve_fit_by_type(
        w           = w, 
        f_set       = f_set[type == "weighted"], 
        ties.method = ties.method,
        ...
      )
    ))
  }
}

solve_fit_by_type <- function (
  w, f_set, ties.method = c("all","first","last","random"), ...
) {
  ties.method <- match.arg(ties.method)
  if (length(f_set) == 0) {
    warning(sprintf("No configurations for group network of size %s", nrow(w)))
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


