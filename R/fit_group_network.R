
#' Fit configurations to group network
#' 
#' \loadmathjax 
#' \code{fit_group_network} selects a configuration from a set that best fits a group network. 
#' 
#' @param x
#' A group network or list of group networks. A group network can be an adjacency matrix, edge list,
#' \code{igraph} object, or \code{network} object. \code{x} may also be an edge list for multiple
#' groups when \code{group_index} is provided.
#' @param configuration_set
#' A \code{configuration_set} object, which is a list of configurations 
#' (see \code{\link{configuration_set}}).
#' @param all_best
#' If \code{all_best = TRUE}, returns all best-fitting configurations that fit a group 
#' network equally well (if more than one do). If \code{all_best = FALSE}, returns one best-fitting
#' configuration among those that fit a network equally well. When \code{all_best = TRUE}, 
#' performance will be slower.
#' @param parallel
#' Use parallel processing. Parallel processing can only be used when multiple group networks are 
#' being fit.
#' @param cores
#' Number of cores to use with parallel processing. If \code{parallel = TRUE}, and 
#' \code{cores = NULL}, then the function will detect the number of cores available.
#' @return
#' A list of \code{configuration_fit} objects, each of which has the following elements:
#' \itemize{
#'   \item{\code{x}: The group network (adjacency matrix) used to fit a configuration.}
#'   \item{\code{fit}: The best-fitting configuration with its rows and columns reordered to
#'   correspond with its fit to the group network.} 
#'   \item{\code{configuration_id}: The \code{id} associated with the best-fitting configuration.}
#'   \item{\code{score}: The fit score for the best-fitting configuration.}
#'   \item{\code{potential}: The highest (or lowest when \code{maximize = FALSE}) potential score 
#'   given \code{x}.}
#'   \item{\code{ROI_obj}: Returned object from the \code{\link{ROI_solve}} function.}
#' }
#' A \code{configuration_fit} object also has the following attributes:
#' \itemize{
#'   \item{\code{lp_duration}: Duration of solver function.} 
#'   \item{\code{solver}: Name of solver used.}
#' }
#' @details 
#' Configurations and group networks are represented as square adjacency matrices. A configuration 
#' must have the same dimensions as a group network to be fitted. The function 
#' maximizes the score for binary configurations and minimizes the score for weighted
#' configurations.
#' 
#' Fitting is done using a linear program and the \code{ROI} package. \code{ROI} allows for
#' different solvers to be used. The default solver is "glpk" using the \code{Rglpk} package. Other
#' solvers may be used by passing the solver name and any solver parameters through \code{...}
#' to the \code{\link{ROI_solve}} function.
#' 
#' \strong{Binary configurations.}
#' When fitting binary configurations, a group network's negative values indicate the 
#' absence of an edge and positive values indicate the presence of an edge. More negative 
#' (or positive) values in the group network give stronger evidence of the absence (or presence) 
#' of a tie. The function will attempt to match a configuration's 0-valued elements to negative 
#' network values and match a configuration's 1-valued elements to positive values. 
#' 
#' Given a binary configuration \mjeqn{f}{\emph{f}} in set \mjeqn{F}{\emph{F}} and 
#' network \mjeqn{x_g}{\emph{x_g}} for group \mjeqn{g}{\emph{g}}, a score for group 
#' \mjeqn{g}{\emph{g}} and configuration \mjeqn{f}{\emph{f}} is calculated as:
#' \mjdeqn{score_{g,f} = \sum_{i=1}^{N_g} \sum_{j=1}^{N_g} x_{g,ij} \cdot b_{ij},}{\emph{score_g,f = \sum_i \sum_j x_g,ij x b_ij,}}
#' where \mjeqn{x_{g,ij}}{\emph{x_g,ij}} is the (i, j)th element in \mjeqn{x_g}{\emph{x_g}} and
#' \mjeqn{b_{ij} = +1}{\emph{b_ij} = +1} when \mjeqn{f_{ij} = 1}{\emph{f_ij = 1}}, 
#' \mjeqn{b_{ij} = -1}{\emph{b_ij} = -1} when \mjeqn{f_{ij} = 0}{\emph{f_ij = 0}}, and 
#' \mjeqn{b_{ij} = 0}{\emph{b_ij} = 0} when \mjeqn{f_{ij}}{\emph{f_ij}} is \code{NA}. The order of 
#' rows and columns in any configuration \mjeqn{f}{\emph{f}} is arbitrary. Thus, the function 
#' determines the ordering of rows and columns in \mjeqn{f}{\emph{f}} that maximizes 
#' \mjeqn{score_{g,f}}{\emph{score_g,f}}.
#' 
#' All binary configurations in set \mjeqn{F}{\emph{F}} that have the same dimensions 
#' (\mjeqn{N_g \times N_g}{\emph{N_g x N_g}}) as \mjeqn{x_g}{\emph{x_g}} are simultaneously fit, 
#' and the function returns, the \code{id} of \mjeqn{F}{\emph{F}} for the best-fitting 
#' configuration \mjeqn{f^\ast}{\emph{f*}}. The \code{fit} matrix in the returned \code{configuration_fit} 
#' object gives the reordered rows and columns of configuration \mjeqn{f^\ast}{\emph{f*}}.
#' 
#' \strong{Weighted configurations.} When fitting weighted configurations, a group network's values 
#' indicate a level of the measured relationship. A configurations \emph{n}-valued elements 
#' are matched to a network's edges with the closest values to \emph{n}.
#' 
#' Given a weighted configuration \mjeqn{f}{\emph{f}} in set \mjeqn{F}{\emph{F}} and 
#' network \mjeqn{x_g}{\emph{x_g}}, a score for group  \mjeqn{g}{\emph{g}} and configuration 
#' \mjeqn{f}{\emph{f}} is calculated as:
#' \mjdeqn{score_{g,f} = \sum_{i=1}^{N_g} \sum_{j=1}^{N_g} \mathrm{abs}(x_{g,ij} - f_{ij}) \cdot d_{ij},}{\emph{score_g,f = \sum_i \sum_j abs(x_g,ij - b_g,ij) x d_ij,}}
#' where \mjeqn{\mathrm{abs}}{\emph{abs}} is the absolute value function and 
#' \mjeqn{d_{ij} = 0}{\emph{d_ij} = 0} when \mjeqn{f_{ij}}{\emph{f_ij}} is \code{NA};
#' otherwise, \mjeqn{d_{ij} = 1}{\emph{d_ij} = 1}. All weighted configurations in set 
#' \mjeqn{F}{\emph{F}} that have the same dimensions (\mjeqn{N_g \times N_g}{\emph{N_g x N_g}}) 
#' as \mjeqn{x_g}{\emph{x_g}} are simultaneously fit, and the function returns the \code{id} 
#' of \mjeqn{F}{\emph{F}} for the weighted configuration \mjeqn{f^\ast}{\emph{f*}} that 
#' minimizes the score.
#' 
#' @seealso \code{\link{configuration}}, \code{\link{configuration_set}}
#' @export
fit_group_network = function(x, configuration_set, all_best = FALSE,
                             parallel = FALSE, cores = NULL, ...) {
  if (is.list(x)) {
    obj = x[[1]]
  } else {
    obj = x
  }
  UseMethod("fit_group_network", obj)
}

#' @describeIn fit_group_network Group networks from adjacency matrices or edge list 
#' @param group_index
#' Vector of names or indices for groups when \code{x} is an edge list for multiple groups. Length 
#' should equal the number of rows in \code{x}. 
#' @param weights
#' Numeric vector of edge weights for an edge list. Length should equal the number of rows in 
#' \code{x}.
#' @param group_sizes
#' Vector of group sizes. Length should equal the number of groups.
#' @param type
#' Used to specify \code{x}. Can be \code{"unspecified"}, \code{"edgelist"}, or \code{"adjacency"}. 
#' If unspecified, the function will guess whether \code{x} is an edge list or adjacency matrix.
#' @examples
#' f_list = c(
#'   star(4),
#'   subgroup_all(4, between = TRUE),
#'   subgroup(4),
#'   star(4:5),
#'   subgroup_all(4:5, between = TRUE),
#'   subgroup(4:5)
#' )
#' set.seed(102)
#' x = list(
#'   matrix(rnorm(16), 4),
#'   matrix(rnorm(25), 5)
#' )
#' fit_group_network(x, f_list)
#' @export
fit_group_network.default = function(x, configuration_set, group_index, weights, group_sizes,
                                     type = c("unspecified","edgelist","adjacency"),
                                     all_best = FALSE, parallel = FALSE, cores = NULL, ...) {
  type = match.arg(type)
  if (is.array(x) && length(dim(x)) == 3) {
    x = asplit(x, 3)
  }
  if (type == "unspecified") {
    if (is.list(x)) {
      type = which_matrix_type(x[[1]])
    } else {
      type = which_matrix_type(x)
    }
  }
  
  if (type == "edgelist") {
    if (!is.list(x)) {
      if (dim(x)[2] != 2)
        stop("an edgelist must have two columns.")   
      if (missing(weights)) {
        weights = 1L
      }
      x = cbind(x, weights)
      if (!missing(group_index)) {
        x = split(x, group_index)
      } else {
        x = list(x)
      }
    }
    v_list = lapply(x, function(el) unique(c(el[, 1:2])))
  } else {
    if (!is.list(x)) {
      x = list(x)
    }
    v_list = lapply(x, function(adj) {
      if (is.null(colnames(adj))) {
        return(seq_along(diag(adj)))
      }
      return(colnames(adj))
    })
  }
  if (!missing(group_sizes) && !is.null(group_sizes)) {
    if (length(group_sizes) != length(x))
      stop("length of group_sizes does not match number of edge list groups.")
    if (any(lengths(v_list) > group_sizes))
      stop("one or more edge list groups has more vertices than their group size")    
  } else {
    group_sizes = lengths(v_list)
  }
  w_list = lapply(seq_along(x), function(i) {
    d          = x[[i]]
    group_size = group_sizes[i]
    v          = v_list[[i]]
    g_ndx      = seq_len(group_size)
    id         = ifelse(!is.na(v[g_ndx]), v[g_ndx], g_ndx)
    m          = matrix(0L, group_size, group_size, dimnames = list(id, id))
    if (type == "edgelist") {
      m[d[, 1:2]] = d[, 3]
    } else {
      m[seq_along(v), seq_along(v)] = d
    }   
  })
  names(w_list) = names(x)
  
  fit_list(w_list, configuration_set, all_best, parallel, cores, ...)
}

#' @describeIn fit_group_network Group networks from \code{igraph} objects
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_group_network.igraph = function(x, configuration_set, attrname, 
                                    all_best = FALSE, parallel = FALSE, cores = NULL, ...) {
  if (!is.list()) {
    x = list(x)
  }
  w_list = lapply(x, function(gr) {
    if (!missing(attrname) && attrname %in% igraph::edge_attr_names(gr)) {
      return(igraph::as_adjacency_matrix(gr, sparse = FALSE, attr = attrname))
    }
    return(igraph::as_adjacency_matrix(gr, sparse = FALSE))
  })

  fit_list(w_list, configuration_set, all_best, parallel, cores, ...)
}

#' @describeIn fit_group_network Group networks from \code{network} objects 
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_group_network.network = function(x, configuration_set, attrname, 
                                     all_best = FALSE, parallel = FALSE, cores = NULL, ...) {
  if (!is.list()) {
    x = list(x)
  }
  w_list = lapply(x, function(gr) {
    if (!missing(attrname) && attrname %in% network::list.edge.attributes(x)) {
      return(network::as.sociomatrix(gr, attrname = attrname))
    } 
    return(network::as.sociomatrix(gr))
  })
  
  fit_list(w_list, configuration_set, all_best, parallel, cores, ...)
}


fit_list = function(w_list, f_list, all_best, parallel, cores, ...) {

  f_list = as.configuration_set(f_list)

  if (is.null(names(w_list))) {
    digits = floor(log10(seq_along(w_list))) + 1
    names(w_list) = paste0(
      "G",
      sapply(digits, function(d, md) paste(rep(0, md - d), collapse = ""), md = max(digits)),
      seq_along(w_list)
    )
  }
  for (i in seq_along(w_list)) {
    attr(w_list[[i]], "group_name") = names(w_list)[i]
  }

  if (parallel && length(w_list) > 1 && (is.null(cores) || cores > 1)) {
    cores = min(cores, parallel::detectCores(), length(w_list))
    cat(sprintf("Parallel processing with %s cores...\n", cores))
    cl = snow::makeCluster(
      spec = cores,
      type = "SOCK"
    )
    tryCatch(
      {
        solution = pbLapply(
          x         = w_list,
          fun       = solve_fit,
          cl        = cl,
          packages  = c("ROI","ROI.plugin.glpk","Rglpk","slam","SmallGroupNetwork"),
          f_list    = f_list,
          all_best  = all_best,
          ...
        )
      },
      error   = function(e) {
        print(e)
        solution = NULL
      },
      finally = snow::stopCluster(cl)
    )
  } else{
    solution = pbLapply(
      x         = w_list,
      fun       = solve_fit,
      f_list    = f_list,
      all_best  = all_best,
      ...
    )
  }
  return(solution)
}







