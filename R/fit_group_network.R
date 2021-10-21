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
#' @return
#' When \code{ties.method = "all"}, the function returns a list of 
#' \code{configuration_fit_set} objects, one for each group network. Otherwise it
#' returns a list of \code{configuration_fit} objects, one for each group network.
#' A \code{configuration_fit} object has the following elements:
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
#' Configurations and group networks are represented as square adjacency matrices. 
#' A configuration must have the same dimensions as a group network to be fitted. 
#' The function \code{\link{fit_group_network}} maximizes the score for binary 
#' configurations and minimizes the score for weighted configurations.
#' 
#' Fitting is done using a linear program and the \code{ROI} package. \code{\link{ROI}} allows for
#' different solvers to be used. The default solver is "glpk" using the \code{Rglpk} package. Other
#' solvers may be used by passing the solver name and any solver parameters through \code{...}
#' to the \code{\link{ROI_solve}} function (e.g., \code{solver = "lpsolve"}). When using other
#' solvers with parallel processing, also pass the solver package name and the 
#' associated \code{ROI.plugin.*} package name through \code{...} using a 
#' "packages" parameter (e.g., \code{packages = c("lpSolve","ROI.plugin.lpsolve")}).
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
#' @seealso \code{\link{configuration}}, \code{\link{configuration_set}}
#' @export
fit_group_network = function(x, configuration_set, ...) {
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
#' @param input_type
#' Used to specify \code{x}. Can be \code{"unspecified"}, \code{"edgelist"}, or \code{"adjacency"}. 
#' If unspecified, the function will guess whether \code{x} is an edge list or adjacency matrix.
#' @param ties.method
#' When \code{ties.method = "all"}, the function will return a \code{configuration_fit_set} object
#' for each group network. A \code{configuration_fit_set} is a list of one or more
#' \code{configuration_fit} objects that all have the best fit. When \code{ties.method} is
#' \code{"first"}, \code{"last"}, or \code{"random"}, the function will return the first, last,
#' or a random \code{configuration_fit} from a set.
#' @param parallel
#' Use parallel processing. Parallel processing can only be used when multiple group networks are 
#' being fit.
#' @param cores
#' Number of cores to use with parallel processing. If \code{parallel = TRUE}, and 
#' \code{cores = NULL}, then the function will detect the number of cores available.
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
                                     input_type = c("unspecified","edgelist","adjacency"),
                                     ties.method = c("all","first","last","random"),
                                     parallel = FALSE, cores = NULL, ...) {
  input_type = match.arg(input_type)
  ties.method = match.arg(ties.method)
  if (is.array(x) && length(dim(x)) == 3) {
    x = asplit(x, 3)
  }
  if (input_type == "unspecified") {
    if (is.list(x)) {
      input_type = which_input_type(x[[1]])
    } else {
      input_type = which_input_type(x)
    }
  }
  
  if (input_type == "edgelist") {
    if (!is.list(x) | is.data.frame(x)) {
      x = as.data.frame(x)
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
    v_list = lapply(x, function(el) unique(c(el[, 1], el[, 2])))
  } else {
    if (!is.list(x) | is.data.frame(x)) {
      x = list(as.matrix(x))
    } else {
      x = lapply(x, as.matrix)
    }
    v_list = lapply(x, function(adj) {
      adj = as.matrix(adj)
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
    if (input_type == "edgelist") {
      m[as.matrix(d[, 1:2])] = d[, 3]
    } else {
      m[seq_along(v), seq_along(v)] = d
    } 
    return(m)
  })
  names(w_list) = names(x)
  
  fit_list(w_list, configuration_set, ties.method, parallel, cores, ...)
}

#' @describeIn fit_group_network Group networks from \code{igraph} objects
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_group_network.igraph = function(x, configuration_set, attrname, 
                                    ties.method = c("all","first","last","random"),
                                    parallel = FALSE, cores = NULL, ...) {
  ties.method = match.arg(ties.method)
  if (!is.list()) {
    x = list(x)
  }
  w_list = lapply(x, function(gr) {
    if (!missing(attrname) && attrname %in% igraph::edge_attr_names(gr)) {
      return(igraph::as_adjacency_matrix(gr, sparse = FALSE, attr = attrname))
    }
    return(igraph::as_adjacency_matrix(gr, sparse = FALSE))
  })

  fit_list(w_list, configuration_set, ties.method, parallel, cores, ...)
}

#' @describeIn fit_group_network Group networks from \code{network} objects 
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
fit_group_network.network = function(x, configuration_set, attrname, 
                                     ties.method = c("all","first","last","random"),
                                     parallel = FALSE, cores = NULL, ...) {
  ties.method = match.arg(ties.method)
  if (!is.list()) {
    x = list(x)
  }
  w_list = lapply(x, function(gr) {
    if (!missing(attrname) && attrname %in% network::list.edge.attributes(x)) {
      return(network::as.sociomatrix(gr, attrname = attrname))
    } 
    return(network::as.sociomatrix(gr))
  })
  
  fit_list(w_list, configuration_set, ties.method, parallel, cores, ...)
}

fit_list = function(w_list, f_list, ties.method, parallel, cores, packages, ...) {

  f_list   <- as.configuration_set(f_list)
  w_list   <- set_group_attributes(w_list)
  wf_list  <- match_wf(w_list, f_list)
  # wf_match <- match_on_group_size(w_list, f_list)

  if (parallel && length(w_list) > 1 && (is.null(cores) || cores > 1)) {
    pkgs = character(0)
    if (!missing(packages)) {
      pkgs = c(pkgs, packages)
    }
    cores = min(cores, parallel::detectCores(), length(w_list))
    cat(sprintf("Parallel processing with %s cores...\n", cores))
    cl = snow::makeCluster(
      spec = cores,
      type = "SOCK"
    )
    tryCatch(
      {
        solution = pbLapply(
          x           = wf_list,
          fun         = solve_fit,
          cl          = cl,
          packages    = pkgs,
          ties.method = ties.method,
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
      x           = wf_list,
      fun         = solve_fit,
      ties.method = ties.method,
      ...
    )
  }
  return(solution)
}

set_group_attributes = function(x) {
  if (is.null(names(x))) {
    digits = floor(log10(seq_along(x))) + 1
    group_ids = paste0(
      "G",
      sapply(digits, function(d, md) paste(rep(0, md - d), collapse = ""), md = max(digits)),
      seq_along(x)
    )
  } else {
    group_ids = names(x)
  }
  x = lapply(seq_along(x), function(i) {
    attr(x[[i]], "class")    = c("group_network","matrix")
    attr(x[[i]], "group_id") = group_ids[i]
    return(x[[i]])
  })
  names(x) = group_ids
  return(x)
}

match_on_group_size <- function(w_list, f_list) {
  f_size   <- get_attribute(f_list, "group_size")
  w_size   <- lapply(w_list, nrow)
  on_match <- kronecker(f_size, w_size, "-") == 0
  wf_match <- which(matrix(on_match, length(w_size)), arr.ind = TRUE, useNames = FALSE)
  colnames(wf_match) <- c("w", "f")
  return(wf_match)
}

match_wf = function(w_list, f_list) {
  f_size = get_attribute(f_list, "group_size")
  lapply(w_list, function(w) {
    list(
      w = w,
      f_list = f_list[f_size == nrow(w)]
    )
  })
}

# configuration_permutations <- function(f_list) {
#   pair <- lapply(f_list, configuration_pairing)
#   freq <- lapply(pair, lengths)
#   perm_obj <- lapply(freq, function(q) {
#     ipermutations(v = -seq_along(q), freq = q)
#   })
#   perm_n  <- sapply(freq, npermutations, x = NULL, k = NULL, n = NULL, v = NULL)
#   data.frame(
#     obj = perm_obj,
#     n   = perm_n
#   )
# }




