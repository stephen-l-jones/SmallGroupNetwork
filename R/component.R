
#' Add a network component
#' 
#' \code{component} adds one or more network components to a configuration. 
#' 
#' @param component
#' A square adjacency matrix, two-column edge list or other object that can be converted 
#' to a \code{configuration} or \code{configuration_set} 
#' (see \code{\link{configuration}}).
#' @param configuration
#' A \code{configuration} or other object that can be converted to a \code{configuration}
#' @param group_size
#' The size of the resultant configuration. Ignored if a configuration is given.
#' @return
#' A \code{configuration} object.
#' @details
#' A component is the full subset of connected vertices (or nodes) in a network which is 
#' unconnected to other subsets. If a \code{configuration} is provided, components
#' are added as block diagonals to available space in the group configuration (i.e., 
#' 0-value blocks). If no \code{configuration} nor \code{group_size} are given,
#' the components are combined block-diagonally into one configuration.
#' @examples
#' f = configuration(cbind(c(1,2),c(2,1)), group_size = 8, matrix_type = "edgelist")
#' m = matrix(c(0,1,1,1,0,0,1,0,0), 3)
#' add_component(m, f)
#' m = list(m, matrix(c(0,1,0,1,0,1,0,1,0), 3))
#' add_component(m, f)
#' add_component(m)
#' @export
add_component = function(component, configuration = NULL, group_size = NULL, ...) {
  if (!is.list(component)) {
    component = list(component)
  }
  if (is.null(configuration)) {
    component = as.configuration_set(component)
    if (is.null(group_size)) {
      g = sum(lengths(lapply(component, diag)))
    } else {
      g = group_size
    }
    f = configuration(0, group_size = g, matrix_type = "adjacency", ...)
    if (!get_attribute(component[[1]], "loops")) {
      diag(f) = NA
    }
  } else {
    if (is.null(group_size)) {
      g = length(diag(configuration))
    } else {
      g = group_size
    }
    f = configuration(configuration, group_size = g, ...)
    component = as.configuration_set(component)
    g = attr(f, "group_size")
  }
  if (length(unique(get_attribute(c(component, list(f)), "type"))) > 1)
    stop("All components and the configuration must be of the same type")
  if (length(unique(get_attribute(c(component, list(f)), "loops"))) > 1)
    stop("The loops attribute must be the same for all components and the configuration")
  sf = space_finder(f)
  sapply(component, function(m) {
    if (sf$has_space(m)) {
      ndx = sf$claim_space(m)
      f[ndx, ndx] <<- m
    } else {
      warning("component was not added to the configuration; insufficent space.")
    }
    NULL
  })
  return(f)
}

#' Create a star configuration
#' 
#' \code{star} creates one or more group configurations in which \emph{n - 1} vertices are connected 
#' to the first vertex. 
#' 
#' @param n
#' Vector for the number of connected vertices.
#' @param group_size
#' Size of the group.
#' @param mode
#' Can be \code{"mutual"}, \code{"in"}, or \code{"out"}: for edge \emph{(i, j)} and matrix \emph{M}, 
#' \code{"out"} creates edge \code{M[i, j]}, \code{"in"} creates edge \code{M[j, i]}, and \code{"mutual"} creates both.
#' @param value
#' Edge value (or weight) to set.
#' @return
#' A list of group \code{configuration}s.
#' @examples
#' star(4)
#' star(2:4, 6)
#' @export
star = function(n, group_size = max(n), mode = c("mutual","in","out"), loops = FALSE, value = 1L) {
  n    = as.integer(n)
  group_size    = as.integer(group_size)
  mode = match.arg(mode)
  if (min(n) < 2) stop("n cannot be less than 2.")
  if (group_size < max(n)) stop("group_size cannot be less than n.")
  
  mlist = lapply(n, function(x) {
    m = matrix(0L, group_size, group_size)
    e = cbind(1, 2:x)
    m[switch(
      mode,
      mutual = rbind(e, e[, 2:1]),
      `in`   = e[, 2:1],
      out    = e
    )] = value
    if (!loops) {
      diag(m) = NA
    }
    make_configuration(
      m           = m, 
      description = paste(x, "star"), 
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' Create a ring configuration
#' 
#' \code{ring} creates one or more group configurations in which vertices are serially connected.
#' 
#' @param n
#' Vector for the number of connected vertices.
#' @param group_size
#' Size of the group.
#' @param mode
#' Can be \code{"mutual"}, \code{"in"}, or \code{"out"}: for edge \emph{(i, j)} and matrix \emph{M}, 
#' out creates edge \code{M[i, j]}, in creates edge \code{M[j, i]}, and mutual creates both.
#' @param value
#' Edge value (or weight) to set.
#' @return
#' A list of group \code{configuration}s.
#' @examples
#' ring(4)
#' ring(2:4, 6)
#' @export
ring = function(n, group_size = max(n), mode = c("mutual","in","out"), loops = FALSE,
                value = 1L) {
  n    = as.integer(n)
  group_size    = as.integer(group_size)
  mode = match.arg(mode)
  if (min(n) < 2) stop("n cannot be less than 2.")
  if (group_size < max(n)) stop("group_size cannot be less than n.")
  
  mlist = lapply(n, function(x) {
    m = matrix(0L, group_size, group_size)
    e = cbind(1:x, c(2:x, 1))
    m[switch(
      mode,
      mutual = rbind(e, e[, 2:1]),
      `in`   = e[, 2:1],
      out    = e
    )] = value
    if (!loops) {
      diag(m) = NA
    }
    make_configuration(
      m           = m, 
      description = paste(x, "ring"), 
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' Create a subgroup configuration
#' 
#' \code{ring} creates one or more group configurations in which vertices are connected within or
#' between subgroups.
#' 
#' @describeIn subgroup Create subgroup combinations defined in \code{...}.
#' @param ...
#' One or more vectors of subgroup sizes. If \code{relation = "between"}, at least two numbers 
#' or vectors are required.
#' @param group_size
#' Size of the group.
#' @param mode
#' Can be \code{"mutual"}, \code{"in"}, or \code{"out"}: for edge \emph{(i, j)} and matrix \emph{M}, 
#' out creates edge \code{M[i, j]}, in creates edge \code{M[j, i]}, and mutual creates both.
#' @param relation
#' Specifies whether edges are \code{"within"} or \code{"between"} subgroups. Default is
#' \code{"within"}.
#' @param value
#' Edge value (or weight) to set.
#' @return
#' A list of group \code{configuration}s.
#' @details
#' If the vector lengths in \code{...} are greater than one, then multiple configurations will be
#' created. Within a group, subgroups are sorted in decreasing size order.
#' @examples
#' subgroup(4)
#' subgroup(2, 4)
#' subgroup(2:4)
#' subgroup(2, 2:4, 3)
#' subgroup(2, 4, group_size = 8, relation = "between")
#' @export
subgroup = function(..., group_size = NULL, mode = c("mutual","in","out"), 
                    relation = c("within","between"), loops = FALSE, value = 1L) {
  mode     = match.arg(mode)
  relation = match.arg(relation)
  
  subg = expand_int_matrix(...)
  if (ncol(subg) > 1) {
    subg = t(apply(subg, 1, sort, decreasing = TRUE))
  }
  subg = unique(subg)
  if (any(subg < 1) | ncol(subg) == 0) stop("subgroup size must be greater than 0.")
  maxg = max(rowSums(subg))
  if (missing(group_size)) {
    group_size = maxg
  } else {
    group_size = as.integer(group_size)
    if (group_size < maxg) 
      stop(sprintf("given the inputs, group_size must be %s or greater.", maxg))
  }
  subg = split(subg, seq_len(nrow(subg)))
  mlist = lapply(subg, function(sg) {
    m = matrix(0L, group_size, group_size)
    x = c(0,cumsum(sg))
    x = cbind(head(x, -1), x[-1])
    x[, 1] = x[, 1] + 1
    x = lapply(split(x, seq_len(nrow(x))), function(s) s[1]:s[2])
    if (relation == "between") {
      if (length(x) < 2) return(m)
      b = t(combn(length(x), 2))
      e = do.call(
        rbind,
        lapply(split(b, seq_len(nrow(b))), function(p) expand_int_matrix(x[[p[1]]], x[[p[2]]]))
      )
      e = rbind(e, e[, 2:1])
    } else {
      e = do.call(
        rbind,
        lapply(x, function(s) expand_int_matrix(s, s))
      )
      e = e[e[, 1] != e[, 2], ]
    }
    m[switch(
      mode,
      mutual = e,
      `in`   = e[e[, 1] > e[, 2], , drop = FALSE],
      out    = e[e[, 1] < e[, 2], , drop = FALSE]
    )] = value
    if (!loops) {
      diag(m) = NA
    }
    make_configuration(
      m           = m, 
      description = paste(paste(sg, collapse = " "), relation),
      type        = ifelse(value == 1, "binary", "weighted"),
      loops       = loops,
      id          = 0
    )
  })
  set_configuration_ids(mlist)
}

#' @importFrom foreach foreach %do%
#' @describeIn subgroup Create all possible subgroup combinations.
#' @param n
#' Vector for the number of vertices in subgroups.
#' @param min_size
#' Smallest subgroup size allowed.
#' @param group_size
#' Size of the group.
#' @examples
#' 
#' subgroup_all(6)
#' subgroup_all(6, relation = "between")
#' subgroup_all(6, group_size = 8)
#' @export
subgroup_all = function(n, group_size = max(n), min_size = 2, mode = c("mutual","in","out"), 
                        relation = c("within","between"), loops = FALSE, value = 1L) {
  n          = as.integer(n)
  group_size = as.integer(group_size)
  min_size   = as.integer(min_size)
  mode       = match.arg(mode)
  relation   = match.arg(relation)
  if (min(n) < 2) stop("n cannot be less than 2.")
  if (min(n) < min_size) stop("n cannot be less than min_size.")
  if (min_size < 1) stop("min_size cannot be less than 1.")
  if (group_size < max(n)) stop("group_size cannot be less than n.")
  
  subgroup_recursive = function(v, n, min_size) {
    if (n < min_size) {
      if (n > 0) return(NULL) else return(v)
    }
    m = min(v, n)
    l = list()
    for (i in m:min_size) {
      g = subgroup_recursive(c(v, i), n - i, min_size)
      if (!is.null(g) && !is.list(g)) 
        g = list(g)
      l = c(l, g)
    }
    return(l)
  }
  
  subg = unlist(lapply(n, subgroup_recursive, v = NULL, min_size = min_size), recursive = FALSE)
  
  if (relation == "between") {
    subg = subg[lengths(subg) > 1]
    if (length(subg) == 0) 
      stop('no groups with two or more subgroups (relation = "between" requires at least 
           two subgroups).')
  }
  
  mlist = foreach(x = subg, .combine = "c") %do% {
    do.call(subgroup, c(
      as.list(x),
      list(
        group_size = group_size,
        mode       = mode,
        relation   = relation,
        loops      = loops,
        value      = value
      )
    ))
  }
  set_configuration_ids(mlist)
}

space_finder = function(f) {
  ndx = which(rowSums(abs(f), na.rm = TRUE) + colSums(abs(f), na.rm = TRUE) == 0)
  ptr = 1L
  id = c(1L, sapply(seq_along(ndx)[-1], function(i) {
    if (ndx[i] - ndx[i - 1] != 1) {
      ptr <<- ptr + 1L
    }
    ptr
  }))
  tbl = table(id)
  
  has_space = function(x) {
    n = length(diag(x))
    return(n <= max(tbl))
  }
  
  claim_space = function(x) {
    if (!has_space(x)) stop("insufficient space to add component to configuration.")
    n = length(diag(x))
    if (any(tbl == n)) {
      tbl_ndx = which(tbl == n)[1]
    } else {
      tbl_ndx = which(tbl > n)[1]
    }
    claim_ndx = ndx[id == tbl_ndx][seq_len(n)]
    ndx          <<- ndx[-(which(id == tbl_ndx)[seq_len(n)])]
    id           <<- id[-(which(id == tbl_ndx)[seq_len(n)])]
    tbl[tbl_ndx] <<- tbl[tbl_ndx] - n
    
    return(claim_ndx)
  }
  
  return(list(
    has_space   = has_space,
    claim_space = claim_space
  ))
}
