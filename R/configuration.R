
make_configuration = function(m, description, type, loops, id) {
  if (dim(m)[1] != dim(m)[2] || !("matrix" %in% class(m)))
    stop("Configuration must be a square matrix.")
  if (type == "binary") {
    if (!all(m %in% c(0:1, NA)))
      stop("Configurations of type 'binary' may only have 0, 1, or NA values.")
  }
  if (is.null(colnames(m))) {
    if (is.null(rownames(m))) {
      rownames(m) = seq_along(diag(m))
    }
    colnames(m) = rownames(m)
  }
  if (is.null(rownames(m))) {
    rownames(m) = colnames(m)
  }
  if (is.null(description)) {
    description = ""
  }
  if (is.null(id)) {
    id = 0
  }
  if (!loops) {
    diag(m) = NA
  }
  
  structure(
    m, 
    class       = c("configuration", "matrix"),
    description = description,
    type        = type,
    loops       = loops,
    group_size  = dim(m)[1],
    id          = id
  )
}

#' Configuration
#' 
#' \code{configuration} creates a group configuration.
#' 
#' @param x
#' A square adjacency matrix, two-column edge list, \code{igraph} object, or \code{network} object.
#' @return
#' A group \code{configuration}.
#' @details 
#' A configuration is represented as a binary or weighted \emph{n}-by-\emph{n} adjacency 
#' matrix for a group of size \emph{n}. Configurations within a \code{configuration_set} 
#' are fitted to a one or more group networks of the same size and type. 
#' See \code{\link{fit_group_network}} for details.
#' @seealso \code{\link{fit_group_network}}, \code{\link{configuration_set}}
#' @export
configuration = function(x, ...) {
  UseMethod("configuration", x)
}

#' @describeIn configuration Create a group \code{configuration} from an adjacency matrix or edge 
#' list.
#' @param group_size
#' Positive integer for the size of the group.
#' @param type
#' Configuration type of either \code{"binary"} or \code{"weighted"}. Binary configuration values 
#' are 0, 1, or \code{NA}. Weighted configuration values are numeric or \code{NA}.
#' @param matrix_type
#' Used to identify \code{x}. Can be \code{"unspecified"}, \code{"edgelist"}, or \code{"adjacency"}. 
#' If unspecified, the function will guess whether \code{x} is an edge list or adjacency matrix.
#' @param weights
#' Numeric vector of edge weights for an edge list. Length should equal the number of rows in 
#' \code{x}.
#' @param description
#' Description or label for the configuration.
#' @param id
#' A unique identifier for the configuration. 
#' @examples
#' adj = matrix(0, 5, 5)
#' adj[1, 2] = 1
#' adj[2, 1] = 2
#' configuration(adj, type = "weighted")
#' el = cbind(c(1,1,1),c(2,3,4))
#' configuration(el, group_size = 6)
#' @export
configuration.default = function(x, group_size, type = c("binary","weighted"), loops = FALSE,
                                 matrix_type = c("unspecified","edgelist","adjacency"), 
                                 weights, description, id,
                                 ...) {
  x           = as.matrix(x)
  type        = match.arg(type)
  matrix_type = match.arg(matrix_type)
  if (matrix_type == "unspecified") {
    if (missing(group_size)) {
      matrix_type = which_matrix_type(x)
    } else {
      matrix_type = which_matrix_type(x, group_size)
    }
  }
  if (missing(id)) {
    id = 0
  }
  if (missing(description)) {
    description = "" 
  }
  if (matrix_type == "edgelist") {
    if (dim(x)[2] != 2)
      stop("an edgelist must have two columns.")
    v = unique(sort(x))
    if (is.numeric(x)) {
      v = seq_len(max(as.integer(v)))
    }
    if (missing(weights)) {
      weights = 1L
    }
  } else {
    if (is.null(colnames(x))) {
      v = seq_along(diag(x))
    } else {
      v = colnames(x)
    }
  }
  if (missing(group_size) || is.null(group_size)) {
    group_size = length(v)
  } else {
    group_size = as.integer(group_size)
    if (group_size < length(v)) 
      stop("group_size cannot be less than the number of vertices.")
  }
  g_ndx = seq_len(group_size)
  v_id  = ifelse(!is.na(v[g_ndx]), v[g_ndx], g_ndx)
  m     = matrix(0L, group_size, group_size, dimnames = list(v_id, v_id))
  if (matrix_type == "edgelist") {
    m[x] = weights
  } else {
    m[seq_along(v), seq_along(v)] = x
  }
  
  make_configuration(m, description[[1]], type, loops, id[[1]])
}

#' @describeIn configuration Create a group \code{configuration} from an \code{igraph} object.
#' @param attrname 
#' Attribute name that holds the edge weights (usually \code{"weight"}).
#' @export
configuration.igraph = function(x, attrname, ...) {
  if (!missing(attrname) && attrname %in% igraph::edge_attr_names(x)) {
    m = igraph::as_adjacency_matrix(x, sparse = FALSE, attr = attrname)
  } else {
    m = igraph::as_adjacency_matrix(x, sparse = FALSE)
  }
  configuration.default(m, matrix_type = "adjacency", ...)
}

#' @describeIn configuration Create a group \code{configuration}  from a \code{network} object.
#' @export
configuration.network = function(x, attrname, ...) {
  type = match.arg(type)
  if (!missing(attrname) && attrname %in% network::list.edge.attributes(x)) {
    m = network::as.sociomatrix(x, attrname = attrname)
  } else {
    m = network::as.sociomatrix(x)
  }
  configuration.default(m, matrix_type = "adjacency", ...)
}

#' @export
configuration.configuration = function(x, description, type, loops, id, ...) {
  if (missing(description)) description = get_attribute(x, "description")
  if (missing(type))        type = get_attribute(x, "type")
  if (missing(loops))       loops = get_attribute(x, "loops")
  if (missing(id))          id = get_attribute(x, "id")
  configuration.default(
    x           = x,
    description = description,
    type        = type,
    loops       = loops,
    id          = id,
    matrix_type = "adjacency",
    ...
  )
}

#' @export
configuration.list = function(x, ...) {
  configuration.default(x[[1]], ...)
}

#' @rdname configuration
#' @export
as.configuration = function(x, ...) {
  UseMethod("as.configuration", x)
}

#' @export
as.configuration.configuration = function(x, ...) {
  return(x)
}

#' @export
as.configuration.default = function(x, ...) {
  configuration(x, ...)
}

#' @export
print.configuration = function(x, ...) {
  name = attr(x, "description")
  size = attr(x, "group_size")
  type = attr(x, "type")
  
  attrs = names(attributes(x))
  attrs = attrs[!(attrs %in% c("dim","dimnames"))]
  for (i in seq_along(attrs))
    attr(x, attrs[i]) = NULL
  
  cat("Configuration: ", name, "\n", sep = "")
  print.default(x, na.print = "-", ...)
}

#' @export
summary.configuration = function(x, ...) {
  edge_ndx = edge_ids(x)
  s = list(
    descr    = attr(x, "description"),
    id       = attr(x, "id"),
    size     = attr(x, "group_size"),
    type     = attr(x, "type"),
    loops    = ifelse(attr(x, "loops"), "allowed", "excluded"),
    values   = table(x[edge_ndx], useNA = "ifany")
  )
  class(s) = "configuration_summary"
  return(s)
}

#' @export
print.configuration_summary = function(x, ...) {
  cat("Configuration: ", ifelse(x$descr == "", "<no description>", x$descr), "\n", sep = "")
  cat("ID           : ", x$id, "\n", sep = "")
  cat("Group size   : ", x$size, "\n", sep = "")
  cat("Type         : ", x$type, "\n", sep = "")
  cat("Loops        : ", x$loops, "\n", sep = "")
  cat("Values table:", sep = "")
  print(x$values, ...)
}

#' @rdname configuration
#' @export
is.configuration = function(x, ...) {
  if (!is.matrix(x) || dim(x)[1] != dim(x)[2]) 
    return(FALSE)
  if (!all(c("description","type","loops","group_size","id") %in% names(attributes(x))))
    return(FALSE)
  return(TRUE)
}

