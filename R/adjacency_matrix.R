adjacency_matrix <- function (x, ...) {
  UseMethod("adjacency_matrix", x)
}

#' @export
adjacency_matrix.default <- function (
  x, group_size, weights, input_type = c("unspecified","edgelist","adjacency"), ...
) {
  x          <- as.matrix(x)
  group_size <- group_size[[1]]
  input_type <- match.arg(input_type)
  if (input_type == "unspecified") {
    input_type <- which_input_type(x, group_size)
  }
  if (input_type == "adjacency" & ncol(x) != nrow(x) ||
      input_type == "edgelist" & ncol(x) != 2 ||
      input_type == "edgelist" & missing(group_size) ||
      input_type == "edgelist" & is.null(group_size))
    stop("x and group_size do not align. When x is an adjacency matrix, it must be 
         square and group_size should be omitted. When x is an edgelist, it must 
         have 2 columns and group size must be specified.")
  if (!missing(group_size) && !is.null(group_size) && input_type == "adjacency")
    warning("group_size is ignored when x is an adjacency matrix")
  if (!missing(weights) && !is.null(weights) && input_type == "adjacency")
    warning("weights are ignored when x is an adjacency matrix")
  
  if (input_type == "edgelist") {
    v <- unique(sort(x))
    if (length(v) > group_size)
      stop("The number of edge list vertices is greater than the group_size.")
    if (length(v) < group_size) {
      v <- c(v, seq_len(group_size)[-seq_len(v)])
    }
    if (missing(weights) || is.null(weights)) {
      weights <- 1L
    } else {
      if (length(weights) != nrow(x))
        stop("Length of weights must match the number of rows in x.")
    }
    m <- matrix(0L, group_size, group_size, dimnames = list(v, v))
    m[x] <- weights
  } else {
    group_size <- length(diag(x))
    if (is.null(colnames(x))) {
      v <- seq_along(diag(x))
    } else {
      v <- colnames(x)
    }
    m <- x
    dimnames(m) <- list(v, v)
  }
  return(m)
}

#' @export
adjacency_matrix.igraph <- function (x, attrname, ...) {
  if (!missing(attrname) && attrname %in% igraph::edge_attr_names(x)) {
    m <- igraph::as_adjacency_matrix(x, sparse = FALSE, attr = attrname)
  } else {
    m <- igraph::as_adjacency_matrix(x, sparse = FALSE)
  }
  return(m)
}

#' @export
adjacency_matrix.network <- function (x, attrname, ...) {
  if (!missing(attrname) && attrname %in% igraph::edge_attr_names(x)) {
    m <- network::as.sociomatrix(x, attrname = attrname)
  } else {
    m <- network::as.sociomatrix(x)
  }
  return(m)
}

which_input_type <- function (x, group_size) {
  x <- as.matrix(x)
  if (ncol(x) == 2) {
    if (ncol(x) == nrow(x)) {
      if (!missing(group_size) && !is.null(group_size)) {
        return("adjacency")
      } else {
        return("edgelist")
      }
    } else {
      return("edgelist")
    }
  } else {
    return("adjacency")
  }
}