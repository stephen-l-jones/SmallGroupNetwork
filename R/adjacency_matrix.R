adjacency_matrix <- function (x, ...) {
  UseMethod("adjacency_matrix", x)
}

#' @export
adjacency_matrix.default <- function (
  x, group_size, weights, input_type = c("unspecified","edgelist","adjacency"), ...
) {
  input_type <- match.arg(input_type)
  if (missing(group_size)) {
    group_size <- NULL
  } else {
    group_size <- group_size[[1]]
  }
  if (missing(x) || is.null(x)) {
    if (is.null(group_size))
      stop("x or group_size must be provided.")
    x <- matrix(0L, group_size, group_size)
    intput_type <- "adjacency"
  } else {
    x <- as.matrix(x)
  }
  if (input_type == "unspecified") {
    input_type <- which_input_type(x, group_size)
  }
  if (input_type == "adjacency" & ncol(x) != nrow(x) ||
      input_type == "edgelist" & ncol(x) != 2)
    stop("x and input_type do not align. When x is an adjacency matrix, it must be 
         square. When x is an edgelist, it must have 2 columns and group size must 
         be specified.")
  if (!missing(weights) && !is.null(weights) && input_type == "adjacency")
    warning("weights are ignored when x is an adjacency matrix")
  
  if (input_type == "edgelist") {
    v <- unique(sort(x))
    if (is.null(group_size)) {
      group_size <- length(v)
    }
    if (length(v) < group_size) {
      v <- c(v, seq_len(group_size)[-seq_along(v)])
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
    group_size <- max(length(diag(x)), group_size)
    if (is.null(colnames(x))) {
      v <- seq_len(group_size)
    } else {
      v <- colnames(x)
      if (length(v) < group_size) {
        v <- c(v, seq_len(group_size)[-seq_along(v)])
      }
    }
    m <- matrix(0L, group_size, group_size, dimnames = list(v, v))
    m[seq_len(nrow(x)), seq_len(ncol(x))] <- x
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
  if (is.null(colnames(m))) {
    v <- seq_along(diag(m))
  } else {
    v <- colnames(m)
  }
  dimnames(m) <- list(v, v)
  return(m)
}

#' @export
adjacency_matrix.network <- function (x, attrname, ...) {
  if (!missing(attrname) && attrname %in% network::list.edge.attributes(x)) {
    m <- network::as.sociomatrix(x, attrname = attrname)
  } else {
    m <- network::as.sociomatrix(x)
  }
  if (is.null(colnames(m))) {
    v <- seq_along(diag(m))
  } else {
    v <- colnames(m)
  }
  dimnames(m) <- list(v, v)
  return(m)
}

which_input_type <- function (x, group_size) {
  x <- as.matrix(x)
  if (ncol(x) == 2) {
    if (ncol(x) == nrow(x)) {
      if (is.null(group_size)) {
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