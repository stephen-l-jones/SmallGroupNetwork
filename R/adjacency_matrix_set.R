#' @export
adjacency_matrix_set <- function (x, ...) {
  UseMethod("adjacency_matrix_set", x)
}

#' @export
adjacency_matrix_set.default <- function (x, ...) {
  x <- list(as.matrix(x))
  adjacency_matrix_set.list(x, ...)
}

#' @export
adjacency_matrix_set.list <- function (x, group_size, weights, ...) {
  if (!missing(group_size) && !is.null(group_size)) {
    if (!("list" %in% class(group_size))) {
      group_size <- list(group_size)
    }
    if (length(x) != length(group_size))
      stop("List length of group_size must match list length of x.")
  } else {
    group_size <- NULL
  }
  if (!missing(weights) && !is.null(weights)) {
    if (!("list" %in% class(weights))) {
      weights <- as.list(weights)
    }
    if (length(x) != length(weights))
      stop("List length of weights must match list length of x.")
  } else {
    weights <- NULL
  }
  
  m_list <- list()
  for (i in seq_along(x)) {
    m_list[[i]] <- adjacency_matrix(
      x          = x[[i]],
      group_size = group_size[[i]],
      weights    = weights[[i]],
      ...
    ) 
  }
  return(m_list)
}



#' @export
adjacency_matrix_set_from_edgelist <- function (
  el, group_index, group_size, weights, ...
) {
  el <- as.matrix(el)
  if (ncol(el) != 2)
    stop("el must be a 2-column matrix.")
  if (nrow(el) != length(group_index))
    stop("Length of group_index must match the number of rows in el.")
  if (nrow(el) != length(group_size))
    stop("Length of group_size must match the number of rows in el.")
  if (!missing(weights) && !is.null(weights) && nrow(el) != length(weights))
    stop("Length of weights must match the number of rows in el.")
  
  if (missing(weights) || is.null(weights)) {
    weights <- rep(1L, nrow(el))
  }
  x          <- split(el, group_index)
  group_size <- lapply(split(group_size, group_index), head, 1)
  weights    <- split(weights, group_index)
  
  adjacency_matrix_set.list(
    x,
    group_size = group_size,
    weights    = weights,
    input_type = "edgelist",
    ...
  )
}